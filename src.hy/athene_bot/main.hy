(import asyncio)
(import logging)
(import operator [not-])
(import functools [partial])
(import datetime [datetime])
(import dataclasses [dataclass])
(import typing [Any Optional List Dict cast])

(import telethon.errors   [RPCError])
(import telethon.utils    [parse-username])
(import telethon          [TelegramClient functions])
(import telethon.tl.types [Channel :as TlChannel ChatInviteAlready ChatInvite])

(import sqlalchemy             [select])
(import sqlalchemy.ext.asyncio [AsyncSession create-async-engine])

(import .args   [Args])
(import .config [Config])
(import .log    [get-logger get-handlers])
(import .sql    [Base Channel Post Subs Views])
(import .utils  [no-nl aenumerate compose strip comment?])
(import .server [Server ServerHandler ServerHandlerResponse ResponseStatus])

(require .macros *)

#@(dataclass
    (defclass Channel? []
      (^bool valid)
      (^(of Optional str) private)
      "hash of private channel or None"
      (^(of Optional TlChannel) channel)))

(defclass Bot []
  (setv
    API-ID 1082434
    API-HASH "78315dcd0760c41f748d54f837497508")

  (defn __init__ [self ^Config config]
    (import urllib.parse [quote :as q*])
    (setv self.config config)
    (. self config (check-paths))

    (when (. self config full-debug)
      (.basicConfig
        logging
        :force True
        :level (. logging DEBUG)
        :handlers
          (get-handlers
            :stderr (not (. self config log-no-stderr))
            :file (. self config log-file)))
      (setv
        self.config.log-level.value (. logging DEBUG)
        self.config.log-file        None
        self.config.log-no-stderr   True))

    (setv
      self.bot (TelegramClient
                  (str (. self config session))
                  (. self API-ID)
                  (. self API-HASH))
     self.sql (create-async-engine  ;; TODO: optionally use postgres
                f"sqlite+aiosqlite:///{(q* (str (. self config db)))}")
     self.log (get-logger
                __name__
                :file (. self config log-file)
                :level (. self config log-level)
                :stderr (not (. self config log-no-stderr)))
     self.server (Server (. self config))
     self.loop-task (cast (of Optional (of (. asyncio Task) None)) None)
     self.tasks (cast (of Dict int (of (. asyncio Task) None)) (dict))))

  #@(staticmethod
      (defn ^str +>channel [^str channel]
        (.replace
          channel
          "/+" "/joinchat/" 1)))


  (defn/a ^None collect-channel [self ^TlChannel channel ^str from*]
    (. self log (info "collecting posts from %r" from*))
    (let [channel (. (await
                       (.bot
                         self
                         (. functions channels
                            (GetFullChannelRequest channel))))
                     full-chat)
          now (int (. datetime (utcnow) (timestamp)))]
      (with/a [session (AsyncSession (. self sql)) _ (.begin session)]
        ;; SESSION: add channel id and get posts ids
        (await (. session (merge (Channel :id (. channel id)))))
        (.add
          session
          (Subs
            :timestamp now
            :channel-id (. channel id)
            :subs (. channel participants-count)))

        (setv
          ^(of List int)
          sql-posts
          (list
            (map
              (fn ^int [^Post p]
                (let [id (. p id)]
                  (assert (not? id None))
                  id))
              (map
               (compose next iter)
               (await
                 (.execute
                   session
                   (. (select Post)
                      (filter (=
                               (. Post channel-id)
                               (. channel id)))
                      (order-by (. Post id (desc)))
                      (limit (. self config n-posts))))))))))

      (setv
        tg-posts
        (await
          (.collect
           (. self bot
              (iter-messages
                channel
                :limit (. self config n-posts))))))

      (with/a [session (AsyncSession (. self sql)) _ (.begin session)]
        ;; SESSION: write info about posts
        (for [post tg-posts]
          (when (not-in (. post id) sql-posts)
            (.add
              session
              (Post
                :id (. post id)
                :channel-id (. channel id)
                :timestamp (int (. post date (timestamp))))))

          (when (not? None (. post views))
            (.add
              session
              (Views
                :timestamp now
                :post-id (. post id)
                :views (. post views)
                :channel-id (. channel id)))))))

    (. self log (debug "collecting from %r done" from*))
    None)

  (defn/a ^(of Optional TlChannel) join [self ^str hash]
    (try
      (let [update (await
                     (.bot
                       self
                       (. functions messages (ImportChatInviteRequest hash))))]
        (next (iter (. update chats))))

      (except [RPCError])))

  (defn/a ^Channel? *valid-chan? [self ^str channel]
    (let [channel (. self (+>channel channel))
          invalid (Channel?
                    :valid False
                    :private None
                    :channel None)]
      (try
        (let [(, hash invite?) (parse-username channel)]
          (if invite?
            (let [invite? (await (.bot self (. functions messages
                                              (CheckChatInviteRequest hash))))
                  private (Channel?
                            :valid True
                            :private hash
                            :channel None)]
              (cond
                [(isinstance invite? ChatInviteAlready)
                 (doto private
                   (setattr "channel" (. invite? chat)))]
                [(isinstance invite? ChatInvite)
                 private]
                [True
                 invalid]))

            (let [chan (await (. self bot (get-entity channel)))]
              (if (isinstance chan TlChannel)
                (Channel?
                  :valid True
                  :private None
                  :channel chan)
                invalid))))
        (except [ValueError] invalid))))

  (defn/a ^None run-bot [self]
    (await (. self bot (start)))
    (. self log (info "%s started" (. self bot __class__ __name__)))

    (with/a [conn (. self sql (begin))]
      (await (. conn (run-sync (. Base metadata create-all)))))

    (.*run-loop self))

  (defn ^None *run-loop [self ^bool [restart False]]
    (when restart
      (assert (. self loop-task))
      (. self loop-task (cancel)))

    (setv
      self.loop-task
      (.create-task
        asyncio
        (.run-loop self)
        :name "bot-mainloop")))

  (defn/a ^None run-loop [self]
    (loop
      (with [fptr (. self config channels (open))]
        (for [channel ((compose (partial filter bool)
                                (partial filter (compose not- comment?)))
                       (map (compose strip no-nl) fptr))]
          (let [chan (await (. self (*valid-chan? channel)))]
            (when (not (. chan valid))
              (. self log (warning "%r is invalid channel" channel))
              (continue))

            (when (and (. chan private) (not (. chan channel)))
              (let [channel* (await (. self (join (. chan private))))]
                (unless channel*
                  (. self log (warning "failed to join to %r" channel))
                  (continue))
                (. self log (debug "succesfully joined to %r" channel))
                (setv chan.channel channel*)))

            (assert (. chan channel))

            (let [id (. chan channel id)
                  task? (. self tasks (get id))]
              (when (and task? (not (.done task?)))
                (. self log (info "task for %r still running" channel))
                (continue))

              (. self tasks
                 (update
                   {id
                    (.create-task
                      asyncio
                      (. self
                         (collect-channel (. chan channel) channel))
                      :name channel)}))))))

      (. self log (info "waiting %.2fs" (. self config interval)))
      (await (. asyncio (sleep (. self config interval))))))

  (defn/a ^ServerHandlerResponse bot-connected?
          [self ^ServerHandler f ^Any #* args ^Any #** kw]
    (if (. self bot (connected?))
      (await (f #* args #** kw))
      (ServerHandlerResponse
        :response "bot is not running"
        :status (. ResponseStatus err))))

  (defn/a ^ServerHandlerResponse reload-channels-list [self ^str _]
    (.*run-loop
      self
      :restart True)
    ;; TODO: maybe response with done and running tasks?
    (ServerHandlerResponse
      :response "restarting"
      :status (. ResponseStatus ok)))


  (defn/a ^ServerHandlerResponse valid-channel? [self ^str channel]
    (let [channel (no-nl channel)]
      (. self log (debug "checking channel %s" channel))
      (ServerHandlerResponse
        :response (if (. (await (. self (*valid-chan? channel))) valid)
                    "y"
                    "n")
        :status (. ResponseStatus ok))))

  (defn/a ^None run-server [self]
    (defmacro server-handler [h]
      `(do #* (, ~(str h) (partial (. self bot-connected?) (. self ~h)))))

    (await
      (.run
        (doto (. self server)
          (.add-handler (server-handler reload-channels-list))
          (.add-handler (server-handler valid-channel?))))))

  (defn ^None run [self]
    (.run-forever
      (doto (.get-event-loop asyncio)
        (.create-task (.run-bot    self))
        (.create-task (.run-server self))))))

(defn ^None main []
  (try
    (.run
      (Bot
        (.parse-args
          (Args Config))))
    None
    (except [KeyboardInterrupt])))

