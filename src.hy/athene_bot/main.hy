(import asyncio)
(import operator [not-])
(import functools [partial])
(import datetime [datetime])
(import dataclasses [dataclass])
(import typing [List Union Optional cast])

(import telethon.errors   [RPCError])
(import telethon.utils    [parse-username])
(import telethon          [TelegramClient functions])
(import telethon.tl.types [Channel :as TlChannel ChatInviteAlready ChatInvite])

(import sqlalchemy             [select])
(import sqlalchemy.ext.asyncio [AsyncSession create-async-engine])

(import .args   [Args])
(import .config [Config])
(import .log    [get-logger])
(import .sql    [Base Channel Post Subs Views])
(import .utils  [no-nl aenumerate compose strip comment?])
(import .server [Server ServerHandler ServerHandlerResponse ResponseStatus])

(import . [__appname__ :as name])

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

    (setv
      self.user (TelegramClient
                  (str (. self config session))
                  (. self API-ID)
                  (. self API-HASH))
     self.sql (create-async-engine
                f"sqlite+aiosqlite:///{(q* (str (. self config db)))}")
     self.log (get-logger
                name
                :file (. self config log-file)
                :level (. self config log-level)
                :stderr (not (. self config log-no-stderr)))
     self.server (Server (. self config))))

  #@(staticmethod
      (defn ^str +>channel [^str channel]
        (.replace
          channel
          "/+" "/joinchat/" 1)))


  (defn/a ^None collect-channel [self ^TlChannel channel]
    (let [channel (. (await
                       (.user
                         self
                         (. functions channels
                            (GetFullChannelRequest channel))))
                     full-chat)
          now (int (. datetime (utcnow) (timestamp)))]
      (with/a [session (AsyncSession (. self sql)) _ (.begin session)]
        (await (. session (merge (Channel :id (. channel id)))))
        (.add
          session
          (Subs
            :timestamp now
            :channel-id (. channel id)
            :subs (. channel participants-count)))

        (let [posts (list
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
                               (limit (. self config n-posts)))))))]
          (for [:async (, i post)
                (aenumerate (. self user (iter-messages channel)))]
            (when (>= i (. self config n-posts))
              (when (not-in (. post id) posts)
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
                    :channel-id (. channel id))))

              None))))))

  (defn/a ^(of Optional TlChannel) join [self ^str hash]
    (try
      (let [update (await
                     (.user
                       self
                       (. functions messages (ImportChatInviteRequest hash))))]
        (next (iter (. update chats))))

      (except [RPCError])))

  (defn/a ^Channel? *walid-chan? [self ^str channel]
    (let [channel (. self (+>channel channel))
          invalid (Channel?
                    :valid False
                    :private None
                    :channel None)]
      (try
        (let [(, hash invite?) (parse-username channel)]
          (if invite?
            (let [invite? (await (.user self (. functions messages
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

            (let [chan (await (. self user (get-entity channel)))]
              (if (isinstance chan TlChannel)
                (Channel?
                  :valid True
                  :private None
                  :channel chan)
                invalid))))
        (except [ValueError] invalid))))


  (defn/a ^None run-bot [self]
    (await (. self user (start)))
    (. self log (info "%s started" (. self user __class__ __name__)))

    (with/a [conn (. self sql (begin))]
      (await (. conn (run-sync (. Base metadata create-all)))))

    (loop ;; TODO: handle signal to reset interval
      (with [fptr (. self config channels (open))]
        (for [channel ((compose (partial filter bool)
                                (partial filter (compose not- comment?)))
                       (map (compose strip no-nl) fptr))]
          (let [chan (await (. self (*walid-chan? channel)))]
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

            (. self log (info "collecting posts from %r" channel))
            (await (. self (collect-channel (. chan channel)))))))

      (. self log (info "done, waiting %.2fs" (. self config interval)))
      (await (. asyncio (sleep (. self config interval))))))

  (defn/a ^ServerHandlerResponse walid-channel? [self ^str channel]
    (if (. self user (connected?))
      (let [channel (no-nl channel)]
        (. self log (debug "checking channel %s" channel))
        (ServerHandlerResponse
          :response (if (. (await (. self (*walid-chan? channel))) valid)
                      "y"
                      "n")
          :status (. ResponseStatus ok)))

      (ServerHandlerResponse
        :response "bot is not running"
        :status (. ResponseStatus err))))

  (defn/a ^None run-server [self]
    (defmacro server-handler [h]
      `(do #* (, ~(str h) (. self ~h))))

    (await
      (.run
        (doto (. self server)
          (.add-handler (server-handler walid-channel?))))))

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

