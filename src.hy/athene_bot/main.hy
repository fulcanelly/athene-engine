(import re)
(import asyncio)
(import datetime [datetime])

(import telethon.utils    [parse-username])
(import telethon.tl.types [Channel :as TlChannel])
(import telethon          [TelegramClient functions])
(import telethon.errors   [UserAlreadyParticipantError RPCError])


(import sqlalchemy.ext.declarative [declarative-base])
(import sqlalchemy.ext.asyncio     [AsyncSession create-async-engine])
(import sqlalchemy                 [Column Integer ForeignKey select])


(import .args   [Args])
(import .config [Config])
(import .log    [get-logger])
(import .utils  [no-nl aenumerate compose])

(import . [__appname__ :as name])

(require .macros *)

(setv
  PLUS-RE (. re (compile r".*/\+([^/]+)$"))
  Base (declarative-base))

(defclass Channel [Base]
  (setv __tablename__ "channel")

  (sqlalchemy-columns Column
    id (Integer :nullable False :primary_key True)))

(defclass Post [Base]
  (setv __tablename__ "post")

  (sqlalchemy-columns Column
    id         (Integer :nullable False :primary_key True)
    channel-id (Integer (ForeignKey "channel.id")
                :nullable False :primary_key True)
    timestamp  (Integer :nullable False)))

(defclass Subs [Base]
  (setv __tablename__ "subs")

  (sqlalchemy-columns Column
    subs       (Integer :nullable False)
    channel-id (Integer (ForeignKey "channel.id")
                :nullable False :primary_key True)
    timestamp  (Integer :nullable False :primary_key True)))

(defclass Views [Base]
  (setv __tablename__ "views")

  (sqlalchemy-columns Column
    views      (Integer :nullable False)
    post-id    (Integer (ForeignKey "post.id")
                :nullable False :primary_key True)
    channel-id (Integer (ForeignKey "channel.id")
                :nullable False :primary_key True)
    timestamp  (Integer :nullable False :primary_key True)))

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
                :stderr (not (. self config log-no-stderr)))))

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

              (when (is-not None (. post views))
                (.add
                  session
                  (Views
                    :timestamp now
                    :post-id (. post id)
                    :views (. post views)
                    :channel-id (. channel id))))

              None))))))

  (defn/a ^bool try-join [self ^str hash]
    (try
      (await
        (.user
          self
          (. functions messages (ImportChatInviteRequest hash))))

      (except [UserAlreadyParticipantError] True)
      (except [RPCError] False)
      (else True)))

  (defn/a ^None run [self]
    (await (. self user (start)))
    (. self log (info "%s started" (. self user __class__ __name__)))

    (with/a [conn (. self sql (begin))]
      (await (. conn (run-sync (. Base metadata create-all)))))

    (loop ;; TODO: handle signal to reset interval
      (with [fptr (. self config channels (open))]
        (for [channel (map no-nl fptr)]
          (as-> (. PLUS-RE (match channel)) match
            (when match
              (setv channel f"https://t.me/joinchat/{(. match (group 1))}")))

          (let [(, hash invite?) (parse-username channel)]
            (when invite?
              (unless (await (. self (try-join hash)))
                (. self log (warning "failed to join to %r" channel))
                (continue))))

          (let [chan (await (. self user (get-entity channel)))]
            (unless (isinstance chan TlChannel)
              (. self log (warning "%r is not a channel" channel))
              (continue))

            (. self log (info "collecting posts from %r" channel))
            (await (. self (collect-channel channel))))))

      (. self log (info "done, waiting %.2fs" (. self config interval)))
      (await (. asyncio (sleep (. self config interval)))))))

(defn ^None main []
  (try
    (. asyncio
       (get-event-loop)
       (run-until-complete
         (.run
           (Bot (.parse-args
                  (Args Config))))))
    (except [KeyboardInterrupt])))

