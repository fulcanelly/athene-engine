(import sqlalchemy.ext.declarative [declarative-base])
(import sqlalchemy                 [Column Integer ForeignKey])

(require .macros [sqlalchemy-columns])

(setv Base (declarative-base))

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
