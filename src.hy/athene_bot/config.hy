(import logging)
(import pathlib [Path])
(import functools [partial])
(import typing [Annotated Optional])
(import dataclasses [dataclass field])

(import xdg)

(import .log [LogLevel])
(import .args [ArgOpts])
(import .utils [collect])
(import . [__appname__ :as name])


(require .macros *)

#@(dataclass
    (defclass Config []
      (setv
        ^(of Annotated int "collect <N> posts from each channel"
             (. ArgOpts no-long))
        n-posts 100

        ^(of Annotated float "delay in seconds between stat collections")
        interval (* 60 60)

        ^(of Annotated LogLevel "log level")
        log-level (. logging (getLevelName (. logging INFO)))

        ^(of Annotated (of Optional Path) "also write logs to file"
             (. ArgOpts no-short))
        log-file None

        ^(of Annotated bool "don't log to stderr"
             (. ArgOpts store-true)
             (. ArgOpts no-short))
        log-no-stderr False

        ^(of Annotated Path "database path")
        db
         (field :default_factory
                (fn [] (/ (.xdg_data_home xdg) name "database.sql")))

        ^(of Annotated Path "path to telegram session file")
        session
         (field :default_factory
                (fn [] (/ (.xdg_data_home xdg) name "bot.session")))

        ^(of Annotated Path "path to channels list")
        channels
         (field :default_factory
                (fn [] (/ (.xdg_config_home xdg) name "channels.list"))))

     (defn ^None check-paths [self]
       (collect
         (map
           (attr-fn (mkdir :parents True :exist_ok True))
           (map
             (fn ^Path [^Path x] (. x parent))
             (filter
               (fn [x] (isinstance x Path))
               (map
                 (partial getattr self)
                 (filter
                   (fn [x] (not (. x (startswith "_"))))
                   (dir self))))))))))
