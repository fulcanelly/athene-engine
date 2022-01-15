(import logging)
(import pathlib [Path])
(import typing [Optional List Union cast])

(import .type-helpers [to])

(require .macros *)

(setv
  TIMEFMT "%d.%m.%Y %H:%M:%S"
  LOGFMT "%(asctime)s::%(levelname)s::%(name)s -> %(message)s"
  LogLevelT (of Union int str))

(defclass LogLevel []
  (defn __init__ [self ^LogLevelT value]
    (setv self.value value))

  #@(property
     (defn ^LogLevelT value [self]
       (. self _value)))

  #@((. value setter)
     (defn ^None value [self ^LogLevelT value]
       (setv self._value (to LogLevelT value)))))

(defn ^(. logging Logger) get-logger
    [^str                 name
     ^LogLevel            [level (LogLevel (. logging INFO))]
     ^bool                [stderr True]
     ^(of Optional Path)  [file None]]

  (doto (.getLogger logging name)
    (.setLevel (. level value))
    ((fn ^None [^(. logging Logger) logger]
      (for [handler
            (doto-when
              (cast
                (of List (. logging Handler))
                (list))

              stderr
              (.append (.StreamHandler logging))

              (is-not None file)
              (.append (. logging (FileHandler file))))]

        (.addHandler logger
          (doto handler
            (.setLevel (. level value))
            (.setFormatter (. logging (Formatter LOGFMT TIMEFMT))))))))))
