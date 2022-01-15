(import enum)
(import argparse)
(import functools [partial])
(import typing [Annotated Type Union get_args get_origin get_type_hints])
(import operator [is_ :as is*
                  not_ :as not*])

(import .utils [compose])
(import .type-helpers [T])

(require .macros *)

(defclass ArgOpts [(. enum Enum)]
  (auto-enum (.auto enum)
    no-long
    no-short
    store-true))

(defclass Args []
  (defn __init__ [self ^(of Type T) from-type]
    (setv
      self.type from-type
      self.parser (. argparse (ArgumentParser
                               :formatter_class
                               (. argparse ArgumentDefaultsHelpFormatter))))

    (let [default (. self (type))
          hints (get_type_hints (. self type)
                                :include_extras True)]
      (for [(, name t) (. hints (items))]
        (let [ass-msg "should have Annotated type hint"]
          (assert (is Annotated (get_origin t))
                  f"{self.type.__name__}.{name} {ass-msg}"))

        (let [(, typ #* _) (get_args t)]
          (when (and
                  (is Union (get_origin type))
                  (in (type None) (get_args typ)))
            (setv
              typ
              (next
                (filter
                  (compose
                    (fn [x] (not x))
                    (fn [x] (is None x)))
                  (get_args typ)))))

          (let [(, hlp #* metadata) (. t __metadata__)
                store-true (in (. ArgOpts store-true) metadata)
                dash  "-"
                under "_"
                args (doto-when (list)
                       (and
                         (not-in (. ArgOpts no-short) metadata)
                         (> (len name) 2))
                       (.append f"-{(next (iter name))}")

                       (or
                         (in (. ArgOpts no-short) metadata)
                         (not-in (. ArgOpts no-long) metadata))
                       (.append f"--{(. name (replace under dash))}"))

                kwargs (doto (dict
                               :dest name
                               :default (getattr default name)
                               :help
                                 (+
                                  (str hlp)
                                  (if store-true
                                    (str)
                                    f" (type: {(. self (type-name typ))})")))

                         ((fn [d]
                            (if store-true
                              (. d (update :action "store_true"))
                              (. d (update :type typ))))))]

            (. self parser (add_argument #* args #** kwargs)))))))


  #@(staticmethod
     (defn ^str type-name [^type typ]
       (if (is Union (get_origin typ))
         (str typ)
         (. typ __name__))))

  (defn ^T parse-args [self]
    (. self (type #** (vars (. self parser (parse_args)))))))
