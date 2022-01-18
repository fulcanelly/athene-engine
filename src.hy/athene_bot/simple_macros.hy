(defmacro of [base #* args]
  "Shorthand for indexing for type annotations."
  (if
    (not args) base
    (if (= (len args) 1)
        `(get ~base ~@args)
        `(get ~base (, ~@args)))))

(defmacro auto-enum [func #* expressions]
  `(do ~@(map (fn [e] `(setv ~e ~func)) expressions)))

(defmacro attr-fn [form]
  `(fn [x] (. x ~form)))

(defmacro loop [#* body]
  `(while (not False) ~@body))

(defmacro unless [test #* body]
  `(when (not ~test) (do ~@body)))

(defmacro -> [head #* args] ;; this is not simple :)
  (setv ret head)
  (for [node args]
    (setv ret (if (isinstance node hy.models.Expression)
                  `(~(get node 0) ~ret ~@(cut node 1 None))
                  `(~node ~ret))))
  ret)
