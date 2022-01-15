(import itertools [islice])

(defn rest [coll]
  "Get all the elements of `coll`, except the first."
  (islice coll 1 None))


(defmacro of [base #* args]
  "Shorthand for indexing for type annotations."
  (if
    (not args) base
    (if (= (len args) 1)
        `(get ~base ~@args)
        `(get ~base (, ~@args)))))


(defmacro doto [form #* expressions]
  "Perform possibly mutating `expressions` on `form`, returning resulting obj."
  (setv f (hy.gensym))
  (defn build-form [expression]
    (if (isinstance expression hy.models.Expression)
      `(~(get expression 0) ~f ~@(rest expression))
      `(~expression ~f)))
  `(do
     (setv ~f ~form)
     ~@(map build-form expressions)
     ~f))

(defmacro auto-enum [func #* expressions]
  `(do ~@(map (fn [e] `(setv ~e ~func)) expressions)))


(defn chunks [it [size 2]]
  (setv it (iter it))

  (iter
    (fn [] (tuple (islice it size)))
    (tuple)))

(defmacro doto-when [form #* expressions]
  (setv f (.gensym hy))

  (defn build-form [expression]
    (setv [cond* expr] expression)
    `(when ~cond*
       (~(get expr 0) ~f ~@(rest expr))))

  (assert (= 0 (% (len expressions) 2)))

  `(do
     (setv ~f ~form)
     ~@(map build-form (chunks expressions))
     ~f))

(defmacro attr-fn [form]
  `(fn [x] (. x ~form)))

(defmacro sqlalchemy-columns [column-f #* expressions]
  (defn build-form [expression]
    (setv [name expr] expression)
    `(setv ~name (~column-f ~@expr)))

  (assert (= 0 (% (len expressions) 2)))

  `(do ~@(map build-form (chunks expressions))))

(defmacro loop [#* body]
  `(while (not False) ~@body))


(defmacro as-> [head name #* rest]
  "Beginning with `head`, expand a sequence of assignments `rest` to `name`."
  `(do (setv
         ~name ~head
         ~@(sum (gfor  x rest  [name x]) []))
     ~name))

(defmacro unless [test #* body]
  `(when (not ~test) (do ~@body)))
