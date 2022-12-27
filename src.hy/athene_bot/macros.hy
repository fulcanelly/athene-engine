(import .utils [head chunks])
(require .simple-macros *)


(defmacro doto [form #* expressions]
  "Perform possibly mutating `expressions` on `form`, returning resulting obj."
  (setv f (hy.gensym))
  (defn build-form [expression]
    (if (isinstance expression hy.models.Expression)
      `(~(get expression 0) ~f ~@(head expression))
      `(~expression ~f)))
  `(do
     (setv ~f ~form)
     ~@(map build-form expressions)
     ~f))

(defmacro doto/a [form #* expressions]
  (setv f (hy.gensym))
  (defn build-form [expression]
    `(await
       ~(if (isinstance expression hy.models.Expression)
          `(~(get expression 0) ~f ~@(head expression))
          `(~expression ~f))))
  `(do
     (setv ~f ~form)
     ~@(map build-form expressions)
     ~f))

(defmacro doto-when [form #* expressions]
  (setv f (.gensym hy))

  (defn build-form [expression]
    (setv [cond* expr] expression)
    `(when ~cond*
       (~(get expr 0) ~f ~@(head expr))))

  (assert (= 0 (% (len expressions) 2)))

  `(do
     (setv ~f ~form)
     ~@(map build-form (chunks expressions))
     ~f))

(defmacro sqlalchemy-columns [column-f #* expressions]
  (defn build-form [expression]
    (setv [name expr] expression)
    `(setv ~name (~column-f ~@expr)))

  (assert (= 0 (% (len expressions) 2)))

  `(do ~@(map build-form (chunks expressions))))

(defmacro as-> [head name #* rest]
  "Beginning with `head`, expand a sequence of assignments `rest` to `name`."
  `(do (setv
         ~name ~head
         ~@(sum (gfor  x rest  [name x]) []))
     ~name))
