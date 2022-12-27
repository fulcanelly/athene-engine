(import functools [reduce])
(import itertools [islice])
(import typing [Any AsyncIterator AsyncIterable
                Iterator Iterable Sequence
                Tuple Callable TypeVar Iterable Union])

(import .type-helpers [T])

(require .simple-macros *)

(defn ^str no-nl [^str line]
  (if (. line (endswith "\n"))
    (no-nl (cut line None -1))
    line))

(defn/a ^(of AsyncIterator (of Tuple int Any)) aenumerate
    [^(of AsyncIterable Any) aiter ^int [start 0]]
  (for [:async i aiter]
    (yield (, start i))
    (setv start (+ 1 start))))

(defn ^Any compose [^(of Callable [Any] Any) #* functions]
  (setv
    T1 (TypeVar "T1")
    T2 (TypeVar "T2")
    T3 (TypeVar "T3"))

  (defn ^(of Callable [T1] T3) compose2
       [^(of Callable [T2] T3) f
        ^(of Callable [T1] T2) g]
    (fn [x] (f (g x))))

  (reduce compose2 functions (fn [x] x)))

(defn ^None collect [^(of Iterable Any) it]
  (for [_ it]))


(defn ^(of Callable [Any] bool) instanceof [^(of Union type (of Tuple type)) t]
  (fn [x] (isinstance x t)))

(defn ^(of Iterator T) head [^(of Iterable T) it]
  (islice it 1 None))

(defn ^(of Iterator (of Sequence T)) chunks [^(of Sequence T) l ^int [size 2]]
  (while l
    (let [chunk (cut l size)]
      (yield chunk))
    (setv l (cut l size None))))

(defn ^str strip [^str in*]
  (-> in*
    (.lstrip)
    (.rstrip)))

(defn ^bool comment? [^str in*]
  (. in* (startswith "#")))
