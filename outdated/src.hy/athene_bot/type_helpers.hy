(import typing [Any TypeVar Union get_args cast])

(require .simple-macros *)

(setv T (TypeVar "T"))

(defn ^(of Union T) to [^Any typ ^(of Union T) value]

  (for [t (get_args typ)]
    (try
      (return (cast (of Union T) (t value)))
      (except [ValueError]))
    (else
      (raise (ValueError f"invalid value for {typ}: {value !r}")))))
