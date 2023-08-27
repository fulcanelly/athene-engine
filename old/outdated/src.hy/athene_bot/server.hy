(import enum)
(import asyncio)
(import dataclasses [dataclass])
(import typing [Awaitable Callable Dict cast])
(import asyncio [StreamReader StreamWriter])

(import .config [Config])
(import .log [get-logger])

(require .macros *)

(defclass ResponseStatus [(. enum Enum)]
  (auto-enum (.auto enum)
    err
    ok))

#@(dataclass
    (defclass ServerHandlerResponse []
       (^str response)
       (^ResponseStatus status)))

(setv ServerHandler (of Callable [str] (of Awaitable ServerHandlerResponse)))


(defclass Server []
  (defn __init__ [self ^Config config]
    (setv
      self.handlers (cast (of Dict str ServerHandler) (dict))
      self.config config
      self.log (get-logger
                 __name__
                 :file (. self config log-file)
                 :level (. self config log-level)
                 :stderr (not (. self config log-no-stderr)))))

  #@(staticmethod
      (defn ^str ->response [^ServerHandlerResponse r]
        (% "%s:%s"
          (,
            (. r status name (upper))
            (. r response)))))

  (defn/a ^ServerHandlerResponse invalid-request-handler [self ^str request]
    (. self log (debug "invalid request %r" request))
    (ServerHandlerResponse
      :response "invlid request"
      :status (. ResponseStatus err)))

  (defn/a ^None server-handler [self ^StreamReader reader ^StreamWriter writer]
    (let [req (.decode (await (. reader (read (<< 1 8)))) "utf-8")]
      (. self log (debug "processing request %r" req))
      (let [handler-future
            (try
              (let [(, request handler)
                    (next
                      (iter
                        (filter
                          (fn [x] (.startswith req (str (next (iter x)))))
                          (.items (. self handlers)))))]

                (handler (cut req (len request) None)))

              (except [StopIteration] (.invalid-request-handler self req)))]

        (let [response (. self (->response (await handler-future)))]
          (. self log (debug "responsing %r" response))
          (doto writer
            (.write (.encode response "utf-8"))
            (.close))))
      None))

  (defn/a ^None run [self]
    (let [server (await (.start-server
                          asyncio
                          (. self server-handler)
                          :host (. self config host)
                          :port (. self config port)))]
      (. self log (info "listening on %s:%d"
                        (. self config host)
                        (. self config port)))
      (with/a [server]
        (await (.serve-forever server)))
      None))

  (defn ^None add-handler [self ^str req ^ServerHandler handler]
    (. (. self handlers) (update {req handler}))))
