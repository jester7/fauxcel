(ns fauxcel.serverless.api.test-handler2)

(defn handler [req res]
  (.send res "... hello world! 2"))