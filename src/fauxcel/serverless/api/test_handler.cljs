(ns fauxcel.serverless.api.test-handler)

(defn handler [req res]
  (.send res "hello world!"))