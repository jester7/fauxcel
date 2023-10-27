(ns fauxcel.util.debug)

(defmacro logging-enabled? []
  `(not= :off log-level))

(defmacro debug-log
  "Prints a message to the console if log-level is :basic or :detailed ."
  [msg & args]
  `(when (logging-enabled?)
    (println ~msg ~@args)))

(defmacro debug-log-detailed 
  "Prints a message to the console only if log-level is :detailed ."
  [msg & args]
  `(when (and (logging-enabled?) (= :detailed log-level))
    (println ~msg ~@args)))

(defmacro do-with-timer
  "Prints the time it takes to execute the body if log-level is not :off .
   Body is always executed and its result is returned regardless of log-level."
  [msg & body]
  `(let [start# (when (logging-enabled?) (.getTime (js/Date.)))
         result# (do ~@body)]
     (debug-log "---> do-with-timer (" ~msg "): " (- (.getTime (js/Date.)) start#) "ms")
     result#))