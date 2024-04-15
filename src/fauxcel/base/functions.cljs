(ns fauxcel.base.functions
  (:require [fauxcel.util.math :as m]
            [fauxcel.util.dates :as dates]
            [clojure.string :as s]))

(def ^:const left-p "(")
(def ^:const right-p ")")
(def ^:const exp "^")
(def ^:const minus "-")
(def ^:const comma ",")

; Map of arithmetic operators to functions
(def ^:const operators {"^" {:fn Math/pow :precedence 3 :arity 2 :nil-equals-zero? true}
                        "*" {:fn * :precedence 2 :arity 2 :nil-equals-zero? true}
                        "/" {:fn / :precedence 2 :arity 2 :nil-equals-zero? true}
                        "+" {:fn + :precedence 1 :arity 2 :nil-equals-zero? true} ; maps to + function with arity 2
                        "-" {:fn - :precedence 1 :arity 2 :nil-equals-zero? true}})

(def ^:const multi-arity -1)

; Map of string tokens to functions
(def ^:const functions {"SUM" {:fn m/sum :precedence 1 :arity multi-arity :nil-equals-zero? true}
                        "AVG" {:fn m/average :precedence 1 :arity multi-arity :nil-equals-zero? false}
                        "ROUND" {:fn m/round :precedence 1 :arity 2 :nil-equals-zero? true}
                        "COUNTA" {:fn m/count-all :precedence 1 :arity multi-arity :nil-equals-zero? false}
                        "COUNT" {:fn m/count-numeric :precedence 1 :arity multi-arity :nil-equals-zero? false}
                        "ABS" {:fn abs :precedence 1 :arity 1 :nil-equals-zero? true}
                        "STDEV" {:fn m/standard-deviation :precedence 1 :arity multi-arity :nil-equals-zero? true}
                        "MEDIAN" {:fn m/median :precedence 1 :arity multi-arity :nil-equals-zero? true}
                        "RAND" {:fn m/random-number :precedence 1 :arity 2 :nil-equals-zero? true}
                        "TODAY" {:fn dates/today :precedence 1 :arity 1 :nil-equals-zero? true}
                        "CONCAT" {:fn str :precedence 1 :arity multi-arity :nil-equals-zero? false}})

(defn get-function
  "Returns the function associated with the token string. Returns nil if not found."
  [^string token-str]
  (if (nil? token-str)
    nil
    (functions (s/upper-case token-str))))

(defn function?
  "Returns true if the token string is a function."
  ^boolean [token-str]
  (cond
    (nil? token-str) false
    (number? token-str) false
    :else (not (nil? (get-function token-str)))))

(defn operator?
  "Returns true if the token string is an operator."
  ^boolean [^string token-str]
  (not (nil? (operators token-str))))


(defn operand?
  "Returns true if the token string is an operand."
  ^boolean [^string token-str]
  (and (not= left-p token-str) (not= comma token-str) (not (function? token-str))
       (not= right-p token-str) (nil? (operators token-str))))

(defn get-arity
  "Returns the arity of the function associated with the token string. Returns 0 if not found."
  [^string token-str]
  (cond
    (function? token-str) (:arity (get-function token-str))
    (operator? token-str) 2
    :else 0))

(defn precedence
  "Returns the precedence of the operator associated with the token string. Returns 0 if not found."
  [^string token-str]
  (or (:precedence (operators token-str)) 0))
