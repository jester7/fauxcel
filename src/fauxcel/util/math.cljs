(ns fauxcel.util.math
  (:require [fauxcel.base.constants :as c]
            [fauxcel.util.dates :as dates]))

(defn round
  "Rounds a number to the nearest integer or to the specified number of decimal places."
  ([num] (round num 0))
  ([num decimal-places]
   (let [power-of-ten-factor (Math/pow 10 decimal-places)]
     (/ (Math/floor (+ (* num power-of-ten-factor) 0.5)) power-of-ten-factor))))

;; Tests if a value is numeric.
;; Unlike the built in number? function, this will return true for numeric strings.
;; TODO - decide how to handle strings that start numeric but have non-numeric chars in them
;; which parseFloat treats as numeric even though they have trailing non-numeric chars
(defn numeric? 
  "Returns true if the value is a number or a numeric string."
  ^boolean [x]
  (= (js/parseFloat x) (js/parseFloat x))) ; parseFloat returns NaN, NaN can never be equal to another NaN

;; ---------------------------------------------
;; Returns the number value from a numeric string but it doesn't do
;; error checking so the return value could be NaN.
(defn eval-number
  ([val] (eval-number val false))
  ([val nil-or-str-equals-zero?]
    (cond
      (dates/date? val) val
      (number? val) val
      (and nil-or-str-equals-zero? (nil? val)) 0 ; coerce nil to 0
      (and nil-or-str-equals-zero? (string? val) (not (re-seq c/is-numeric-re val))) 0 ; coerce non-numeric string to 0
      ;; (nil? val) 0
      ;; (coll? val) (map eval-number val)
      :else
      (if (re-seq #"\." val) (js/parseFloat val) (js/parseInt val)))))

(defn sum
  "Returns the sum of one or more numbers.
  Works with combinations of numbers and strings with numeric values."
  [& nums]
  (reduce + (map #(eval-number %1 true) nums)))

(defn average
  "Returns the average of one or more numbers or numeric strings.
  Returns nil if no numbers are provided."
  ([] nil)
  (^number [nums]
   (if (seq? nums)
     (apply average nums)
     (if (numeric? nums) (eval-number nums) nil)))
  (^number [x & more]
   (let [nums (map eval-number (filter #(not (nil? %1)) (cons x more)))
         count (count nums)]
     (if (> count 0)
       (/ (reduce + nums) count)
       nil))))

(defn standard-deviation
  "Returns the standard deviation of a list of numbers."
  [& nums]
  (let [nums (map eval-number nums) ; convert numeric strings to numbers
        avg (average nums)] ; get the average
    (->> nums
         (map #(- % avg)) ; subtract the average from each number
         (map #(* % %)) ; square each number
         (average) ; get the average of the squared numbers
         (Math/sqrt))))

(defn median
  "Returns the median of a list of numbers."
  [& nums]
  (let [nums (map eval-number nums) ; convert numeric strings to numbers
        nums (sort nums) ; sort the numbers
        len (count nums)] ; get the length of the list
    (cond
      (even? len) ; if the length is even
      (let [mid (/ len 2)] ; get the middle index
        (average (nth nums (dec mid)) (nth nums mid))) ; average the two middle numbers
      :else ; if the length is odd
      (nth nums (/ len 2))))) ; return the middle number

;; ---------------------------------------------
;; Count the number of numeric values in a list.
;; In general the parser considers empty cells as having value 0, but here they are excluded.
(defn count-numeric [& nums]
  (count (filter #(numeric? %1) nums)))
;;   (count (filter #(and (numeric? %1) (not= %1 nil)) nums))) ; TODO not= nil might be redundant 

;; ---------------------------------------------
;; Count all non empty values
(defn count-all [& items]
  (count (filter #(not= %1 nil) items)))

;; ---------------------------------------------
;; Returns a random number between min and max
(defn random-number [min max]
  (+ min (rand-int (- max min))))
