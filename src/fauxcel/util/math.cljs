(ns fauxcel.util.math)

;; ---------------------------------------------
;; copied from my ClojureScript7 project
;; ---------------------------------------------

;; round to specified decimal place or default 0, nearest integer
(defn round
  ([num] (round num 0))
  ([num decimal-places]
   (let [power-of-ten-factor (Math/pow 10 decimal-places)]
     (/ (Math/floor (+ (* num power-of-ten-factor) 0.5)) power-of-ten-factor))))

;; test if a value is numeric
(defn numeric? [x]
  (= (js/parseFloat x) (js/parseFloat x))) ; parseFloat returns NaN, NaN can never be equal to another NaN

(defn average [& nums]
  (/ (reduce + nums) (count nums)))