(ns fauxcel.base.constants)

(def ^:const max-cols 27)
(def ^:const max-rows 101)
(def ^:const cell-range-re #"([a-zA-Z]{1,2}[0-9]{0,4})[\:]([a-zA-Z]{1,2}[0-9]{0,4})")
(def ^:const cell-range-check-re #"^[a-zA-Z]{1,2}[0-9]{0,4}[\:][a-zA-Z]{1,2}[0-9]{0,4}$")
(def ^:const cell-range-start-end-re #"^([a-zA-Z]{1,2}[0-9]{0,4})[\:]([a-zA-Z]{1,2}[0-9]{0,4})$")

(def ^:const cell-ref-re #"^([a-zA-Z]{1,2})([0-9]{0,4})")

(def ^:const is-numeric-re #"^[+-]?(?:\d*[\.\,])?\d*$")

(def ^:const empty-cell {:formula "" :format "" :value ""})