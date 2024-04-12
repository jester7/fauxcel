(ns fauxcel.base.tokenization
  (:require
   [clojure.string :as s]
   [fauxcel.base.functions :as fn]
   [fauxcel.base.utility :as u]
   [fauxcel.base.constants :as c]))

(def tokenize-re
  "Regular expression for tokenizing a string expression."
  (re-pattern (str "\\,|" (str (s/join "|" (keys fn/functions)))
                   "|[[a-zA-Z]{1,2}[0-9]{0,4}[\\:][a-zA-Z]{1,2}[0-9]{0,4}]*|[[0-9]?\\.?[0-9]+]*|[\\/*\\-+^\\(\\)]"
                   "|[[a-zA-Z]{1,2}[0-9]{1,4}]*"
                   "|(?<=\")[^,]*?(?=\")")))

(defn strip-whitespace
  "Strips whitespace from a string."
  ^string [^string input-str]
  (s/replace input-str #"\s(?=(?:\"[^\"]*\"|[^\"])*$)" ""))

(defn tokenize
  "Takes a string expression and returns a sequence of tokens."
  [^string expression-str]
  (let [cell-ref-re   c/cell-range-re
        expanded-refs (s/replace expression-str
                                 cell-ref-re
                                 #(str (s/join "," (u/expand-cell-range (%1 0)))))]
    (re-seq tokenize-re (s/upper-case (strip-whitespace expanded-refs)))))
