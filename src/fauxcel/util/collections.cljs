(ns fauxcel.util.collections)

(defn vector-contains? [vector value]
  (if-not (some #(= value %) vector) ; some will return nil if not found, converts to false
    false true)) ; probably not necessary to use if-not just wanted to cast NIL to explicit FALSE

(defn remove-from-vector [vector index] ; TODO - handle index out of bounds (return nil?)
  (vec (concat (subvec vector 0 index) (subvec vector (inc index)))))

(defn remove-val-from-vector [vector value]
  (let [index (.indexOf vector value)] ; TODO - handle val not present / index out of bounds (return nil?)
    (remove-from-vector vector index)))

(defn positions [pred coll]
  (keep-indexed (fn [index x] (when (pred x) index)) coll))