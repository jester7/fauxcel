(ns fauxcel.base.utility
  (:require
   [reagent.ratom]
   [fauxcel.base.state :as state :refer [cells-map current-selection current-formula edit-mode]]
   [fauxcel.util.dom :as dom :refer [querySelector]]
   [fauxcel.base.constants :as c]))

(def ^:const cells-parent-selector ".cellgrid.wrapper")
(def ^:const max-cols 27)
(def ^:const max-rows 101)

(defn num-to-char [num]
  (char (+ num 64)))

(defn is-formula? [str]
  (= (get str 0) "="))

(defn cell-ref
  ([cell] (cell-ref (:row cell) (:col cell))) ; if single arg, assumes map with :row and :col
  ([row col]
   (str (num-to-char col) row)))

(defn el-by-cell-ref [cell-ref]
  (querySelector (str "#" cell-ref)))

(defn changed! [^js/HTMLElement cell-el]
  (set! (.-changed (.-dataset cell-el)) true))

(defn not-changed! [^js/HTMLElement cell-el]
  (set! (.-changed (.-dataset cell-el)) false))

(defn changed? [^js/HTMLElement cell-el]
  (.-changed (.-dataset cell-el)))



(defn has-formula? [cell-ref]
  (let [data (@cells-map cell-ref)
        val (-> (el-by-cell-ref cell-ref) .-value)]
    (or (not (nil? (:formula data)))
        (is-formula? val))))

(defn scroll-to-cell ; TODO needs work, check offsets of all parent els, scrolling not quite right
  ([cell-ref] (scroll-to-cell cell-ref false true)) ; default just scroll, no range check, smooth yes
  ([cell-ref check-if-out-of-range?] (scroll-to-cell cell-ref check-if-out-of-range? true))
  ([cell-ref check-if-out-of-range? smooth-scroll?]
   (let [parent-el (querySelector cells-parent-selector)
         child-el (el-by-cell-ref cell-ref)
         child-offset-l (-> child-el .-offsetLeft)
         child-offset-t (-> child-el .-offsetTop)
         parent-offset-l (-> parent-el .-offsetLeft)
         parent-offset-t (-> parent-el .-offsetTop)
         smoothness (if smooth-scroll? "smooth" "auto")
         scroll-to-info {:left (- child-offset-l parent-offset-l)
                         :top (- child-offset-t parent-offset-t)
                         :behavior smoothness}]
     (if check-if-out-of-range?
       (when (or
              (> (- child-offset-l parent-offset-l) (.-clientWidth parent-el))
              (> (- child-offset-t parent-offset-t) (.-clientHeight parent-el)))
         (.scrollTo parent-el (clj->js scroll-to-info)))
       (.scrollTo parent-el (clj->js scroll-to-info))))))

(defn selection-cell-ref []
  (querySelector (str cells-parent-selector " input.selected")))

(defn row-col-for-el [^js/HTMLElement el]
  {:row (js/parseInt (-> el .-dataset .-row))
   :col (js/parseInt (-> el .-dataset .-col))})

(defn row-col-for-cell-ref [cell-ref]
  (let [matches (re-matches c/cell-ref-re cell-ref)]
    {:row (js/parseInt (matches 2)) :col (matches 1)}))

(defn col-label [col-num]
  [:span.col-label {:key (str "col-label-" (num-to-char col-num))} (num-to-char col-num)])

(defn cell-ref-for-input [^js/HTMLElement input-el]
  (cell-ref (js/parseInt (-> input-el .-dataset .-row)) (js/parseInt (-> input-el .-dataset .-col))))

(defn cell-data-for
  ([cell-ref] (@cells-map cell-ref))
  ([row col] (@cells-map (cell-ref row col))))

(defn cell-value-for
  ([cell-ref] (:value (@cells-map cell-ref)))
  ([row col] (:value (@cells-map (cell-ref row col)))))


;; (defn cell-data-for
;;   ([cell-ref] (r/cursor cells-map [cell-ref]))
;;   ([row col] (r/cursor cells-map [(cell-ref row col)])))

;; (defn cell-value-for
;;   ([cell-ref] (r/cursor cells-map [cell-ref :value]))
;;   ([row col] (r/cursor cells-map [(cell-ref row col) :value])))


(defn derefable? [val]
  (or (instance? cljs.core/Atom val)
      (instance? reagent.ratom/RAtom val)
      (instance? reagent.ratom/RCursor val)
      (instance? reagent.ratom/Reaction val)))

(defn recursive-deref [atom]
  (if (derefable? atom)
    (recursive-deref @atom) ; deref until no longer derefable
    atom))

(defn deref-or-val [val]
  (if (derefable? val) @val val))

(defn update-selection!
  ([el] (update-selection! el false))
  ([el get-formula?] 
   (when (not= @current-selection "") 
     (dom/remove-class (querySelector (str "#" @current-selection)) "selected")) 
   (dom/add-class-name el "selected")
   (reset! current-selection (cell-ref-for-input el))
   (.focus el)
   (let [rc (row-col-for-el el)
         data (cell-data-for (:row rc) (:col rc))
         formula (:formula data) ;(or (:formula data) (:value data))
         value (:value data)]
     (if formula
       (reset! current-formula formula)
       (reset! current-formula value))
     (when get-formula?
       (set! (-> el .-value) (if (nil? formula) value formula))))))

(defn keyboard-navigation [] ; TODO getting too long, split into more functions
  {:on-key-down
   (fn [e]
     (let [curr-cell (selection-cell-ref)]
       (case (.-key e)
         "ArrowUp" (doall (let [rc (row-col-for-el curr-cell)
                                rc-new {:row (dec (:row rc)) :col (:col rc)}]
                            (when (and (not @edit-mode) (> (:row rc) 1))
                              (set! (.-readOnly curr-cell) true)
                              (scroll-to-cell (cell-ref rc-new) true)
                              (update-selection! (querySelector (str "#" (cell-ref (:row rc-new) (:col rc-new))))))))
         "ArrowDown" (doall (let [rc (row-col-for-el curr-cell)
                                  rc-new {:row (inc (:row rc)) :col (:col rc)}]
                              (when (and (not @edit-mode) (< (:row rc) (dec max-rows)))
                                (set! (.-readOnly curr-cell) true)
                                (scroll-to-cell (cell-ref rc-new) true)
                                (update-selection! (querySelector (str "#" (cell-ref (:row rc-new) (:col rc-new))))))))
         "ArrowLeft" (doall (let [rc (row-col-for-el curr-cell)
                                  rc-new {:row (:row rc) :col (dec (:col rc))}]
                              (when (and (not @edit-mode) (> (:col rc) 1))
                                (set! (.-readOnly curr-cell) true)
                                (scroll-to-cell (cell-ref rc-new) true)
                                (update-selection! (querySelector (str "#" (cell-ref (:row rc-new) (:col rc-new))))))))
         "ArrowRight" (doall (let [rc (row-col-for-el curr-cell)
                                   rc-new {:row (:row rc) :col (inc (:col rc))}]
                               (when (and (not @edit-mode) (< (:col rc) (dec max-cols)))
                                 (set! (.-readOnly curr-cell) true)
                                 (scroll-to-cell (cell-ref rc-new) true)
                                 (update-selection! (querySelector (str "#" (cell-ref (:row rc-new) (:col rc-new))))))))
         "Tab" (doall (let [rc (row-col-for-el curr-cell)
                            rc-new {:row (:row rc) :col (inc (:col rc))}]
                        (reset! edit-mode false)
                        (set! (.-readOnly curr-cell) true)
                        (when (< (:col rc) (dec max-cols))
                          (scroll-to-cell (cell-ref rc-new) true)
                          (update-selection! (querySelector (str "#" (cell-ref (:row rc-new) (:col rc-new))))))))
         "Enter" (let [rc (row-col-for-el curr-cell)]
                   (if (not @edit-mode)
                     (do ; edit mode is false, so enter edit mode
                       (reset! edit-mode true)
                       (set! (.-readOnly curr-cell) false)
                       (update-selection! (querySelector (str "#" (cell-ref (:row rc) (:col rc)))) true)
                       (.focus curr-cell))
                     (do ; edit mode is true, so exit edit mode and move down a row if not at max already
                       (reset! edit-mode false)
                       (set! (.-readOnly curr-cell) true)
                       (update-selection! (querySelector (str "#" (cell-ref (if (< (:row rc) (dec max-rows))
                                                                              (inc (:row rc))
                                                                              (:row rc))
                                                                            (:col rc))))))))
         "Escape" (when @edit-mode
                    (let [rc (row-col-for-el curr-cell)
                          cell-r (cell-ref rc)
                          cell-map ; ignore changes and get original value
                          {:formula (:formula (@cells-map cell-r))
                           :format (:format (@cells-map cell-r))
                           :value (:value (@cells-map cell-r))}]
                      (println "Escape pressed; cell-map: " cell-map)
                      (reset! edit-mode false)
                      (set! (.-readOnly curr-cell) true)
                      (swap! cells-map
                             assoc cell-r cell-map) ; reset cell to original value
                      (set! (-> e .-target .-value) (recursive-deref (:value cell-map)))))

         (when (not @edit-mode) ; default case, any other key pressed enter edit mode if not already
           (set! (-> e .-target .-value) "")
           (set! (-> e .-target .-readOnly) false)
           (reset! edit-mode true)
           ;(update-selection! (.-target e))
           ))))})

(defn get-cell-row [^js/HTMLElement cell-el]
  (js/parseInt (-> cell-el .-dataset .-row)))

(defn get-cell-col [^js/HTMLElement cell-el]
  (js/parseInt (-> cell-el .-dataset .-col)))

(defn handle-cell-blur
  ;([^js/HTMLElement cell-el] (handle-cell-blur cell-el parser/parse-formula))
  [^js/HTMLElement cell-el parser]
  (when (changed? cell-el)
    (reset! edit-mode false)
    (set! (-> cell-el .-readOnly) true) ; set back to readonly
    (let [element-val (-> cell-el .-value)
          ;; if empty, set to nil because math functions count nil as 0 but fail on empty string ""
          val (if (= element-val "") nil element-val)
          row (get-cell-row cell-el)
          col (get-cell-col cell-el)
          cell-r (cell-ref row col)
          c-map {:formula (if (is-formula? val) val (:formula (@cells-map cell-r)))
                 :format ""
                 :value (if (is-formula? val) (parser val) val)}] ; invoke parser if formula, else just set value
      (set! (-> cell-el .-value) (deref-or-val (:value c-map)))
      (swap! cells-map
             assoc (cell-ref row col) c-map))
    (not-changed! cell-el)))