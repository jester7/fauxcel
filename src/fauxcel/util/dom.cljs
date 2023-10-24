(ns fauxcel.util.dom
  (:require
   [clojure.string :as s]
   [fauxcel.util.collections :as c]))

;;; Various DOM utility functions

(defn querySelector [selector]
  (println "querySelector" selector)
  (js/document.querySelector selector))

(defn querySelectorAll [selector]
  (array-seq (js/document.querySelectorAll selector)))

(defn el-by-id [el-id]
  (js/document.getElementById el-id))

(defn style-by-id [el-id]
  (.-style (el-by-id el-id)))

(defn contains-class? [el class-name]
  (let [cur-class-name (or (.getAttribute el "class") "")] ; set to empty string in case js returns null because class not set
    (s/includes? cur-class-name class-name)))

;; todo: handle multiple class names
;; If the element doesn't have the supplied class name yet, it will add it
;; If it does already have it, does nothing
(defn add-class-name [el class-name]
  (let [cur-class-name (or (.getAttribute el "class") "")] ; set to empty string in case js returns null because class not set
    (if-not (contains-class? el class-name)
      (.setAttribute el "class" (str cur-class-name " " class-name)))))

(defn swap-class [el from-class to-class]
  (when el
    (let [cur-class-name (.getAttribute el "class")
          classes (s/split cur-class-name #"(?i)[\s]+")] ; split class name property into vector of classes
      (when (c/vector-contains? classes from-class)
        (.setAttribute el "class"
               ; remove the class to be swapped then add the new class
               ; join classes vector with spaces again and set the className property on the element
                       (s/join " " (concat (c/remove-val-from-vector classes from-class) [to-class])))))))

(defn remove-class [el class]
  (swap-class el class " "))

(defn toggle-class [el class]
  (if (contains-class? el class)
    (swap-class el class " ")
    (add-class-name el class)))

;; Todo: toggle enabled and disabled classes
(defn set-enabled-class [el]
  (add-class-name el "enabled"))

;; sets disabled property on an input element and swaps enabled for "disabled" to class names by default
;; pass false as second argument to avoid changes to css classes 
(defn input-disable
  ([el] (set! (.-disabled el) true))
  ([el set-class?]
   (input-disable el)
   (when set-class? (add-class-name el "disabled"))))

(defn input-enable
  ([el] (set! (.-disabled el) false))
  ([el set-class?]
   (input-enable el)
   (when set-class? (remove-class el "disabled"))))

(defn touch-device? []
  (or (.hasOwnProperty js/window "ontouchstart")
      (.hasOwnProperty (.-navigator js/window) "msMaxTouchPoints")))

(defn animate-then [el-id count post-animation-fn]
  ;;   (.getElementById js/document el-id)
  (let [element        (js/document.getElementById el-id)
        style       (.-style element)
        transform     (str "rotate(" (* 360 (inc count)) "deg)")]
    ;(js/console.log transform new-transform)
    (set! (.-transform style) transform))
  (post-animation-fn))

(defn canvas-init [canvas ctx]
  (reset! canvas (querySelector "#circles-canvas"))
  (set! (.-width @canvas) (.-offsetWidth @canvas))
  (set! (.-height @canvas) (.-offsetHeight @canvas))
  (reset! ctx (.getContext @canvas "2d")))

(defn ctx-menu-disable []
  (fn [e] (.preventDefault e)
    false))

(defn mouse-info [e target-el]
  (when (identical? target-el (.-target e))
    (let [client-rect (.getBoundingClientRect (.-target e))
          btn (case (.-button e)
                0 :left
                1 :middle
                2 :right
                :left)] ; assume left (should not be another value unless using old IE versions)
      {:button btn
       :x (- (.-clientX e) (.-left client-rect))
       :y (- (.-clientY e) (.-top client-rect))
       :left? (#(= btn :left))
       :right? (#(= btn :right))})))

(defn draggable [el container-el]
  (let [active? (atom false)
        initial-x (atom 0)
        initial-y (atom 0)
        current-x (atom 0)
        current-y (atom 0)
        offset-x (atom 0)
        offset-y (atom 0)
        position-set! (fn [x y]
                        (set! (-> el .-style .-left) (str x "px"))
                        (set! (-> el .-style .-top) (str y "px")))
        drag-start (fn [e]
                     (reset! initial-x (- (-> e .-clientX) @offset-x))
                     (reset! initial-y (- (-> e .-clientY) @offset-y))
                     (when (identical? el (-> e .-target))
                       (.preventDefault e)
                       (reset! active? true)))
        drag (fn [e]
               (when @active?
                 (.preventDefault e)
                 (reset! current-x (- (-> e .-clientX) @initial-x))
                 (reset! current-y (- (-> e .-clientY) @initial-y))
                 (reset! offset-x @current-x)
                 (reset! offset-y @current-y)
                 (position-set! @current-x @current-y)))
        drag-end (fn [e]
                   (when (identical? el (-> e .-target))
                     (.stopPropagation e))
                   (reset! initial-x @current-x)
                   (reset! initial-y @current-y)
                   (reset! active? false))]
    (.addEventListener container-el "mousedown" drag-start false)
    (.addEventListener container-el "mouseup" drag-end false)
    (.addEventListener container-el "mousemove" drag false)))