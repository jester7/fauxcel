(ns fauxcel.util.dom
  (:require
   [clojure.string :as s]
   [fauxcel.util.collections :as c]))

;;; Various DOM utility functions

(defn query-selector
  "Query the DOM for a single element"
  ^js/HTMLElement [^string selector]
  (js/document.querySelector selector))

(defn query-selector-all
  "Query the DOM for all elements matching a selector and return them as a seq"
  [^string selector]
  (array-seq (js/document.querySelectorAll selector)))

(defn el-by-id
  "Get an element by its id"
  ^js/HTMLElement [^string el-id]
  (js/document.getElementById el-id))

(defn style-by-id
  "Get the style object of an element by its id"
  [^string el-id]
  (.-style (el-by-id el-id)))

(defn contains-class?
  "Check if an element has a class name"
   ^boolean [^js/HTMLElement el ^string class-name]
  (let [cur-class-name (or (.getAttribute el "class") "")] ; set to empty string in case js returns null because class not set
    (s/includes? cur-class-name class-name)))

;; todo: handle multiple class names
(defn add-class-name
  "Add a class name to an element if it doesn't already have it"
  [^js/HTMLElement el ^string class-name]
  (let [cur-class-name (or (.getAttribute el "class") "")] ; set to empty string in case js returns null because class not set
    (when-not (contains-class? el class-name)
      (.setAttribute el "class" (str cur-class-name " " class-name)))))

(defn swap-class
  "Swap one class name for another on an element if the from-class is present"
  [^js/HTMLElement el ^string from-class ^string to-class]
  (when el
    (let [cur-class-name (.getAttribute el "class")
          classes (s/split cur-class-name #"(?i)[\s]+")] ; split class name property into vector of classes
      (when (c/vector-contains? classes from-class)
        (.setAttribute el "class"
               ; remove the class to be swapped then add the new class
               ; join classes vector with spaces again and set the className property on the element
                       (s/join " " (concat (c/remove-val-from-vector classes from-class) [to-class])))))))

(defn remove-class
  "Remove a class name from an element if it has it"
  [^js/HTMLElement el ^string class]
  (swap-class el class " "))

(defn toggle-class
  "Toggle a class name on an element"
  [^js/HTMLElement el ^string class]
  (if (contains-class? el class)
    (swap-class el class " ")
    (add-class-name el class)))

;; Todo: toggle enabled and disabled classes
(defn set-enabled-class [^js/HTMLElement el]
  (add-class-name el "enabled"))

(defn input-disable
  "Disable an input element and optionally  add class name \"disabled\" to it"
  ([^js/HTMLElement el] (set! (.-disabled el) true))
  ([^js/HTMLElement el ^boolean set-class?]
   (input-disable el)
   (when set-class? (add-class-name el "disabled"))))

(defn input-enable
  "Enable an input element and optionally remove class name \"disabled\" from it"
  ([^js/HTMLElement el] (set! (.-disabled el) false))
  ([^js/HTMLElement el ^boolean set-class?]
   (input-enable el)
   (when set-class? (remove-class el "disabled"))))

(defn touch-device?
  "Uses browser feature detection to determine if the device is a touch device"
  ^boolean []
  (or (.hasOwnProperty js/window "ontouchstart")
      (.hasOwnProperty (.-navigator js/window) "msMaxTouchPoints")))

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

(defn draggable
  "Make an element draggable with the mouse"
  [^js/HTMLElement el ^js/HTMLElement container-el]
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

