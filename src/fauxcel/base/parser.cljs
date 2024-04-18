(ns fauxcel.base.parser
  (:require
   [fauxcel.base.functions :as fn :refer
    [operator? operand? function? get-function get-arity
     operators precedence exp minus left-p right-p comma]]
   [fauxcel.base.tokenization :as tok]
   [fauxcel.util.math :as m]
   [reagent.core :as r]
   [fauxcel.base.utility :as util :refer
    [cell-value-for cell-ref?]]
   [fauxcel.util.debug :as debug :refer [debug-log debug-log-detailed do-with-timer]]))

;; TODO - needs better error handling for invalid formulas

(defn swap-unary-minus
  "Takes a seq of tokens and returns a new vector with unary minus replaced by -1 *
   and surrounded by parentheses. This is done to make unary minus the highest
   priority operator."
  [infix-tokens]
  (loop [original-tokens (into [] infix-tokens)
         prev-token nil ; nil at start of loop
         prev-token-unary? false ; false at start of loop
         new-tokens []] ; as tokens are processed they are added to vector
    (if original-tokens
      (let [token (first original-tokens)
            replace?
            (if (= minus token)
              (cond
                (nil? prev-token) true ; if minus is at beginning, is unary minus
                (operator? prev-token) true ; if previous token is an operator, assume unary -
                (= left-p prev-token) true ; if prev token is left (, must be unary -
                :else false) ; if current token is minus but none of above conditions match, it can't be a unary operator
              false)] ; if it isn't a minus sign, don't replace
        (recur (next original-tokens) token replace?
               (if replace?
                 (conj new-tokens "(" "-1" "*") ; if above conditions set the replace flag, add -1 multiplication
                 (if prev-token-unary? ; else check unary flag for previous token
                   (conj new-tokens token ")") ; if the previous token was unary -, conj token and closing parenthesis
                   (conj new-tokens token))))) ; else just add the token and nothing extra
      new-tokens)))

(defn eval-cell-ref
  ([cell-ref] (eval-cell-ref cell-ref true))
  ([cell-ref when-not-cell-ref-return-nil?]
   (if (cell-ref? cell-ref)
     (r/track! #(let [cell-data (cell-value-for cell-ref)]
                  (if (m/numeric? cell-data) (m/eval-number cell-data) cell-data)))
     (if when-not-cell-ref-return-nil? nil cell-ref))))

(defn eval-token
  "Evaluates a single token string and returns the corresponding function,
  cell value, or number."
  [^string token]
  (cond
    ; If cell ref, evaluate and return 
    (cell-ref? token) ; moved this cond up for formulas with many cell refs, slight performance boost maybe?
    (util/recursive-deref (eval-cell-ref token))

    ; If it's an operator, return the function
    (:fn (operators token)) ; (operators token) returns nil if not found
    (:fn (operators token))

    (function? token)
    (:fn (get-function token))

    (and (string? token) (not (m/numeric? token)))
    token ; if string and not numeric, return the string

    ;; must be a number then
    :else
    (do
      (debug-log-detailed ":else eval-number for token: " token)
      (m/eval-number token))))

(defn swap-parentheses
  "Swaps parentheses in an expression. Used to reverse the expression for
  postfix evaluation."
  [expression]
  (loop [remaining expression
         new-expression []]
    (if (empty? remaining)
      (into () new-expression)
      (let [token (first remaining)
            new-token (case token
                        left-p right-p
                        right-p left-p
                        token)]
        (recur (rest remaining)
               (conj new-expression new-token))))))

;; TODO - convert to loop/recur
(defn pop-stack-while!
  "Pops the operator stack while the predicate function evaluates to true and
  pushes the result to the output/operand stack. Used by infix-expression-eval."
  [predicate op-stack out-stack arg-count]
  (while (predicate)
    (let [op-or-fn-token (peek @op-stack)
          fn-arity (get-arity op-or-fn-token)
          op? (operator? op-or-fn-token)
          func? (function? op-or-fn-token)
          nil-equals-zero? (fn/nil-equals-zero? op-or-fn-token)
          num-args (cond
                  (and (or func? op?) (= fn-arity 0) (> @arg-count 0)) ; throw exception
                  (throw (ex-info (str "Expected 0 arguments but found more token arity fn-arity "
                                       op-or-fn-token " " @arg-count " " fn-arity) {:arity @arg-count}))
                  (and (or func? op?) (not= fn-arity fn/multi-arity) (< @arg-count fn-arity))
                  (throw (ex-info (str "Expected more arguments but found less "
                                       fn-arity " " @arg-count) {:arity @arg-count}))
                  (and (or func? op?) (not= fn-arity fn/multi-arity) (> @arg-count fn-arity))
                  (throw (ex-info (str "Expected less arguments but found more "
                                      fn-arity " " @arg-count) {:arity @arg-count}))
                  :else
                  @arg-count)]
      (debug/debug-log-detailed "out-stack before pop-stack-while!" @out-stack)
      (debug/debug-log-detailed "op-stack before pop-stack-while!" @op-stack)
      (debug/debug-log-detailed "nthrest @out-stack num-args" (nthrest @out-stack num-args))
      (when (or func? op?)
        (reset! out-stack
                (conj
                 (nthrest @out-stack num-args) ; pop operands equal to num-args of func
                 (apply (eval-token op-or-fn-token)
                        (map #(if nil-equals-zero? ; if function or operator has nil-equals-zero? flag
                                (or % 0) ; replace nil with 0
                                %) ; else just eval the token
                             (take num-args @out-stack))))))
      (swap! arg-count #(- % (dec num-args))) ; decrement arg count by (num-args - 1)
      (swap! op-stack pop))))

(defn infix-expression-eval [reversed-expr]
  (let [num-items (count reversed-expr)
        op-stack (atom ())
        arg-count (atom 0)
        out-stack (atom ())]
    (debug-log-detailed ">>> reversed-expr" reversed-expr)
    (dotimes [i num-items]
      (let [token (nth reversed-expr i)]
        (debug/debug-log-detailed "infix-expression-eval token" token)
        (cond
          ; if operand, adds it to the operand stack
          (operand? token)
          (do
            (swap! arg-count inc)
            (swap! out-stack conj (eval-token token)))

          ; left parenthesis 
          (= left-p token)
          (do
            (reset! arg-count 0)
            (swap! op-stack conj token))

          ; right parenthesis
          (= right-p token)
          (pop-stack-while!
             #(not= left-p (peek @op-stack)) op-stack out-stack arg-count)

          ;; comma ; TODO - don't tokenize commas in the first place
          ;;(= comma token)

          ;; if token is an operator and is the first one found in this expression
          (and (operator? token) (empty? @op-stack))
          (swap! op-stack conj token)

          (function? token)
          (do
            (debug/debug-log-detailed "function? is true: arg-count - token" @arg-count token)
            (swap! op-stack conj token)
            (pop-stack-while!
             #(function? (peek @op-stack)) op-stack out-stack arg-count)
            (swap! arg-count inc))

          ;; handles all other operators when not the first one
          (operator? token)
          (do
            (pop-stack-while!
             #(or (< (precedence token) (precedence (peek @op-stack)))
                  (and (<= (precedence token) (precedence (peek @op-stack)))
                       (= exp token)))
             op-stack out-stack arg-count)
            (swap! op-stack conj token)
            (swap! arg-count inc)))))
    ;; Once all tokens have been processed, pop and eval the stacks while op stack is not empty.
    (pop-stack-while! #(seq @op-stack) op-stack out-stack arg-count)
    ;; Assuming the expression was a valid one, the last item is the final result.
    (let [peek-result (peek @out-stack)]
      (if (not (nil? peek-result))
        (eval-token peek-result) ; handle edge case where formula is a single cell reference
        "#ERROR")))) ; TODO - better error handling and exception use

(defn infix-expression-prepare [infix-expression]
  (let [reversed-expr (swap-parentheses (swap-unary-minus (tok/tokenize infix-expression)))]
    (debug-log-detailed "infix-expression was: " infix-expression)
    (do-with-timer "infix-expression-eval" (infix-expression-eval reversed-expr))))

(defn parse-formula [^string formula-str]
  (r/track! #(infix-expression-prepare formula-str)))
