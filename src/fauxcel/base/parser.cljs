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
   [fauxcel.util.debug :as debug :refer [debug-log do-with-timer]]))

;;; Scans tokens for minus signs and determines if the minus sign should
;;; be treated as a subtraction operator or a unary negation operator.
;;; If the latter it replaces the token with multiplication by -1 and surrounds
;;; with parentheses. This effectively makes unary minus the highest priority
;;; operator (same as Excel, Numbers and Google Sheets).
(defn swap-unary-minus [infix-tokens]
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

;;; Takes a token in string format and returns the corresponding function (if an operator)
;;; or the text in the cell (nil if empty) or the numeric value.
(defn eval-token [token]
  (cond
    ; If cell ref, evaluate and return 
    (cell-ref? token) ; moved this cond up for formulas with many cell refs, slight performance boost maybe?
    (util/recursive-deref (eval-cell-ref token))

    ; If it's an operator, return the function
    (:fn (operators token)) ; (operators token) returns nil if not found
    (:fn (operators token))

    (function? token)
    (:fn (get-function token))

    ;(cell-range? token) ; TODO check if safe to delete; moved cell range expansion to tokenizer
    ;(expand-cell-range token) 

    (and (string? token) (not (m/numeric? token)))
    token ; if string and not numeric, return the string

    ; must be a number then
    :else
    (do
      (debug-log ":else eval-number for token: " token)
      (m/eval-number token))))

;;; Needed to handle parentheses swapping for expression reversal
(defn swap-parentheses [expression] ; turns "(" into ")" and vice versa
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

;;; Pops the operator stack while the predicate function evaluates to true and
;;; pushes the result to the output/operand stack. Used by infix-expression-eval
(defn pop-stack-while! [predicate op-stack out-stack arity]
  (while (predicate)
    (let [op-or-fn-token (peek @op-stack)
          fn-arity (get-arity op-or-fn-token)
          func? (function? op-or-fn-token) ; unlike built-in fn? , function? only returns true for non operator functions in function map
          nil-equals-zero? (cond (operator? op-or-fn-token)
                                 (:nil-equals-zero? (operators op-or-fn-token)) ; get nil-equals-zero? flag from operators map
                                 (function? op-or-fn-token)
                                 (:nil-equals-zero? (get-function op-or-fn-token))) ; get nil-equals-zero? flag from functions map
          arity (cond
                  (and (= fn-arity 0) (> @arity 0)) ; throw exception
                  (throw (ex-info "Expected 0 arguments but found more than 0" {:arity @arity}))

                  (and (not= fn-arity fn/multi-arity) (< @arity fn-arity))
                  (throw (ex-info "Expected more arguments but found less" {:arity @arity}))

                  :else
                  (do
                    ;(reset! arity 0)
                  @arity))]
      ;(when func? (swap! arity-stack pop))
      
      (reset! out-stack
              (conj
               (nthrest @out-stack arity) ; pop operands equal to arity of func
               (apply (eval-token op-or-fn-token)
                      (map #(if nil-equals-zero? ; if function or operator has nil-equals-zero? flag
                              (or % 0) ; replace nil with 0
                              %) ; else just eval the token
                           (take arity @out-stack)))))
      (swap! op-stack pop))))

(defn infix-expression-eval [reversed-expr]
  (let [num-items (count reversed-expr)
        op-stack (atom ())
        arity (atom 0)
        out-stack (atom ())]
        (debug-log "...>>>reversed-expr" reversed-expr)
    (dotimes [i num-items]

      (let [token (nth reversed-expr i)]
        (debug/debug-log-detailed "infix-expression-eval token" token)
        (cond
          ; if operand, adds it to the operand stack
          (operand? token)
          (do
            (swap! arity inc)
            (swap! out-stack conj (eval-token token)))

          ; left parenthesis 
          (= left-p token)
          (swap! op-stack conj token)
          
          ; right parenthesis
          (= right-p token)
          (do
            (pop-stack-while!
             #(not= left-p (peek @op-stack)) op-stack out-stack arity)
            (swap! op-stack pop))

          ; comma
          ;(= comma token)
          ;(swap! arity inc)

          ; if token is an operator and is the first one found in this expression
          (and (operator? token) (empty? @op-stack))
          (swap! op-stack conj token)

          (function? token)
          (do
            (debug/debug-log-detailed "arity function" @arity token)
            (swap! op-stack conj token)
            (pop-stack-while!
             #(function? (peek @op-stack)) op-stack out-stack arity))
            ;(swap! op-stack conj token) ;)

          ; handles all other operators when not the first one
          (operator? token)
          (do
            (pop-stack-while!
             #(or (< (precedence token) (precedence (peek @op-stack)))
                  (and (<= (precedence token) (precedence (peek @op-stack)))
                       (= exp token)))
             op-stack out-stack arity)
            (swap! op-stack conj token)))))
    ;; Once all tokens have been processed, pop and eval the stacks while op stack is not empty.
    (pop-stack-while! #(seq @op-stack) op-stack out-stack arity)
    ;; Assuming the expression was a valid one, the last item is the final result.
    (let [peek-result (peek @out-stack)]
       (if (not (nil? peek-result))
         (eval-token peek-result)
         "#ERROR")
       ))) ; handle edge case where formula is a single cell reference

(defn infix-expression-prepare [infix-expression]
  (let [reversed-expr (swap-parentheses (swap-unary-minus (tok/tokenize infix-expression)))]
    (debug-log "infix-expression was: " infix-expression)
    (do-with-timer "infix-expression-eval" (infix-expression-eval reversed-expr))))

(defn parse-formula [^string formula-str]
  (r/track! #(infix-expression-prepare formula-str)))
