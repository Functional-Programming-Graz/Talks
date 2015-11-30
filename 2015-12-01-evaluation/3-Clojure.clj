;; Again, the basic definition of our wrapper: this time as a macro.
;; It just takes the unevaluated code, and puts it inside a try/catch (tagging the 
;; result values, to disambiguate them -- like a union type).
(defmacro wrapExpr [body] 
  `(try 
     {:ok ~body} 
     (catch Throwable ~'t {:error (.getMessage ~'t)})))

;; Seems to work:
;;   user=> (wrapExpr (+ 39 3))
;;   {:ok 42}
;; What happens is this:
;;   user=> (macroexpand '(wrapExpr (+ 39 3)))
;;   (try {:ok (+ 39 3)} (catch java.lang.Throwable t {:error (.getMessage t)}))
;; Now, if we want to actually produce an error:
;;   user=> (wrapExpr (/ 39 0))
;;   {:error "Divide by zero"}
;; The evaluator will first call the macro on the expression '(/ 39 0):
;;   user=> (macroexpand '(wrapExpr (/ 39 0)))
;;   (try {:ok (/ 39 0)} (catch java.lang.Throwable t {:error (.getMessage t)}))
;; And then evaluate the resulting code:
;;   user=> (eval '(try {:ok (/ 39 0)} (catch java.lang.Throwable t {:error (.getMessage t)})))
;;   {:error "Divide by zero"}




;; can also be simulated 'by hand':
(defn wrapExprImpl [quoted]
  `(try              
     {:ok ~quoted} 
     (catch Throwable ~'t {:error (.getMessage ~'t)})))

;; Note the quoted argument this time:
;;   user=> (wrapExprImpl '(+ 39 3))
;;   (try {:ok (+ 39 3)} (catch java.lang.Throwable t {:error (.getMessage t)}))
;;   user=> (eval (wrapExprImpl '(+ 39 3)))
;;   {:ok 42}











(defmacro my-delay [body]
  `{:thunk (fn [] ~body) :value (atom nil)})  ;; a tagged union of thunk and cache

(defn my-force [delayed]
  (let [value (get delayed :value)
        thunk (get delayed :thunk)]
    (do  
      (if (nil? (deref value))
        (reset! value (thunk)))
      (deref value))))

;; alternatively, using map destructuring: 
;; (defn force [delayed]
;;   (let [{value :value thunk :thunk} delayed]
;;     (do  
;;       (if (nil? (deref value))
;;         (reset! value (thunk)))
;;       (deref value))))

;; Checking for expected behaviour: 
;;  user=> (def d (my-delay (do (print "hello\n") 42)))
;;  #'user/d
;;  user=> d
;;  {:value #object[clojure.lang.Atom 0x2415fc55 {:status :ready, :val nil}], :thunk #object[user$fn__51 0x13bc8645 "user$fn__51@13bc8645"]}
;;  user=> (my-force d)
;;  hello
;;  42
;;  user=> (my-force d)
;;  42
;;  user=> d
;;  {:value #object[clojure.lang.Atom 0x2415fc55 {:status :ready, :val 42}], :thunk #object[user$fn__51 0x13bc8645 "user$fn__51@13bc8645"]}






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; some more obscure experiments... thats not really content of the talk. Improvement
;; nevertheless welcome.

(defn replace-deep [x y tree] 
  (if (empty? tree)             ;; _not_ `nil?`!
    ()
    (let [car (first tree)
          cdr (rest tree)]
      (if (list? car) 
        (cons (replace-deep x y car)
              (replace-deep x y cdr))
        (cons (if (= x car) y car)
              (replace-deep x y cdr))))))

;; we want that
;;   (defn-by-need foo [x] (do-something-to x))
;; expands into
;;   (defn foo-delayed [x] (do-something-to (force x))
;;   (defmacro foo [x] `(foo-delayed (delay x)))

;; The below implementation works only for pure s-expressions (not, e.g., values in 
;; hash-maps or vectors), and does _not_ respect lexical scope inside the body!
(defn defn-by-need-3-impl [fun-name args body]
  (let [ext-body (if (list? body) body `(do ~body))
        new-body (reduce (fn [bdy arg] (replace-deep arg `(my-force ~arg) bdy)) ext-body args)
        new-name (symbol (str fun-name "-delayed"))
        new-args (map (fn [a] `(list `my-delay ~a)) args)]
    (do
      ;;(pprint args)
      ;;(pprint new-args)
      `(do
        (defn ~new-name [~@args] ~new-body)
        (defmacro ~fun-name ~args 
          `(~'~new-name ~~@new-args))))))

(defmacro defn-by-need-3 [fun-name args body]
  (defn-by-need-3-impl fun-name args body))

;; Usage examples:
;;   user=> (defn-by-need-3 if-true-else [cond else] (if cond cond else))
;;   #'user/if-true-else
;;   user=> (if-true-else (do (print "bla\n") 5) 6)
;;   bla
;;   5
;; To see what this converts the body into:
;;   user=> (pprint (macroexpand '(defn-by-need-3 if-true-else [cond else] (if cond cond else))))
;;   (do
;;    (clojure.core/defn
;;     if-true-else-delayed
;;     [cond else]
;;     (if (user/my-force cond) (user/my-force cond) (user/my-force else)))
;;    (clojure.core/defmacro
;;     if-true-else
;;     [cond else]
;;     (clojure.core/seq
;;      (clojure.core/concat
;;       (clojure.core/list 'if-true-else-delayed)
;;       (clojure.core/list
;;        (clojure.core/list 'user/my-delay cond)
;;        (clojure.core/list 'user/my-delay else))))))

;; testing stuff:
;; (defmacro test1 [x] `(do (defn ~'a [y#] (+ 42 ~x)) (defmacro ~'b [y#] `(+ 42 ~~x))))
;; (eval (let [yy (gensym)] `(let [~yy 3] ~(replace-deep 'x yy '(+ x 1)))))

;; (pprint (defn-by-need-3-impl 'foo '[x] '(+ 42 x)))
;; (pprint (macroexpand '(defn-by-need-3 foo [x] (+ 42 x))))       
;; (defn-by-need-3 foo [x] (+ x 42))




;; THIS WONT WORK BECAUSE WE NEED TO REPLACE EVERY SINGLE CALL, NOT 
;; ONLY FORCE ONCE AT THE BEGINNING (IN THE `LET`):       
;; (defn gensym-map [vars] 
;;   (apply merge (map #(hash-map %1 (gensym %1)) vars)))

;; (defn defn-by-need-4-impl [fun-name args body]
;;   (let [internal-names (gensym-map args)
;;         new-name (symbol (str fun-name "-delayed"))
;;         new-args (map (fn [a] `(list `my-delay ~a)) args)]
;;     (do
;;       ;;(pprint args)
;;       ;;(pprint new-args)
;;       `(do
;;         (defn ~new-name [~@(vals internal-names)]
;;           (let [[~@args] ~(mapv (fn [n] `(my-force ~n)) (vals internal-names))] ~body))
;;         (defmacro ~fun-name ~args 
;;           `(~'~new-name ~~@new-args))))))

;; (defmacro defn-by-need-4 [fun-name args body]
;;   (defn-by-need-4-impl fun-name args body))

;; (pprint (defn-by-need-4-impl 'foo '[x] '(+ 42 x)))
;; (pprint (macroexpand '(defn-by-need-4 foo [x] (+ 42 x))))       
;; (defn-by-need-4 foo [x] (+ x 42))

;; (pprint (macroexpand 
;;   '(defn-by-need-4 wrapExpr [expr] (try {:ok expr} (catch Throwable t {:error "error!"})))))
