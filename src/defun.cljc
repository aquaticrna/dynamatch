;(ns
    ;^{:author "dennis <killme2008@gmail.com>"
      ;:doc "A macro to define clojure functions with parameter pattern matching
            ;just like erlang or elixir based on core.match. Please see
            ;https://github.com/killme2008/defun"}
  ;defun
  ;(:require #?(:clj [clojure.core.match]
               ;:cljs [cljs.core.match :include-macros true])
            ;#?@(:clj [[clojure.tools.macro :refer [name-with-attributes]]
                      ;[clojure.walk :refer [postwalk]]]))
  ;#?(:cljs (:require-macros [defun :refer [fun letfun defun defun-]])))
(ns defun
  (:require [clojure.core.match :as core.match]
            [clojure.tools.macro :refer [name-with-attributes]]
            [clojure.walk :refer [postwalk]]))



(defprotocol UpdatableClauses
  (update-clauses- [this update-fn args]))


(defn clauses-match-fn
  [clauses]
  ;(when-not (vector? vars)
    ;[(vector vars)
     ;(mapcat (fn [[c a]]
               ;[(if (not= c :else) (vector c) c) a])
       ;(partition 2 clauses)))])]
  ;; This is terrible... ideally, we'd be able to compile the forms outside the function. But my macro-foo is
  ;; too weak-sauce... :-( Need to be able to bind the args...
  (fn [& args]
    (eval (core.match/clj-form (vec args) clauses))))


;(defn fun*
  ;"Defines a function just like clojure.core/fn with parameter pattern matching
  ;See https://github.com/killme2008/defun for details."
  ;[name-sym sigs]
  ;{:forms '[(fun name? [params* ] exprs*) (fun name? ([params* ] exprs*)+)]}
  ;(let [name (when (symbol? name-sym) name-sym)
        ;sigs (postwalk
              ;(fn [form]
                ;(if (and (list? form) (= 'recur (first form)))
                  ;(list 'recur (cons 'vector (next form)))
                  ;form))
              ;sigs)
        ;sigs `([& args#]
               ;(match (vec args#)
                      ;~@(mapcat
                         ;(fn [[m & more]]
                           ;[m (cons 'do more)])
                         ;sigs)))]
    ;(list* 'fn (if name
                 ;(cons name sigs)
                 ;sigs))))

(declare match-fn)

(deftype MatchFn [clauses matchfn]
  ;#?(:clj clojure.lang.IFn :cljs IFn)
  clojure.lang.IFn
  (invoke [this]
    (matchfn))
  (invoke [this arg1]
    (matchfn arg1))
  (invoke [this arg1 arg2]
    (matchfn arg1 arg2))
  (invoke [this arg1 arg2 arg3]
    (matchfn arg1 arg2 arg3))
  (invoke [this arg1 arg2 arg3 arg4]
    (matchfn arg1 arg2 arg3 arg4))
  (invoke [this arg1 arg2 arg3 arg4 arg5]
    (matchfn arg1 arg2 arg3 arg4 arg5))
  (invoke [this arg1 arg2 arg3 arg4 arg5 arg6]
    (matchfn arg1 arg2 arg3 arg4 arg5 arg6))
  (invoke [this arg1 arg2 arg3 arg4 arg5 arg6 arg7]
    (matchfn arg1 arg2 arg3 arg4 arg5 arg6 arg7))
  (invoke [this arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8]
    (matchfn arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8))
  (invoke [this arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9]
    (matchfn arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9))
  (invoke [this arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10]
    (matchfn arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10))
  UpdatableClauses
  (update-clauses- [this update-fn args]
    (let [new-clauses (apply update-fn clauses args)]
      (match-fn new-clauses))))

(defn update-clauses
  [match-fn update-fn & args]
  (update-clauses- match-fn update-fn args))

(defn match-fn
  [clauses]
  (MatchFn. clauses (clauses-match-fn clauses)))


(comment
  (def mf (match-fn '([:this] :that [x] (* 3 x))))
  (mf 8)
  (mf :this)
  (def new-mf (update-clauses mf (fn [clauses] (concat '([y] (* 999 y)) clauses))))
  (new-mf 8)
  (new-mf :this))

(defmacro fun
  "Defines a function just like clojure.core/fn with parameter pattern matching
  See https://github.com/killme2008/defun for details."
  [& sigs]
  {:forms '[(fun name? [params* ] exprs*) (fun name? ([params* ] exprs*)+)]}
  ;; Would be nice to be able to actually use name... For now we're just throwing away
  (let [name (when (symbol? (first sigs)) (first sigs))
        sigs (if name (next sigs) sigs)
        sigs (if (vector? (first sigs))
               (list sigs)
               (if (seq? (first sigs))
                 sigs
                 ;; Assume single arity syntax
                 (throw (IllegalArgumentException.
                         (if (seq sigs)
                           (str "Parameter declaration "
                                (first sigs)
                                " should be a vector")
                           (str "Parameter declaration missing"))))))
        sigs (postwalk
              (fn [form]
                (if (and (list? form) (= 'recur (first form)))
                  (list 'recur (cons 'vector (next form)))
                  form))
              sigs)
        sigs (mapcat
               (fn [[m & more]]
                 [m (cons 'do more)])
               sigs)
        form (list `match-fn (list `quote sigs))]
    (println form)
    form))

(defmacro defun
  "Define a function just like clojure.core/defn, but using core.match to
  match parameters. See https://github.com/killme2008/defun for details."
  [name & fdecl]
  (let [[name body] (name-with-attributes name fdecl)
        body (if (vector? (first body))
               (list body)
               body)
        name (vary-meta name assoc :argslist (list 'quote (@#'clojure.core/sigs body)))]
    `(def ~name (fun ~@body))))


;; Now time to add our add-match macro

(defmacro addmatch
  [matchfn-var-sym pattern & match-forms]
  (list `alter-var-root
        (list `var matchfn-var-sym)
        update-clauses
        conj
        (list `quote (list pattern (cons `do match-forms)))))

(comment
  (defun star
    ([:this] :that)
    ([x] (* x 4)))
  (macroexpand (fun ([:this] :that)))
  (star :this)
  (star 7)
  (addmatch star [:whatevs] :poop-butt)
  #_(poop))




;#?(:clj
   ;(defmacro if-cljs
     ;"Return then if we are generating cljs code and else for clj code.
     ;Source:
     ;http://blog.nberger.com.ar/blog/2015/09/18/more-portable-complex-macro-musing/"
     ;[then else]
     ;(if (boolean (:ns &env)) then else)))

;#?(:clj
   ;(defmacro match
     ;[& args]
     ;`(if-cljs (cljs.core.match/match ~@args)
               ;(clojure.core.match/match ~@args))))

;#?(:clj
   ;(defmacro fun
     ;"Defines a function just like clojure.core/fn with parameter pattern matching
     ;See https://github.com/killme2008/defun for details."
     ;[& sigs]
     ;{:forms '[(fun name? [params* ] exprs*) (fun name? ([params* ] exprs*)+)]}
     ;(let [name (when (symbol? (first sigs)) (first sigs))
           ;sigs (if name (next sigs) sigs)
           ;sigs (if (vector? (first sigs))
                  ;(list sigs)
                  ;(if (seq? (first sigs))
                    ;sigs
                    ;;; Assume single arity syntax
                    ;(throw (IllegalArgumentException.
                            ;(if (seq sigs)
                              ;(str "Parameter declaration "
                                   ;(first sigs)
                                   ;" should be a vector")
                              ;(str "Parameter declaration missing"))))))
           ;sigs (postwalk
                 ;(fn [form]
                   ;(if (and (list? form) (= 'recur (first form)))
                     ;(list 'recur (cons 'vector (next form)))
                     ;form))
                 ;sigs)
           ;sigs `([& args#]
                  ;(match (vec args#)
                         ;~@(mapcat
                            ;(fn [[m & more]]
                              ;[m (cons 'do more)])
                            ;sigs)))]
       ;(list* 'fn (if name
                    ;(cons name sigs)
                    ;sigs)))))

;#?(:clj
   ;(defmacro letfun
     ;"letfn with parameter pattern matching."
     ;{:forms '[(letfun [fnspecs*] exprs*)]}
     ;[fnspecs & body]
     ;`(letfn* ~(vec (interleave (map first fnspecs)
                                ;(map #(cons `fun %) fnspecs)))
              ;~@body)))

;#?(:clj
   ;(defmacro defun
     ;"Define a function just like clojure.core/defn, but using core.match to
     ;match parameters. See https://github.com/killme2008/defun for details."
     ;[name & fdecl]
     ;(let [[name body] (name-with-attributes name fdecl)
           ;body (if (vector? (first body))
                  ;(list body)
                  ;body)
           ;name (vary-meta name assoc :argslist (list 'quote (@#'clojure.core/sigs body)))]
       ;`(def ~name (fun ~@body)))))

;#?(:clj
   ;(defmacro defun-
     ;"same as defun, yielding non-public def"
     ;[name & decls]
     ;(list* `defun (vary-meta name assoc :private true) decls)))

