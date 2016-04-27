(ns
    ^{:author "Chris Small <metasoarous@gmail.com>"
      :doc "A macro to define clojure functions with parameter pattern matching
            based on core.match, but with multimethod-like extensibility. Please see
            https://github.com/metasoarous/defun"}
  defun
  (:require #?(:clj [clojure.core.match]
               :cljs [cljs.core.match :include-macros true])
            #?@(:clj [[clojure.tools.macro :refer [name-with-attributes]]
                      [clojure.walk :refer [postwalk]]]))
  #?(:cljs (:require-macros [defun :refer [fun letfun defun defun-]])))
;(ns defun
  ;(:require [clojure.core.match :as core.match]
            ;[clojure.tools.macro :refer [name-with-attributes]]
            ;[clojure.walk :refer [postwalk]]))


(defprotocol UpdatableClauses
  "A protocol which abstracts the ability to update a clause list based on an update function and an args collection seq.
  Note that the update-clauses fn is not part of the public api, but is an implementation detail; use update-clauses instead."
  (update-clauses- [this update-fn args]))

(defn- clauses-match-fn
  "Construct a match function from sequence of clauses, as you'd pass to `clojure.core.match/match`. This function is not
  part of the public API."
  [clauses]
  ;; This is terrible for perf... ideally, we'd be able to compile the forms outside the function. But not sure how to
  ;; do that while leaving the args variable unbound...
  (fn [& args]
    (eval (core.match/clj-form (vec args) clauses))))
  ;(fn [& args]
    ;(eval (core.match/clj-form [(vec args)] (mapcat (fn [[pattern match]] [[pattern] match])
                                                    ;clauses)))))


;; Declare our constructor so we can call it in our type definition
(declare match-fn)

(deftype MatchFn [clauses matchfn]
  ;; Ye old pyramid of invoke
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
  (invoke [this arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11]
    (matchfn arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11))
  (invoke [this arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12]
    (matchfn arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12))
  (invoke [this arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13]
    (matchfn arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13))
  (invoke [this arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13 arg14]
    (matchfn arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13 arg14))
  (invoke [this arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13 arg14 arg15]
    (matchfn arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13 arg14 arg15))
  (invoke [this arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13 arg14 arg15 arg16]
    (matchfn arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13 arg14 arg15 arg16))
  UpdatableClauses
  (update-clauses- [this update-fn args]
    (let [new-clauses (apply update-fn clauses args)]
      (match-fn new-clauses))))

(defn update-clauses
  "Update the cluases of a dynamic match fun by applying update-fn to the clauses list, along with any additional args."
  [match-fn update-fn & args]
  (update-clauses- match-fn update-fn args))

;; Should push down clauses as a list of lists further through the stack since it is semantically clearer and
;; gives us more extensibility
(defn match-fn
  "Create a new match fn object based on a list of clauses. Implementation detail; not part of the public API."
  [clauses]
  (MatchFn. clauses (clauses-match-fn clauses)))

(defmacro fun
  "Defines a function just like clojure.core/fn with parameter pattern matching and extensibility."
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

(defn prepend-clauses
  [clauses new-clauses]
  (concat new-clauses clauses))

(defmacro addmatch
  ([matchfn-var-sym pattern & match-forms]
   (list `alter-var-root
         (list `var matchfn-var-sym)
         `update-clauses
         `prepend-clauses
         (list `quote (list pattern (cons `do match-forms))))))

(defmacro addmatches
  ([matchfn-var-sym & new-clauses]
   (list `alter-var-root
         (list `var matchfn-var-sym)
         `update-clauses
         `prepend-clauses
         (list `quote (->> new-clauses
                           (mapcat (fn [[pattern & match-forms]] [pattern (cons `do match-forms)])))))))


;; Need to define a macro for overriding a given pattern match based on a keyword identifier (like :default)
(declare setmatch)

(comment
  (defun star
    ([:this] :that)
    ([x] (* x 4)))
  (star :this)
  (star 7)
  (addmatch star [:whatevs] :poop-butt)
  (star :whatevs)
  (map first (partition 2 (.clauses star)))
  (addmatches star
    ([:shifty] :shiner)
    ([:butt-butt] (println "Go dooger") :houser))
  (map first (partition 2 (.clauses star)))
  (star :shifty)
  (star :butt-butt)
  #_(poop))


;; Would be nice to add letfun and defun-, as below

;; Will this work as is?
;#?(:clj
   (defmacro letfun
     "letfn with parameter pattern matching."
     {:forms '[(letfun [fnspecs*] exprs*)]}
     [fnspecs & body]
     `(letfn* ~(vec (interleave (map first fnspecs)
                                (map #(cons `fun %) fnspecs)))
              ~@body))


(require '[clojure.test :as test])
(test/run-tests 'defun.core-test)

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

