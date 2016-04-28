(ns ^{:author "Chris Small <metasoarous@gmail.com>"
      :doc "A macro to define clojure functions with parameter pattern matching
            based on core.match, but with multimethod-like extensibility. Please see
            https://github.com/metasoarous/defun"}
  defun
  (:require #?(:clj [clojure.core.match :as core.match]
               :cljs [cljs.core.match :as core.match :include-macros true])
            #?@(:clj [[clojure.tools.macro :refer [name-with-attributes]]
                      [clojure.walk :refer [postwalk]]]))
  #?(:cljs (:require-macros [defun :refer [fun letfun defun defun-]])))



;; # Dynamatch

;; We'd like to be able to construct function objects -- based on pattern matching -- which allow us to extend
;; the set of patterns and actions we match on.
;; The goal of this is to strike a balance between the flexibility and expressiveness of pattern matching and
;; the extensibility of multimethods.

;; Our strategy here is to create a class of objects which act like methods but can be dynamically recompiled
;; by adding pattern match clauses.
;; Then we'll build up some macros which ease the construction of these functions, as well as the manipulation
;; of their clauses.


;; ## `MatchFn` objects

;; Towards our first goal, we'll define a `MatchFn` type whose instances wrap dynamic lists of clojure.core.match
;; clauses, as well as compiled functions based on these clauses.
;; This `MatchFn` type will implements the `IFn` protocol (making it act like a function) by delegating to the
;; compiled function.

;; We'll also need a protocol which allows us to update the clauses and recompile the wrapped function based
;; on these clauses.
;; To that end, here we have the `UpdatbleClauses` protocol:

(defprotocol UpdatableClauses
  "A protocol which abstracts the ability to update a clause list based on an update function and an args collection seq.
  Note that the update-clauses fn is not part of the public api, but is an implementation detail; use update-clauses instead."
  (update-clauses- [this update-fn args]))

;; Next, we'll declare our constructor so we can call it in our type definition (so that we can compile a new
;; match function when we update the function clauses)

(declare match-fn)

;; Now we can actually define our `MatchFn` type

(deftype MatchFn [name clauses matchfn]
  ;; Ye old pyramid of invoke
  #?(:clj clojure.lang.IFn :cljs IFn)
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
      (match-fn name new-clauses))))


;; Our public API shouldn't include this update-clauses- implementation, so we'll wrap it in the following
;; function:

(defn update-clauses
  "Update the cluases of a dynamic match fun by applying update-fn to the clauses list, along with any additional args."
  [match-fn update-fn & args]
  (update-clauses- match-fn update-fn args))


;; ## Compiling clauses

;; Our next goal is to compile a dynamic list of clauses into an actual function.
;; Towards this end, we'll define `clauses-match-fn`.

;; But first a helper function

(defn- rebind-var
  "Takes the output of `clojure.core.match/clj-form`, and rebinds the input data so we can bind it to our function's
  arguments vector dynamically."
  [clj-form new-var]
  (let [[let-sym [bound-args-sym _] inner-form] clj-form]
    (list let-sym [bound-args-sym new-var] inner-form)))

(defn clauses-match-fn
  "Construct a match function from sequence of clauses, as you'd pass to `clojure.core.match/match`. This function is not
  part of the public API."
  ([clauses] (clauses-match-fn (gensym "some-random-matchfn") clauses))
  ([fn-name clauses]
   (let [args-sym (gensym "args")
         compiled-expr (core.match/clj-form
                         [[:defun/token]]
                         (mapcat (fn [[pattern match]]
                                   [[pattern] match])
                           (partition 2 clauses)))
         compiled-expr (rebind-var compiled-expr args-sym)
         fn-def-form `(fn ~fn-name [& ~args-sym]
                        (let [~args-sym (vec ~args-sym)]
                          ~compiled-expr))]
     (eval fn-def-form))))

;; Quick test of this:

(comment
  (let [clauses '([x] (* x 3)
                  [y x] (* y x))
        f (clauses-match-fn clauses)]
    (f 3)))


;; ## Constructor function

;; And now we'll actually define the constructor function we declared above.
;; In short, here we just create a new `MatchFn` object with the clauses we pass in, as well as a freshly
;; compiled match function.

;; XXX TODO Should push down clauses as a list of lists further through the stack since it is semantically clearer and
;; gives us more extensibility
(defn match-fn
  "Create a new match fn object based on a list of clauses. Implementation detail; not part of the public API."
  [name clauses]
  (MatchFn. name clauses (clauses-match-fn name clauses)))



;; ## Macros (proper API)

;; No one is going to want to have to write quoted forms as part of their API...
;; Well... probably...
;; I mean, we all love Datomic, so who knows.
;; There are advantages to that too.
;; But let's not drink that cool-aid just yet...


;; Our first macro, `fun`, will simple create a `MatchFn` instance based on a signature.
;; The goal is that it have exactly the same API as clojure's `fn`.

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
              ;; Do we always need this? Need to think about it...
              ;(fn [form]
                ;(if (= 'recur (first form))
                  ;(list 'recur (cons 'vector (next form)))))
              sigs)
        sigs (mapcat
               (fn [[m & more]]
                 [m (cons 'do more)])
               sigs)
        form `(match-fn '~(or name (gensym "fun")) '~sigs)]
    ;(clojure.pprint/pprint form)
    form))

;((fun testerer ([x] (* x 3))) 5)

;; Next, we have `defun`, which should mirror clojure's `defn` by defining a var which points to a `fun`

(defmacro defun
  "Define a function just like clojure.core/defn, but using core.match to
  match parameters. See https://github.com/killme2008/defun for details."
  [name & fdecl]
  (let [[name body] (name-with-attributes name fdecl)
        body (if (vector? (first body))
               (list body)
               body)
        name (vary-meta name assoc :argslist (list 'quote (@#'clojure.core/sigs body)))]
    `(do (declare ~name) (def ~name (fun ~name ~@body)))))

;; Now our extensibility macros

(defn prepend-clauses
  [clauses new-clauses]
  (concat new-clauses clauses))

;; XXX TODO Should rewrite these using syntax quote...

(defmacro addmatch
  ([matchfn-var-sym pattern & match-forms]
   `(alter-var-root
      (var ~matchfn-var-sym)
      update-clauses
      prepend-clauses
      (quote (~pattern (do ~@match-forms))))))

(defmacro addmatches
  ([matchfn-var-sym & new-clauses]
   `(alter-var-root
      (var ~matchfn-var-sym)
      update-clauses
      prepend-clauses
      (quote ~(mapcat (fn [[pattern & match-forms]] [pattern `(do ~@match-forms)])
                      new-clauses)))))


;; XXX TODO Need to define a macro for overriding a given pattern match based on a keyword identifier (like :default)
(declare setmatch)

;; Some examples testing these things out:

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


;; Uncomment if you'd like to run tests

;(require '[clojure.test :as test])
;(test/run-tests 'defun.core-test)


;; Copied over from original defun for reference in development...

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

