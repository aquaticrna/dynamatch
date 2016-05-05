(ns ^{:author "Chris Small <metasoarous@gmail.com>"
      :doc "A macro to define clojure functions with parameter pattern matching
            based on core.match, but with multimethod-like extensibility. Please see
            https://github.com/metasoarous/defun"}
  dynamatch
  #?(:clj (:refer-clojure :exclude [alter-var-root]))
  (:require #?(:clj [clojure.core.match :as core.match]
               :cljs [cljs.core.match :as core.match :include-macros true])
            #?@(:clj [[clojure.tools.macro :refer [name-with-attributes]]
                      [clojure.walk :refer [postwalk]]]))
  #?(:cljs (:require-macros [defun :refer [fun letfun defun addmatches addmatches! addmatch addmatch!]])))


;; A portable version of alter var root
;; From https://gist.github.com/eyelidlessness/e760c5350b113a0bbcab
;; Separate namespace?

#?(:clj
    (defmacro if-cljs
      "Return then if we are generating cljs code and else for Clojure code.
       http://blog.nberger.com.ar/blog/2015/09/18/more-portable-complex-macro-musing"
      [then else]
      (if (:ns &env) then else)))

(def resolve-clj
  (try clojure.core/resolve
    (catch Exception _
      (constantly nil))))

(defmulti sym->var
  (fn [env sym]
    (cond
      (contains? env sym) :clj
      (resolve-clj sym) :clj-resolved
      :else :cljs)))

(defn meta->fq-sym [{:keys [ns name] :as m}]
  (symbol (str (ns-name ns)) (str name)))

(defmethod sym->var :clj [env sym]
  (loop [init (-> env sym .-init)]
    (cond
      (instance? clojure.lang.Compiler$TheVarExpr init)
      (-> init .-var meta meta->fq-sym)

      (instance? clojure.lang.Compiler$LocalBindingExpr init)
      (recur (-> init .-b .-init))

      :default
      nil)))

(defmethod sym->var :clj-resolved [env sym]
  (-> sym resolve meta meta->fq-sym))

(defmethod sym->var :cljs [env sym]
  (let [init (get-in env [:locals sym :init])
        var-name (get-in init [:var :info :name])]
      (cond
        var-name var-name
        (:form init) (recur (:env init) (:form init))
        :else nil)))

#?(:clj
    (defmacro portable-alter-var-root [var-ref f]
      (let [var-seq? (and (seq? var-ref) (= 'var (first var-ref)))
            sym? (symbol? var-ref)
            var-sym (cond
                      var-seq? (second var-ref)
                      sym? (sym->var &env var-ref)
                      :else nil)]
        (if (nil? var-sym)
            `(throw (ex-info "Expected var" {:got ~var-ref}))
            `(if-cljs
               (set! ~var-sym (~f ~var-sym))
               (clojure.core/alter-var-root (var ~var-sym) ~f))))))


;; # Dynamatch

;; We'd like to be able to construct function objects -- based on pattern matching -- which allow us to extend
;; the set of patterns and actions we match on.
;; The goal of this is to strike a balance between the flexibility and expressiveness of pattern matching and
;; the extensibility of multimethods.

;; Our strategy here is to create a class of objects which act like methods but can be dynamically recompiled
;; by adding pattern match clauses.
;; Then we'll build up some macros which ease the construction of these functions, as well as the manipulation
;; of their clauses.


;; ## Pattern submatrices

;; To keep things sane, we'll be storing each set of pattern additions as a separate and named pattern
;; submatrix.
;; Once a submatrix has been added, its order within the greater matrix is fixed.
;; Re-evaluating a form which defines and adds one of these submatrices will then completely replace (in
;; place) the old clause stack for that submatrix, meaning that once initial load has taken place, order is
;; more or less fixed.


;; ## `MatchFn` objects

;; Towards our goal, we'll define a `MatchFn` type whose instances wrap dynamic lists of clojure.core.match
;; clauses, as well as compiled functions based on these clauses.
;; This `MatchFn` type will implements the `IFn` protocol (making it act like a function) by delegating to the
;; compiled function.

;; We'll also need a protocol which allows us to update the clauses and recompile the wrapped function based
;; on these clauses.
;; To that end, here we have the `UpdatbleClauses` protocol:


;; Conceptual breakdown

;; * clause stack
;;   * contigs
;; * pattern matrix
;;   * pattern blocks
;;   * default pattern



(defprotocol ClauseStack
  "A _clause stack_ is an ordered collection of _clause contigs_ which represent contiguous sub stacks of a derivable
  clause sequence which can be processed using pattern matching."
  (update-contigs- [this update-fn args]
    "Updates clause contigs (stored as `{:keys [ident clauses]}` maps) vector based on an update-fn and optionally an args vector.")
  (set-default- [this clause]
    "Sets the default clause in the stack (always at end of stack).")
  ;; This feels rather bad; need to think about what the right abstraction is here for this
  (contigs- [this]
    "The contiguous clause blocks for this clause stack"))
    

;; Next, we'll declare our constructor so we can call it in our type definition (so that we can compile a new
;; match function when we update the function clauses)

(declare new-match-fn)

;; Now we can actually define our `MatchFn` type

(defn validate-clause-stacks!
  ([contigs]
   (assert (apply distinct? :default (map :ident contigs))
           "Clause stack idents must be unique!")
   (assert (apply distinct? :default (->> contigs
                                          (mapcat (comp (partial map (comp :ident meta)) :clauses))
                                          (remove nil?)))
           "Clause idents must be unique!")))

(deftype MatchFn [name contigs default-clause matchfn]
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
  ClauseStack
  (update-contigs- [this update-fn args]
    (let [new-contigs (vec (apply update-fn contigs args))]
      (validate-clause-stacks! new-contigs)
      (new-match-fn name new-contigs default-clause)))
  (set-default- [this new-default]
    (new-match-fn name contigs new-default))
  (contigs- [this]
    contigs))


;; Our public API shouldn't directly include these protocol functions, so we'll wrap it in the following
;; function:

(defn update-contigs
  "Update the cluases of a dynamic match fun by applying update-fn to the contigs vector.
  Each clause stack is a map {:ident ident-keyword :clauses clause-seq}."
  [match-fn update-fn & args]
  (update-contigs- match-fn update-fn args))

(defn set-default
  [match-fn new-clause]
  (set-default- match-fn new-clause))

;; Building out our core functionality around these things so that we have pure functions underneath
;; everything; These core functions take quoted form clauses

(defn add-contig
  ([match-fn new-contig {:as opts :keys [after before]}]
   (assert (not (and after before)))
   (update-contigs
     match-fn
     ;; Todo; write logic for general :after or :before options pointing to any existing contig ident
     ;; All that exists currently is :after :end and :before :beginning
     (cond after
           (case after
             :end conj)
           before
           (case before
             :beginning (fn [contigs new-contig] (cons new-contig contigs))))
     new-contig))
  ([match-fn new-contig]
   (add-contig match-fn new-contig {:after :end})))

(defn set-contig
  "Set in place a contig based on ident"
  [match-fn new-contig]
  (update-contigs
    match-fn
    (fn [contigs]
      (assert ((set (map :ident contigs)) (:ident new-contig))
              "set-contig called for new-contig which doesn't match any existing contigs by ident")
      (let [i (some (fn [[i contig]]
                      (when (= (:ident contig) (:ident new-contig))
                         i))
                    (map vector (range) contigs))]
        (assoc contigs i new-contig)))))

(defn add-or-set-contig
  "Doesn't actually run an update if the ident is already taken; instead updates in place."
  ([match-fn new-contig opts]
   ;; Hmm... should be in protocol?
   (if-not ((set (map :ident (contigs- match-fn)))
            (:ident new-contig))
     (add-contig match-fn new-contig opts)
     (set-contig match-fn new-contig)))
  ([match-fn new-contig]
   (add-or-set-contig match-fn new-contig {:after :end})))

;; Eventually...
;(defn remove-contig)


;; ## Compiling clauses

;; Our next goal is to compile a dynamic list of clauses into an actual function.
;; Towards this end, we'll define `compiled-clauses`.

;; But first a helper function

(defn- rebind-var
  "Takes the output of `clojure.core.match/clj-form`, and rebinds the input data so we can bind it to our function's
  arguments vector dynamically."
  [clj-form new-var]
  (let [[let-sym [bound-args-sym _] inner-form] clj-form]
    (list let-sym [bound-args-sym new-var] inner-form)))

(defn- compiled-clauses
  "Construct a match function from sequence of clauses, as you'd pass to `clojure.core.match/match`. This function is not
  part of the public API."
  ([clauses]
   (compiled-clauses (gensym "some-random-matchfn") clauses))
  ([fn-name clauses]
   (let [args-sym (gensym "args")
         compiled-expr (core.match/clj-form
                         [[::compile-token]]
                         (mapcat (fn [[pattern & match]]
                                   [[pattern] (cons 'do match)])
                           clauses))
         compiled-expr (rebind-var compiled-expr args-sym)
         fn-def-form `(fn ~fn-name [& ~args-sym]
                        (let [~args-sym (vec ~args-sym)]
                          ~compiled-expr))]
     (eval fn-def-form))))



;; Quick test of this:

(comment
  (let [clauses '([x] (* x 3)
                  [y x] (* y x))
        f (compiled-clauses clauses)]
    (f 3)))


;; ## Constructor function

;; And now we'll actually define the constructor function we declared above.
;; In short, here we just create a new `MatchFn` object with the clauses we pass in, as well as a freshly
;; compiled match function.

;; XXX TODO Should push down clauses as a list of lists further through the stack since it is semantically clearer and
;; gives us more extensibility
(defn new-match-fn
  "Create a new match fn object based on a list of clauses. Implementation detail; not part of the public API."
  ([name contigs]
   (new-match-fn name contigs nil))
  ([name contigs default-clause]
   (let [flattened-clauses (concat (mapcat :clauses contigs)
                                   [default-clause])
         compiled-fn (compiled-clauses name flattened-clauses)]
     (MatchFn. name contigs default-clause compiled-fn))))

(defn print-contigs
  [match-fn]
  (doseq [{:keys [ident clauses]} (contigs- match-fn)]
    (println ident)
    (doseq [c clauses]
      (println "  " (:ident (meta c)) (first c)))))


;; ## Macros (proper API)

;; No one is going to want to have to write quoted forms as part of their API...
;; Well... probably...
;; I mean, we all love Datomic, so who knows.
;; There are advantages to that too.
;; But let's not drink that cool-aid just yet...


;; Our first macro, `fun`, will simple create a `MatchFn` instance based on a signature.
;; The goal is that it have exactly the same API as clojure's `fn`.

#?(:clj
    (defmacro fun
      "Defines a function just like clojure.core/fn with parameter pattern matching and extensibility."
      [& sigs]
      {:forms '[(fun name? [params* ] exprs*) (fun name? ([params* ] exprs*)+)]}
      (let [orig-sigs sigs
            name (when (symbol? (first sigs)) (first sigs))
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
            orig-sigs sigs
            sigs (postwalk
                  (fn [form]
                    (if (and (list? form) (= 'recur (first form)))
                      (list 'recur (cons 'vector (next form)))
                      form))
                  sigs)
            ;; Fixing the metadata... I think the postwalk mucks this up; not sure why I need first below
            ;; though... Was wrapping in an extra list form without...
            sigs (first (map (fn [sig orig-sig] (with-meta sig (meta orig-sig)) sigs orig-sigs) sigs orig-sigs))
            ;; XXX Should add explicit syntax for some default clause
            form `(new-match-fn '~(or name (gensym "fun")) [{:ident :dynamatch/main :clauses '~sigs}])]
        form)))


(comment
  ((fun testerer ^{:this :that} ([x] (* x 3))) 5)
  (print-contigs (fun testerer ^{:ident :this} ([x] (* x 3)))))


;; Next, we have `defun`, which should mirror clojure's `defn` by defining a var which points to a `fun`

(defmacro defined?
  [varname]
  `(if-cljs
     ;; This should be optimized for the cljs case (if possible) to test if varname is a var; tricky since
     ;; cljs doesn't actually have vars... but what we want to know is if it was defined with def, really.
     ;; Uhh... have to use reader conditional here to get it not to break on varname when trying to compile
     ;; this form...
     #?(:cljs (and ~varname (instance? MatchFn ~varname)) :clj ::brokerz?)
     (and (resolve '~varname) (bound? instance? MatchFn ~varname))
     (let [v# (def ~varname)]
       (when-not (and (.hasRoot v#) (instance? MatchFn (deref v#)))
         (def ~(with-meta mm-name m)
              (new clojure.lang.MultiFn ~(name mm-name) ~dispatch-fn ~default ~hierarchy))))))

;(clojure.pprint/pprint (macroexpand-1 '(defined? fsss)))
;(resolve 'fsss)
;(defined? fsss)
;(defined? *)


(defmacro defun
  "Define a function just like clojure.core/defn, but using core.match to
  match parameters. See https://github.com/killme2008/defun for details."
  [name & fdecl]
  (let [[name body] (name-with-attributes name fdecl)
        body (if (vector? (first body))
               (list body)
               body)
        name (vary-meta name assoc :argslist (list 'quote (@#'clojure.core/sigs body)))]
    ;; Attempting to update with defonce semantics; should only create the var if it doesn't exist, and if it does
    ;; update the clause set in place (with :dynamatch/main as the ident)
    `(do
       (declare ~name)
       (if (instance? MatchFn ~name)
         (addmatches! ~name :dynamatch/main ~@body)
         (def ~name (fun ~name ~@body))))))
      ;; XXX TODO Might be a problem with this when you want to declare some fn before hand...

;(clojure.pprint/pprint (macroexpand '(defun thefun ([x] (* x 3)))))
;(defun thefun ([x] (* x 3)))
;(thefun 8)

;; Now our extensibility macros

(defn prepend-clauses
  [clauses new-clauses]
  (concat new-clauses clauses))

#?(:clj
    (defmacro set-default!
      [matchfn-var-sym pattern & match-forms]
      `(portable-alter-var-root
         ~matchfn-var-sym
         set-default
         '~(cons pattern match-forms))))

#?(:clj
    (defmacro addmatches
      [matchfn block-ident & clauses]
      (let [opts (when (map? (first clauses)) (first clauses))
            clauses (if opts (next clauses) clauses)]
        `(add-or-set-contig ~matchfn {:ident ~block-ident :clauses '~clauses} ~@(when opts [opts])))))

#?(:clj
    (defmacro addmatch
      [matchfn block-ident pattern & match-forms]
      `(addmatches ~matchfn ~block-ident ^{:ident ~block-ident} ~(cons pattern match-forms))))

#?(:clj
    (defmacro addmatches!
      ([matchfn-var-sym block-ident & new-clauses]
       `(portable-alter-var-root
          ~matchfn-var-sym
          (fn [matchfn#] (addmatches matchfn# ~block-ident ~@new-clauses))))))

#?(:clj
    (defmacro addmatch!
      ([matchfn-var-sym block-ident pattern & match-forms]
       `(portable-alter-var-root
          ~matchfn-var-sym
          (fn [matchfn#] (addmatch matchfn# ~block-ident ~pattern ~@match-forms))))))


;(clojure.pprint/pprint (macroexpand '(set-default! varsym [x] (* x y) (println))))
;(clojure.pprint/pprint (macroexpand '(addmatch thefn :block-ident [x] (println x) (* x 3))))
;(clojure.pprint/pprint (macroexpand '(addmatches thefn :block-ident {:after :theone} ([x] (println x) (* x 3)))))
;(clojure.pprint/pprint (macroexpand-1 (macroexpand-1 '(addmatches! thefun :block-ident {:after :theone} ([x] (println x) (* x 3))))))


;; XXX TODO Need to define a macro for overriding a given pattern match based on a keyword identifier (like :default)
;; Update; actually maybe we don't need this if we're using the semantic of using idents keywords to refer to things
(declare setmatch)

;; Some examples testing these things out:


;; Need a fn which compiles a fn based on a particular set of idents
;; This way you can override things, by adding before, but then still refer to them by their ident

(comment
  ;; Testing out new api
  (ns-unmap *ns* 'thefun)
  (defun thefun
    ^{:ident :main-form}
    ([{:this :thing} x] (* x 333)))
  (thefun {:this :thing} 5)
  (print-contigs thefun)
  ;; Seems to be adding the clauses, but not on second addmatches! (where you might want to change the values
  ;; associated with the block ident; check the add-or-set-contig function, since this is where the
  ;; interesting bits happen
  (addmatches! thefun :some-block-name {:before :beginning}
    ^{:ident :this-form} ([:this x] (* x 4))
    ^{:ident :something} ([x y]     (* x y)))
  ;; This one should 
  (thefun 8 99)
  (thefun {:this :thing} 5)
  ;; Longer form with idents; definitely some extra noise...
  (addmatches! thefun :some-block-name {:before :beginning}
    ^{:ident :this-form}
    ([:this x]
     (let [x (* x 4)]
       (println x)
       (* x 11)))
    ^{:ident :something}
    ([x y]
     (let [y (* x y)]
       (println y)
       y)))
  ;; Not that much extra noise if you don't need the idents
  (addmatches! thefun :another-block-name {:after :end}
    ([:this x]
     (let [x (* x 4)]
       (println x)
       (* x 11)))
    ([x y]
     (let [y (* x y)]
       (println y)
       y)))
  ;; Testing out simpler api
  (ns-unmap *ns* 'star)
  (defun star
    ([:this] :that)
    ([x] (* x 4)))
  (star :this)
  (star 7)
  ;; This should add :whatevs-ident as a clause ident as well
  (addmatch! star :whatevs-ident [:whatevs] :poop-butt)
  (print-contigs star)
  (star :whatevs)
  (addmatches! star :shifty-butt-butt-matches {:before :beginning}
    ([:shifty] :shiner)
    ([:butt-butt] (println "Go dooger") :houser))
  (star :shifty)
  (star :butt-butt)
  #_(poop))


;; Would be nice to add letfun and defun-, as below

;; Will this work as is?
#?(:clj
    (defmacro letfun
      "letfn with parameter pattern matching."
      {:forms '[(letfun [fnspecs*] exprs*)]}
      [fnspecs & body]
      `(letfn* ~(vec (interleave (map first fnspecs)
                                 (map #(cons `fun %) fnspecs)))
               ~@body)))

;#?(:clj
   ;(defmacro defun-
     ;"same as defun, yielding non-public def"
     ;[name & decls]
     ;(list* `defun (vary-meta name assoc :private true) decls)))

;; Uncomment if you'd like to run tests

;(require '[clojure.test :as test])
;(test/run-tests 'dynamatch.core-test)


