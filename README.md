
# Dynamatch

Betwixt pattern matching and the multimethod

<br/>


## Introduction

We'd like to be able to define pattern matching functions which have multimethod-like extensibility.

Multimethods are wonderful, but they have a weakness:
The dispatch function is fixed, meaning you cannot extend the semantics of how a multimethod dispatches.

In contrast, pattern matching is immensely expressive with respect to how you describe what shapes of data should be treated a certain way.
However, the problem with this flexibility is that it's possible to have distinct match patterns which match for overlapping data.
As such, it's necessary that pattern matching respect clause order, so that order wins when multiple patterns match some input data.
This order-dependence leads to significant complications in the semantics of how we'd extend such a function in a multimethod-like fashion.

Dynamatch addresses these challenges by enforcing a certain structure in our ordering of match clauses such that many of the incidental complexities of order dependence in a dynamically extensible pattern matching system become more manageable.

<br/>


## Usage

This library intends to be feature complete and more or less interchangeable with [defun](https://github.com/killme2008/defun), from which it was forked.
In particular, we presently have implemented extensible versions of `fun` and `defun` (but not yet `letfun` or `defun-`).
Patterns are compiled using [core.match](https://github.com/clojure/core.match), and we offer its full expressiveness.

We organize our match clauses as `MatchFn` objects, which implement the `IFn` protocol.
These objects also implement some protocols which allow us to manipulate and recompile the clause stack into a callable function.

In this setting, `fun` returns a `MatchFn` object while `defun` creates or updates a var pointing to a `MatchFn`.

Let's walk through a use case:

### Defining a match function

Let's start by defining a function & var using `defun`.

```clojure
(require '[dynamatch :refer :all])

(defun greet
  ([:bob] "Hey bob")
  ([x] (str "Greetings, " x)))

(greet :bob) ;; => "Hey bob"
(greet "Joe") ;; => "Greetings, Joe"
```

That's it; this creates a `MatchFn` that computes the described clause stack.
This macro should operate more or less as a drop in for `defn`, with the obvious addition of pattern matching semantics.
The `fun` version works more or less the same, but like the analogous core `fn` form, directly returns a `MatchFn` value instead of creating a var.

### Adding matches

Suppose we now want to add some patterns to the top of this clause stack.

We can use the `addmatches!` macro for this:

```clojure
(addmatches! greet :chef-matches {:before :beginning}
  ([:emeril] "Love the zest")
  ([:child] "First, we baste the chicken!"))
```

We call this sequence of matches a _match block_.

There are two important things to notice here.

#### Block idents

The second argument to the macro is the `:chef-matches` keyword.
This is an `:ident` key used by Dynamatch to keep track of your calls to `addmatches!`.
When this form is re-evaluated, you will will be guaranteed that as long as this value remains constant, the _match block_ being added will remain contiguous within `greet`'s match ordering, and will retain its position relative to other match blocks.
Thus, after initial `addmatches!` form compilation, we have the following:

* We can re-evaluate any block, and nothing will change if the code hasn't changed
* If we change one of these blocks and re-evaluate, all old clauses will be removed, and the corresponding new clauses put in their place
* We can reorder clauses within a given block and these updates will not affect the ordering of blocks

If you take a minute to think about a naive approach to adding matches to such a function without organizing them by named blocks, it should be pretty easy to see how challenging it would be to get the semantics listed above.
It's our current hypothesis that this is the simplest approach towards resolving the issues 

#### Position

The third argument to the macro, `{:before :beginning}` indicates that this _match block_ should go at the beginning of the match stack being constructed, and thus have highest precedence.
Omitting this options map is equivalent to setting the options to `{:after :end}`, which adds to the very end or bottom of the clause stack (lowest precedence).
Lowest precedence is default because it offers the least surprise from the perspective of modularity.
With it, we know that it's only ever possible to _add_ behavior to the system; no behavior will be modified (except perhaps from a default).
This makes it easier to reason about match blocks as modular units, and should be taken into consideration when using Dynamatch.
The multimethod pattern explicitly avoids these issues (as stated above), so extra care and thought is still required with matching.

Eventually we'll also support specifying `:before` and `:after` with arbitrary block idents for more control over positioning.
For greater clarity, we should also reserver `:beginning` and `:end` as options for a third `:at` option.

#### Adding single matches

It's also worth mentioning that there is a separate form for adding single matches to a match stack.

```clojure
(addmatch! greet :hendrix-match {:before :beginning}
  [:hendrix] "Castles made of sand fall in the sea.")
```

This creates a match block with a single match clause in it.


### Clause idents

It's possible to specify idents not just for entire match blocks, but for individual match clauses as well.
We can do this using metadata on the clauses:

```clojure
(addmatches! greet :chef-matches {:before :beginning}
  ^{:ident :emeril-match}
  ([:emeril] "Love the zest")
  ^{:ident :julia-child-match}
  ([:child] "First, we baste the chicken!"))
```

Why on earth would we want to do that?

#### Overriding individual match clauses

[Pending; WIP]

By naming individual clauses, we have the ability to override their behavior individually.

```clojure
(setmatch! greet :emeril
  ([:emeril] "Bam!"))
```

It's worth pointing out that adding a single match with `addmatch!` assigns the block ident as the single clause's ident as well, so it's possible to override such a match using the block's ident...


### Default values

Dynamatch supports the specification of a special `:default` clause which will always come at the very end of the compiled match stack.
As such, to preserve the semantics of `:default`, it's recommended that you never define a clause that matches any input pattern of permissible arity, unless that pattern is set as below:

```clojure
(set-default! greet
  [& args] (str (apply str "Who is this " args) "?"))
```

Eventually, we may analyze match patterns added to ensure none are implied by the default match pattern.

### Inspecting the match stack and match blocks

We offer the `print-blocks` (soon to be; `print-contigs` currently) function/macro for printing out the stacks 
This gives you something like this:

```
(print-blocks greet)
;; :hendrix-match
;;    :hendrix-match [:hendrix]
;; :chef-matches
;;    :emeril-match [:emeril]
;;    :julia-child-match [:child]
;; :dynamatch/main
;;    nil [:bob]
;;    nil [x]
```

Still need to get this to display the default...

Also note: The block initially defined with `defun` is assigned a `:dynamatch/main` ident.

### Misc

There is an `update-blocks` (currently `update-contigs`; to be changed soon) function which let's you specify an arbitrary function to be applied to the block stack of a `MatchFn` instance.
We may add an `update-blocks!` function which applies this to our var.

I've also thought it might be interesting to offer a `merge-funs` option that would effectively stack two functions on top of each other (preserving defaults).
That's perhaps not that valuable though.

`overmatch`

Re-define a pattern match by turning it into it's own pattern match clause matrix?
What about redefine semantics of the form that initially created the ident though?
Maybe this doesn't make sense... but seems there's something here.

`matchbind`

It should be possible to create a lexical scope where a dynamic matchfn has been overridden.

<br/>



## Clojurescript support...

This is coming soon.

I wanted for the Clojure implementation of this to be fully dynamic, and so used `eval` to dynamically compile functions based on the compiled code produced by `core.match`.
Of course, Clojurescript does not have `eval`...
So I'm quite certain we won't be able to get the full dynamism we have with the Clojure version.

But this is probably okay.
We should be able to get a static projection of the extension semantics.
In particular, we can still have variants of `fun`, `defun`, `addmatches!`, `addmatch!`, `set-default!`, etc.
But the dynamic functionality (`addmatches`, `addmatch`, `set-default`, `update-args`, etc.) will not be supported.

How do we do this?
We should be able to use the compile time var metadata features of the ClojureScript compiler (described in [What's in a Var](http://swannodette.github.io/2014/12/17/whats-in-a-var/), by David Nolen) to build up the match stacks statically via `defun`, `addmatches!`, etc.

Right now though, I'm still trying to wrap my head around how to restructure code for the macro separation required by ClojureScript.
Once that's done, I'll start figuring out how to get the support for the compile time extensibility.

Another possibility... we may be able to fudge some of the dynamic features in cljs by "pretending", and not actually recompiling a new clause stack, but just keeping track of fns defined at compile time for each block, and using a dynamic compose function to give something with functional equivalence, which executes less efficiently (in general), due to the inability to optimize the entire match matrix.
Something to think about.

<br/>


## Ideas for the future

### Misc todo

* Make it so that catch-alls are caught and only allowed as the actual :default clause
* Get defn- and letfn working
* Should be :at :bottom, :at :top or :before vs :after existing block ident
* Reorganize to work with cljs
* Make it possible to update a specific clause

</br>



## Contributors

This library was initially forked from the [defun](https://github.com/killme2008/defun) library.
The following individuals were listed as contributors at the time of this fork:

- [killme2008](https://github.com/killme2008)
- [kgann](https://github.com/kgann)
- [danielcompton](https://github.com/danielcompton)
- [Sander Dijkhuis](https://github.com/sander)

As of the Dynamatch fork, we have the following contributors:

- [Christopher Small](https://github.com/metasoarous)
- [Kyle Langford](https://github.com/aquaticrna)

</br>


## License

Copyright © 2016 [Christopher Small](https://github.com/metasoarous)

Distributed under the Eclipse Public License either version 1.0 or (at your option) any later version.

<br/>
<br/>



# Below is the original defun Readme


# defun

A macro to define clojure functions with pattern matching just as erlang or elixir.

**Since 0.3.0, defun supports ClojureScript.**

## Usage

Dependency in leiningen:

``` clj
    [defun "0.3.0-alapha"]
```

### Basic usage

Require `defun` in clojure:

```clj
(require '[defun :refer [defun]])
```

Or `refer-macros` in clojurescript:

```cljs
(ns cljs-test
  (:require  [defun :refer-macros [defun]])
(enable-console-print!)            
```

Try to define function just like `defn`:

``` clj
(defun hello
   "hello world"
   [name] (str "hello," name))
(hello "defun")
;; "hello,defun"
```

Supports variadic arguments, doc, metadata etc. as `defun` too. No supprises?

The fun thing is coming, let's say hi to people:

``` clj
(defun say-hi
  ([:dennis] "Hi,good morning, dennis.")
  ([:catty] "Hi, catty, what time is it?")
  ([:green] "Hi,green, what a good day!")
  ([other] (str "Say hi to " other)))
```

Then calling `say-hi` with different names:

``` clj
(say-hi :dennis)
;;  "Hi,good morning, dennis."
(say-hi :catty)
;;  "Hi, catty, what time is it?"
(say-hi :green)
;;  "Hi,green, what a good day!"
(say-hi "someone")
;;  "Say hi to someone"
```

We define functions just like Erlang's function with parameters pattern match (thanks to [core.match](https://github.com/clojure/core.match)), we don't need `if,cond,case` any more, that's cool!

### Recursion

Let's move on, what about define a recursive function? That's easy too:

``` clj
(defun count-down
  ([0] (println "Reach zero!"))
  ([n] (println n)
     (recur (dec n))))
```

Invoke it:

``` clj
(count-down 5)
;;5
;;4
;;3
;;2
;;1
;;Reach zero!
nil
```

An accumulator from zero to number `n`:

``` clj
    (defun accum
      ([0 ret] ret)
      ([n ret] (recur (dec n) (+ n ret)))
      ([n] (recur n 0)))

	 (accum 100)
	 ;;5050
```

A fibonacci function:

``` clj
(defun fib
    ([0] 0)
    ([1] 1)
    ([n] (+ (fib (- n 1)) (fib (- n 2)))))
```

Output:

``` clj
(fib 10)
;; 55
```

Of course it's not tail recursive, but it's really cool, isn't it?

### Guards

Added a guard function to parameters:

``` clj
(defun funny
  ([(N :guard #(= 42 %))] true)
  ([_] false))

(funny 42)
;;  true
(funny 43)
;; false
```

Another function to detect if longitude and latitude values are both valid:

``` clj
(defun valid-geopoint?
    ([(_ :guard #(and (> % -180) (< % 180)))
      (_ :guard #(and (> % -90) (< % 90)))] true)
    ([_ _] false))

(valid-geopoint? 30 30)
;; true
(valid-geopoint? -181 30)
;; false
```

### Private defun

Of course, you can use `defun-` to define a function that is private just as `defn-`

### More Patterns

In fact ,the above `say-hi` function will be expanded to be:

``` clj
(defn
 say-hi
 {:arglists '([& args])}
 [& args#]
 (clojure.core.match/match
  [(vec args#)]
  [[:dennis]]
  (do "Hi,good morning, dennis.")
  [[:catty]]
  (do "Hi, catty, what time is it?")
  [[:green]]
  (do "Hi,green, what a good day!")
  [[other]]
  (do (str "Say hi to " other))))
```

The argument vector is in fact a pattern in core.match, so we can use all patterns that supported by [core.match](https://github.com/clojure/core.match/wiki/Basic-usage).

For example, matching literals

``` clj
(defun test1
    ([true false] 1)
    ([true true] 2)
    ([false true] 3)
    ([false false] 4))

(test1 true true)
;; 2
(test1 false false)
;; 4
```

Matching sequence:

``` clj
(defun test2
    ([([1] :seq)] :a0)
    ([([1 2] :seq)] :a1)
    ([([1 2 nil nil nil] :seq)] :a2))

(test2 [1 2 nil nil nil])
;; a2
```

Matching vector:

``` clj
(defun test3
    ([[_ _ 2]] :a0)
    ([[1 1 3]] :a1)
    ([[1 2 3]] :a2))

(test3 [1 2 3])
;; :a2
```

Rest Pattern, Map Pattern, Or Pattern etc.

I don't want to copy the [core.match's wiki](https://github.com/clojure/core.match/wiki/Basic-usage),please visit it by yourself.

### fun and letfun

Since 0.2.0, there are two new macros: `fun` and `letfun`, just like `clojure.core/fn` and `clojure.core/letfn`

``` clojure
((fun
    ([[_ _ 2]] :a0)
    ([[1 1 3]] :a1)
    ([[1 2 3]] :a2))
  [1 2 3])
;; :a2

(letfun [(test3 ([[_ _ 2]] :a0)
                    ([[1 1 3]] :a1)
                    ([[1 2 3]] :a2))]
  (test3 [1 2 3]))
;; :a2
```



## Criterium benchmarking

Uses the above function `accum` compared with a normal clojure function:

``` clj
(require '[criterium.core :refer [bench]])

(defn accum-defn
    ([n] (accum-defn 0 n))
    ([ret n] (if (= n 0) ret (recur (+ n ret) (dec n)))))

(defun accum-defun
  ([0 ret] ret)
  ([n ret] (recur (dec n) (+ n ret)))
  ([n] (recur n 0)))

(bench (accum-defn 10000))
;;Evaluation count : 210480 in 60 samples of 3508 calls.
;;             Execution time mean : 281.095682 µs
;;    Execution time std-deviation : 2.526939 µs
;;   Execution time lower quantile : 277.691624 µs ( 2.5%)
;;   Execution time upper quantile : 286.618249 µs (97.5%)
;;                   Overhead used : 1.648269 ns

(bench (accum-defun 10000))
;;Evaluation count : 26820 in 60 samples of 447 calls.
;;             Execution time mean : 2.253477 ms
;;    Execution time std-deviation : 13.082041 µs
;;   Execution time lower quantile : 2.235795 ms ( 2.5%)
;;   Execution time upper quantile : 2.281963 ms (97.5%)
;;                   Overhead used : 1.648269 ns
```

accum-defn is much faster than accum-defun. Pattern matching does have a tradeoff.


