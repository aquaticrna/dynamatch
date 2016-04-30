
# Dynamatch

Betwixt pattern matching and the multimethod

<br/>


## Problem Statement

What is the simplest way to construct a mapping from `data` to `other-data` (possibly with side effects) such that we can dynamically extend the mapping to respond to some specified class of input data in some way?
In particular, how do we do this while maximizing flexibility in how we describe these classes of data, while maintaining semantic clarity with respect to building predictable, modular systems?

This library is an attempt to solve this problem through a dynamically dispatched pattern matching functions.
Feel free to skip to [Usage](#usage).


### Clojure offerings: multimethods and protocols

Clojure offers us multimethods and protocols for doing extensible dynamic dispatch.
But these unfortunately fall short of our goal.

With multimethods, we can specify some function to compute a value on which to dispatch.
However, the problem here is that if we didn't create the multimethod, we have no sane ability to extend the semantics of the dispatch function.
However it dispatches, we have to live with it.

With protocols, we have the ability to define and consume abstract APIs for some set of behavior (protocols) and build implementations of these protocols for our own or existing types.
This is very powerful and meets a lot of our goals to some extent.
However, the problem is that we are limited to data for which these protocols have been implemented: records, types or reified objects.
This means we can't just throw any 'ol data at such a function; only data which has been wedged into one of the reificiations.
This falls short of our flexibility and extensibility; we'd like to extend functionality to _any_ data.


### Pattern matching

Programming languages such as Haskell, ML, Erlang & Elixer have been built around pattern matching (to various degrees).
The way in which they've built around pattern matching has greatly affected the style in which people tend to program in these languages.
In particular, in these languages we often find functions defined based on pattern matching of the structure of their arguments.
For an interesting talk about pattern matching in Clojure (as well as a history of how pattern matching evolved more generally), I'd direct you to [Pattern Matching in Clojure](https://www.youtube.com/watch?v=n7aE6k8o_BU), by Sean Johnson.

Pattern matching solves part of our problem here.
It particularly nails the problem of a flexible descriptive language around the class of data which should get matched.

However, functions defined via pattern matching generally can not be extended outside of the context in which they are created (to my knowledge; please correct me if I'm wrong).
This means they can't be used (as multimethods or protocols are) as a basis on which users of (e.g.) a library add to the behavior or capability of a system.


### Extending pattern matching functions

What if we could do this?
What if we could _add_ matches dynamically to a function which dispatches on pattern matches?

There is one major problem with this as I see it: clause order.
Modularity is predicated on being able to break a problem into sub-problems, and think of them as more or less independent.
This is difficult when order of execution matters in a non-trivial way.

Multimethods are more or less independent of the order in which methods are defined [see aside].
This is a part of what makes them so useful for defining extensible and modular systems.
By not having to worry about order, users of an API can generally extend a multimethod without having to worry about how it will affect the rest of the system.
That sort of an awkward thing to say, since it's really the fact that multimethods will only ever fire a default or a .
When you define a multimethod, you generally either expand the class of data on which the multimethod operates, or change the behavior from the default for some class of data.
As such, the order in which multimethods are added has no effect.
It's an interesting question to ask what properties are necessary and sufficient for this kind of modularity.
But it is certainly the case that if the order in which extensions are defined affects the behavior of the system, modularity must inherently be effected adversely, since you can't reason about the subsystems without considering their ordering within the greater system.

[aside: Keyword inheritance softens this statement, but only somewhat, since in general the semantics of what's happening are still clear]

However, multimethods still suffer from this problem, which is that you cannot extend the semantics of _how data is dispatched_.
And this tradeoff sort of makes sense.
For order not to matter, there have to be restrictions on the semantics of the manner in which we define classes.


### Partitionability

What we need is _partitionability_.

A function naturally partitions a class of data into subclasses based on the return values.
This partitionability is related to the order independence these semantics give us.

With _fully static_ pattern matching, statics remove some of the burden of thinking about order; it's static.
As long as you're looking at some static description of matches, and can statically reason about partitioning based on static ordering specifications.
But what we want are dynamics; because we're in a dynamic language (in particular a LISP) and because we want extensibility as a modular system API, this won't cut it for us.

So how can we get partitionability, reasonability, predictability and modularity?
Perhaps via some layering of hierarchical structure which takes care of a top level partitioning?
How do you maintain flexilibity here?
Can we extend the notion of the protocol to itself be more dynamic and flexible in a way which ensures semantic clarity?
I don't know; right now it's just an idea worth exploring.


### A practical and motivating example

Right now I'm working on _datview_, a pattern for describing UI as Datomic data.

The idea behind datview is that we should be able to add metadata to our Datomic database schema which instructs the semi-automated construction of UI from this data.
By describing things like which entities should relate to which entities via what relationships, we can dynamically generate (e.g.) forms based on this data, together with the raw shape of the data as it exists.

The problem and blessing is that Datomic data is extremely extensible, flexible and open.
There _are no_ entity types.
And entities can have _whatever attributes_ we give them.
While this enables a great deal of flexibility, it also provides us with a challenge matching the problem statement above.

How can datview describe mappings from Datomic data to UI DOM (hiccup data, whatevs) flexibly, so that these tools aren't just disposable scaffolding, but a truly flexible, modular and extensible rendering framework that can span the life of an application?

This is the problem I'm trying to solve with Dynamatch.

<br/>


## Current solution

This library is an attempt to solve this problem with pattern matching, while considering how we can mitigate the semantic challenges this imposes on the systems we describe in this manner.

We present the following features:

* `defun`: Create a var referencing a `MatchFn` function object defined by pattern matching dispatch a la the `defun` library.
* `addmatches!` and `addmatch!`: Add matches to an existing `MatchFn`, with semantics for adding to the beginning, or end of the match/clause matrix.
* `update-clauses`: Low level ability to atomically update the match/clause matrix via an arbitrary update function mapping the existing clause matrix to some new clause matrix.
* `merge-funs`: Take two `MatchFn` functions and merge their clauses.

To deal with the semantic complexity arising from clause-order dependence, we specifically have the following:

* Soon: Match clauses (which take the form `([& pattern] & forms)`) can accept metadata with particular note being paid to the `:ident` attribute, which can be used to refer to a specific clause programmatically.
* Soon: Re-valuation of impure macros `defun`, `addmatches` and `addmatch!` update in-place the corresponding clauses (by `:ident`).
* Soon: Blocks of matches added via `addmatches` should "stick together" in the compiled matrix, meaning that once we've added a series of addmatches pattern submatrices, those submatrices will always and forever be contiguous, and can be internally reordered.

Together these features mean that once we perform the initial compilation of the order of the addmatches submatrices, we are more or less immune to ordering problems.

<br/>


## Is this a good solution?

I don't know.
Maybe not.
But I _do_ think it's a good _problem_.
Please share your thoughts and critiques.

This is the best I've been able to come up with.
I challenge you to come up with a better solution to the problem :-)

What we have here solves most of the problems

<br/>


## Current status

Clojurescript support is also currently broken till I fix the macro layout to be cljs compatible.
We also haven't yet implemented the matchset or clause idents and corresponding in place updates.

<br/>


## Usage

For detailed help with the pattern matching, please refer to the [core.match overview](https://github.com/clojure/core.match/wiki/Overview).
The README for [defun](https://github.com/killme2008/defun) may also be of value, as this library is a fork thereof.
Dynamatch supports all of the defun API, except for `defn-` and `letfn` (but we plan to implement these soon), so you should be able to use dynamatch as a near- drop-in replacement.

### Defining functions

more coming soon...

<br/>


## Ideas for the future

### `overmatch`

Re-define a pattern match by turning it into it's own pattern match clause matrix?
What about redefine semantics of the form that initially created the ident though?
Maybe this doesn't make sense... but seems there's something here.

### `matchbind`

It should be possible to create a lexical scope where a dynamic matchfn has been overridden.
This would make it possible to do specialized scope where a certain class of data behaves in a certain customized way.

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


