
# Dynamatch?

This is a (WIP) fork of the `defun` Clojure(script) library designed to allow for multimethod style extension of match functions.
The `defun` macro should work more or less the same as the original `defun` (or eventually should anyway).
Additionally, there is a `addmatch` macro that lets you add matches to the front of the match stack.
Eventually, it should be possible to add at the end, and ideally anywhere in the stack.


## The big challenge

This system of extensible pattern matching has some pretty major issues.
The semantics of multimethod dispatch are always quite clear and well defined.
Pattern matching, however, depends on clause order.
This is generally fine for co-located definitions, but as soon as you try to pull them apart and make things extensible, you open up a can of worms.
The order in which you call `addmatch` can affect the resulting function you're trying to define.
And the semantics of how you'd add the equivalent of a "default" match are unclear, since there might already be a "catch all" somewhere in the match stack.

I think some of this problem can be solved by adding keyword identities to match clauses.
This would ensure that once a clause had been defined for a given keyword identity, any changes to it would be made in place, not at the top of the clause stack.
This avoids some of the issues of what order things load in and how you'd override/update an existing clause in the stack.
However, a lot of problems still remain, and I don't think this is functionality that will ever be as general and extensible as multimethod dispatch.
It will likely be most tenable when you expect that a single consumer in a single namespace will be overriding some predefined match funs.


## Some other things:

This is pretty inefficient right now, as we're compiling the match form and evaluating it on every function call.

Also recur will likely not work as expected; hopefully we can get that eventually.


## That's all

Will try to add more later, but for now, below is original README.

Enjoy,

Chris Small


</br>

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

## Contributors

Thanks .

- [kgann](https://github.com/kgann)
- [danielcompton](https://github.com/danielcompton)
- [Sander Dijkhuis](https://github.com/sander)

## License

Copyright © 2014 [Dennis Zhuang](mailto:killme2008@gmail.com)

Distributed under the Eclipse Public License either version 1.0 or (at

your option) any later version.
