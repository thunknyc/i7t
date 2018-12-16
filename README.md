# Incrément aka I7t

Incrément (henceforth referred to as I7t) is a Scheme that supports
and embraces an S-expression reader based on Clojure's (and also
therefore EDN's) syntax. It is hosted atop Alex Shinn's [Chibi
Scheme](https://github.com/ashinn/chibi-scheme).

Among other things, I7n provides a protocol facility and uses it
pervasively e.g. by supporting application of any object that supports
the `core.appliable` / `apply` method or finding the length of any
collection which supports the `core.col` / `length` method via the
`len` procedure.

Aside from the semantics of application described above, I7t's
semantics are identical to Scheme. I7t is compiled into Scheme, and
the translated code preserves the position of tail expressions in the
resulting Scheme, so, unlike Clojure, I7t features TCO. (For this
reason I7t lacks the `recur` form.)

Pull requests and issue submissions are welcomed.

## Using I7t

Clone this repository and cd to its root directory. Run
`chibi-scheme`. Evaluate `(load "i7t.scm")`. The following procedures
of interest will now be available to you:

* `parse-i7t source [index]`: Parse and return a single I7t object
  from the source (a string or `(chibi parse)` parse stream) at
  `index` or at the beginning if not specified.
* `translate-i7t form`: Translate the I7t object into a Scheme
  S-expression suitable for evaluation.
* `read-file-i7t filename`: Exhaustively parse file with name
  `filename` and return a list of I7t objects.
* `expand-file-i7t filename`: Read I7t all expressions in `filename`
  and return a list of translated S-expressions.
* `load-i7t filename`: Expand `filename` and evaluate each translated
  S-expression.
* `eval-i7t string`: Parse, translate, and evaluate `string`
  containing an I7t form.

At the Chibi REPL you can evaluate I7t like this:

```
> (eval-i7t "(loop iter [i 0] (if (> i 10) i (iter (inc i))))")
11
> ((eval-i7t "#([& xs] (apply + xs))") 1 2 3 4)
10
> (eval-i7t "(define-proc sum-of-squares [a b] (+ (* a a) (* b b)))")
> (sum-of-squares 3 4)
25
>
```

Or you can load and evalute files like this:

```
> (load-i7t "test/core.i7t")
Testing I7t language: ............
12 out of 12 (100.0%) tests passed in 0.04197406768798828 seconds.
>
```

Have fun!

## Goals of I7t

One goal of I7t is to implement a reader that can be used as the basis
of data parsing or language development. The reader is available as
library `(thunknyc i7t reader)`. If the reader fails to read a valid
Clojure source file please open an issue.

Another goal is to design and implement a language atop that
reader. Subgoals of this effort:

* Embrace the increased expressive power of EDN to implement language forms that increase the concision, expressiveness, and readability of written code
* Strongly embrace R7RS and bake in type from the Red (and Tangerine and Orange and...)
  standardization processes
* Presume immutability
* Maintain compatibility with Scheme code i.e. Scheme->I7t and I7t->Scheme
  procedure applications should Just Work

## Currently supported special forms

* `And`, `or`, `if`, `cond`
* `Let`, behaving as Scheme's `let*`
* `Loop`, behaving as Schemes named-`let`
* `Define-proc` for single and multiple arities with nested vector
  dereferencing, `& rest-arguments` and `:as all-arguments` support
* `Define`
* `Quote` (and traditional `'foo` syntax)
* `Test` from `(chibi test)`
* Procedure application allows strings, lists, vectors, sets, and hash
  tables to be applied
* `#([args ...] ...)` lambda form for single and multiple arities

## Notable missing features

* Quasi-quoting
* Serialization via `Edn-str' does not support most types at the moment

## New types

* `<nil>`: The symbol `nil` represents the nil object and `nil?` tests
  for it.
* `<keyword>`: A variety of procedures construct, test, or convert
  keyword objects: `keyword`, `keyword?`, `keyword-name`,
  `keyword->string`, `keyword->symbol`, `string->keyword`,
  `symbol->keyword`.

## Why should you care?

There are many things to like about Clojure, and none of them are
inherently incompatible with inclusion in Scheme<sup
id="a0">[0](#f0)</sup>. Among these features are its protocol
orientation and its heavy bias toward immutability. The Scheme
community has recognized the value of these features and has worked --
and continues to work -- to incorporate these features into the Scheme
ecosystem.

The issue of Clojure's reader, however, attracts less enthusiasm
within the community. I think this is mistaken. There are at least two
kinds of improvements that Clojure's reader delivers.

The first kind is self-evident. The reader supports among other things
keyword, set, and map literals. I have witnessed the benefits of the
Clojure reader pooh-poohed based on the observation that none of these
improvements, if incorporated into Scheme, would fundamentally
increase the expressiveness of the language.

The second kind of improvement, however, is the one I believe is far
more significant. One of the features of Lisp that writers note when
discussing the language is that it is _homoiconic_, meaning that Lisp
programs are written such that they are straightforwardly constructed
from Lisp data structure literals and can be manipulated as such.

Clojure is a Lisp in this respect, but the syntax of the language does
not merely allow hash tables and vectors to be read as literal data:
the syntax embraces its core data structures and the creation of a much
richer language.

Two areas where this embrace shines are in the forms for defining
procedures with multiple arities and in its destructuring binding
operations. Because of the power and concision and pervasive
availability of these forms, especially destructuring in the context
of procedure argument declarations and in let and other binding forms,
most Clojure source files productively make use of these language
features.

As another example, consider the `lambda` form. In Clojure, the lambda
form (named `fn`) allows the function to be named by writing e.g. `(fn
loop [arg1 arg2] ...)`. It would be a useful to be able to do this in
Scheme as well, but a cascade of design decisions and limitations
makes this feature impossible. A transliteration of this form into
Scheme yields `(lambda loop #(arg1 arg2) ...)`, which is meaningful: a
list of the procedure's arguments are bound to `loop` and then `#(arg1
arg2)` is evaluated. Clojure's way of doing this is to write `(fn [&
args] ...)`, which allows the original Clojure form above to be
recognized as a name procedure definition. Doing this in Scheme would
require adding parenthses or supporting `(lambda (. rest) ...)`, which
is nonsensical in Scheme. The sad part of this is that thanks to
Scheme's support for tail call optimization, this facility would be
far more useful in Scheme than it is in Clojure, which sets up the
lambda as the target of enclosed `recur` calls.

## Licensing

MIT License.

## Footnote

0. <a name="f0">Aside from JVM support, which, according to Taylor
Campbell, is _the_ killer Clojure feature.</a> [↩](#a0)
