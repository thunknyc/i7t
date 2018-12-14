# Incrément aka I7t

Incrément (henceforth referred to as I7t) is a Scheme that supports
and embraces an S-expression reader based on Clojure's (and also
therefore EDN's) syntax. It is hosted atop Alex Shinn's [Chibi
Scheme](https://github.com/ashinn/chibi-scheme).

Pull requests and issue submissions are welcomed.

## Using I7t

Clone this repository and cd to its root directory. Run
`chibi-scheme`. Evaluate `(import (scheme red) (thunknyc i7t))`. The
following procedures of interest will now be available to you:

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

## Goals of I7t

* Implement a reader that can be used as the basis of language development
* Design and implement a series of special forms that allow Scheme to be
  written with the benefits of the enhanced reader.
* Strongly embrace R7RS and bake Scheme Red (and Tangerine and Orange and...)
  types into the langue.
* Prefer immutability wherever possible
* Provide a protocol-like facility (and use it pervasively e.g. by
  supporting application of any object that supports a hypothetical
  `<appliable>` protocol.
* Maintain compatibility with Scheme code i.e. Scheme->I7t and I7t->Scheme
  procedure applications should Just Work.

## Currently supported special forms

* `Define-proc` for single and multiple arities with nested vector
  dereferencing, `& rest-arguments` and `:as all-arguments` support
* `Define`
* `Quote` (and traditional `'foo` syntax)
* `Test`

* Procedure application allows strings, lists, vectors, and hash tables
  to be applied
* `#([args ...] ...)` lambda form for single and multiple arities

## Notable missing features

* `And`, `or`, etc.
* `If` and `cond`
* `Let`
* Quasi-quoting

## A sample source file

The file `test.i7t`:

```
(define v1 '[foo bar (snafu blorg)])
(define beverages '{scotch laphroig rye bulleit
                    vodka no-thanks beer pilsner
                    water tap wine (bordeaux red)})

(define-proc add
  ([] 0)
  ([a] a)
  ([a b] (+ a b))
  ([a b c] (+ a b c))
  ([a b c d & e] (apply + a b c d e)))

(define-proc sine
  [theta] (sin theta))

(define-proc inc-all [xs]
  (map #([[x] & rest :as args]
         (+ x 1))
       xs (iota 100)))

(define-proc pick [col & offsets]
  (map #([i] (col i)) offsets))

(define-proc drink-ingredients [{:keys [scotch water]}]
  (list scotch water))

(define-proc drink-ingredients* [{s 'scotch w 'water :as bevs}]
  (list s w 'bevs-length (*-length bevs)))

(test-begin)
(test '(1 2 3 4) (inc-all '([0] [1] [2] [3])))
(test '(laphroig tap (bordeaux red)) (pick beverages 'scotch 'water 'wine))
(test '((snafu blorg)) (pick v1 2))
(test '(laphroig tap) (drink-ingredients beverages))
(test '(laphroig tap bevs-length 6) (drink-ingredients* beverages))
(test-end)
```

## Why should you care?

There are many things to like about Clojure, and none of them are
inherently incompatible with inclusion in Scheme. Among these features
are its protocol orientation and its heavy bias toward
immutability. The Scheme community has recognized the value of these
features and has worked -- and continues to work -- to incorporate
these features into the Scheme ecosystem.

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

## Licensing

MIT License.