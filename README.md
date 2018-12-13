# Incrément aka I7t: A Scheme based on an enhanced EDN-like reader

## Introduction, or why should you care?

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
the syntax embraces its core data structures and the create of a much
richer language.

Two areas where this embrace shines are in the forms for defining
procedures with multiple arities and in its destructuring binding
operations. Because of the power and concision and pervasive
availability of these forms, especially destructuring in the context
of procedure argument declarations and in let and other binding forms,
most Clojure source files productively make use of these language
features.

## Goals of I7t

* Implement a reader that can be used as the basis of language development
* Design and implement a series of special forms that allow Scheme to be
  written with the benefits of the enhanced reader.
* Maintain compatibility with Scheme code i.e. Scheme->I7t and I7t->Scheme
  procedure applications should Just Work.

## Licensing

MIT License.