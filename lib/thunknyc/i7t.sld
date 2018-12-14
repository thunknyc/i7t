(define-library (thunknyc i7t)
  (export
   ;; meta procs
   parse-i7t translate-i7t read-file-i7t expand-file-i7t load-i7t eval-i7t

   ;; support procs and values
   keyword keyword? string->keyword symbol->keyword
   keyword->string keyword->symbol

   nil nil? i7t-comparator *-ref *-length *-drop

   make-applicable

   ;; handy procs
   inc dec

   ;; re-exports
   match-lambda*
   test test-begin test-end)

  (import (scheme red)

        (only (chibi ast) analyze ast->sexp optimize)
        (chibi io) (chibi match)
        (chibi parse) (chibi show) (chibi test)

        (srfi 111) (srfi 113) (srfi 128))

  (include "i7t.scm"))
