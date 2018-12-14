(define-library (thunknyc i7t)
  (export
   ;; meta procs
   parse-i7t translate-i7t read-file-i7t expand-file-i7t load-i7t

   ;; support procs
   i7t-comparator make-applicable *-ref *-length *-drop

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
