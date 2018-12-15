(define-library (thunknyc i7t reader)
  (export parse-i7t read-file-i7t)
  (import (scheme red) (chibi parse))
  (include "reader.scm"))
