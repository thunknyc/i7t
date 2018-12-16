(define-library (thunknyc i7t reader)
  (export parse-i7t read-file-i7t

          i7ttagged
          i7tvector i7tlist i7tmap i7tkw i7tset i7tlambda i7tnil
          i7ttrue i7tfalse)

  (import (scheme red) (chibi parse))
  (include "reader.scm"))
