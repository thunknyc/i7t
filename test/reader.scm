(import (scheme red)
        (chibi test)
        (thunknyc i7t reader))

(define-syntax test-i7t
  (syntax-rules ()
    ((_ expected i7t-string)
     (test expected (parse-i7t i7t-string)))
    ((_ equal expected i7t-string)
     (test-equal equal expected (parse-i7t i7t-string)))))

(test-begin "Testing I7t reader")
(test-i7t '|.| ".")
(test-i7t '|..| "..")
(test-i7t '|...| "...")
(test-i7t '|..| "..")
(test-i7t '|.1| ".1")                   ; A symbol, not a number in EDN/Clojure
(test-i7t '|+.1| "+.1")                 ; Ditto
(test-i7t '|->| "->")
(test-i7t 1/2 "1/2")                    ; Support ratios
(test-i7t 3+4i "+3+4i")                 ; And complex numbers
(test-i7t 3+4i "3+4i")
(test-i7t 42 "42")
(test-i7t 42.3 "42.3")
(test-i7t 'e20 "e20")
(test-i7t (i7tvector) "[]")
(test-i7t (i7tvector 0 1 2 3 42) "[0 1 2 3 42]")
(test-i7t (i7tlist) "()")
(test-i7t (i7tlist 0 1 2 3 42) "(0 1 2 3 42)")
(test-i7t (i7tlist i7ttrue i7tfalse i7tnil) "#_blorg (true #_0.32 false nil) #_:->")
(test-i7t (i7tset 0 1 2 42) "#{0 1 2 42}")
(test-i7t (i7tmap "foo" 0 "bar" 42) "{\"foo\" 0 \"bar\" 42}")
(test-i7t (i7tkw "foo") ":foo")
(test-i7t 42 "#_:foo 42")
(test-i7t (i7tlist 1 2 3 (i7tlist 4 5 6)) "(1 2 #_true #_() 3 (4 #_\"blah\" 5 6))")
(test-i7t (i7tlambda '+ '%1 1) "#(+ %1 1)")
(test-end) (if #f #f)
