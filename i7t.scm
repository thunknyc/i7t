;; Incrément: A Scheme based on an enhanced EDN-like reader
;;
;; Edwin Watkeys, Thunk NYC Corp.
;; <edw@poseur.com>
;; 13 December 2018
;; http://github.com/thunknyc/i7n
;;

(import (scheme red)

        (chibi ast) (chibi io) (chibi match)
        (chibi parse) (chibi show) (chibi test)

        (srfi 99 records inspection)
        (srfi 111) (srfi 113) (srfi 128))

(define (debug message x)
  (show #t message x nl)
  x)

(define i7t-comparator (make-default-comparator))

(define sym-char-set
  (let ((cs (char-set-delete char-set:graphic
                             #\# #\( #\) #\[ #\] #\{ #\} #\' #\` #\,)))
    cs))

(define post-sign-sym-char-set
  (let ((cs (char-set-delete sym-char-set
                             #\0 #\1 #\2 #\3 #\4 #\5
                             #\6 #\7 #\8 #\9 #\.)))
    cs))

(define first-sym-char-set
  (let ((cs (char-set-delete sym-char-set #\+ #\-
                             #\0 #\1 #\2 #\3 #\4 #\5
                             #\6 #\7 #\8 #\9 #\.)))
    cs))

(define (in-char-set? cs)
  (lambda (ch) (char-set-contains? cs ch)))

(define i7ttrue
  (list '__TRUE))

(define i7tfalse
  (list '__FALSE))

(define i7tnil
  (list '__NIL))

(define (i7tlambda . o)
  (apply list '__LAMBDA o))

(define (i7tmap . o)
  (apply list '__MAP o))

(define (i7tvector . o)
  (apply list '__VEC o))

(define (i7tset . o)
  (apply list '__SET o))

(define (i7tlist . o)
  (apply list '__LIST o))

(define (i7tkw . o)
  (apply list '__KW o))

(define-grammar i7t

  (i7t-discardable ((: "#_" ,i7t-space ,i7t-datum ,i7t-space)))

  (i7t-space ((* (or ,(parse-char char-whitespace?) #\, ,i7t-discardable))))

  (i7t-num ((or (-> n (: ,(parse-char char-numeric?)
                         (* (or ,(parse-char char-numeric?)
                                #\. #\- #\+ #\e #\E))))
                (-> n (: (or  #\. #\- #\+)
                         (+ (or ,(parse-char char-numeric?)
                                #\. #\- #\+ #\e #\E)))))
            (string->number (list->string (apply cons n)))))

  (i7t-quoted
   ((: #\' (-> d ,i7t-datum))
    (i7tlist 'quote d)))

  (i7t-keyword
   ((: #\: (-> s (+ ,(parse-char sym-char-set))))
    (i7tkw (apply string s))))

  (i7t-sym
   ((-> s (: (or #\+ #\-)
             (+ ,(parse-char post-sign-sym-char-set))
             (* ,(parse-char sym-char-set))))
    (apply string (car s) (concatenate (cdr s))))
   ((-> s (or #\+ #\-)) (string s))
   ((-> s (: ,(parse-char first-sym-char-set)
             (* ,(parse-char sym-char-set))))
    (apply string (car s) (cadr s))))

  (i7t-str ((: ,(parse-char #\")
               (-> s (* ,(parse-not-char #\")))
               ,(parse-char #\"))
            (list->string s)))

  (i7t-seq-el ((: ,i7t-space (-> el ,i7t-datum)) el))

  (i7t-lambda ((: "#(" ,i7t-space (-> el ,i7t-datum) ,i7t-space
                  (-> els (* ,i7t-seq-el)) ,i7t-space ")")
               (apply i7tlambda (cons el els)))
              ((: "#(" ,i7t-space ")") (i7tlambda)))

  (i7t-set ((: "#{" ,i7t-space (-> el ,i7t-datum) ,i7t-space
               (-> els (* ,i7t-seq-el)) ,i7t-space "}")
            (apply i7tset el els))
           ((: "#{" ,i7t-space "}") (i7tset)))

  (i7t-vec ((: "[" ,i7t-space (-> el ,i7t-datum) ,i7t-space
               (-> els (* ,i7t-seq-el)) ,i7t-space "]")
            (apply i7tvector el els))
           ((: "[" ,i7t-space "]") (i7tvector)))

  (i7t-list ((: "(" ,i7t-space (-> el ,i7t-datum) ,i7t-space
                (-> els (* ,i7t-seq-el)) ,i7t-space ")")
             (apply i7tlist el els))
            ((: "(" ,i7t-space ")") (i7tlist)))

  (i7t-map-el ((: ,i7t-space (-> k ,i7t-datum) ,i7t-space
                  ,i7t-space (-> v ,i7t-datum)) (list k v)))

  (i7t-map ((: "{" ,i7t-space (-> k ,i7t-datum) ,i7t-space
               ,i7t-space (-> v ,i7t-datum) ,i7t-space
               (-> els (* ,i7t-map-el)) ,i7t-space "}")
            (apply i7tmap k v (concatenate els)))
           ((: "{" ,i7t-space "}") (i7tmap)))

  (i7t-atom ("true" i7ttrue)
            ("false" i7tfalse)
            ("nil" i7tnil)
            ((-> k ,i7t-keyword) k)
            ((-> n ,i7t-num) n)
            ((-> s ,i7t-str) s)
            ((-> s ,i7t-sym) (string->symbol s)))

  (i7t-datum ((or ,i7t-quoted ,i7t-atom
                  ,i7t-vec ,i7t-list ,i7t-map
                  ,i7t-set ,i7t-sym ,i7t-lambda)))

  (i7t-object ((: ,i7t-space (-> o ,i7t-datum) ,i7t-space) o)))

(define (parse-i7t source . o)
  (let ((index (if (pair? o) (car o) 0)))
    (parse i7t-object source index)))

(define-syntax test-i7t
  (syntax-rules ()
    ((_ expected i7t-string)
     (test expected (parse-i7t i7t-string)))
    ((_ equal expected i7t-string)
     (test-equal equal expected (parse-i7t i7t-string)))))

(define (run-tests)
  (test-begin)
  (test-i7t 42 "42")
  (test-i7t 42.3 "42.3")
  (test-i7t 'e20 "e20")
  (test-i7t (i7tvector) "[]")
  (test-i7t (i7tvector 0 1 2 3 42) "[0 1 2 3 42]")
  (test-i7t (i7tlist) "()")
  (test-i7t (i7tlist 0 1 2 3 42) "(0 1 2 3 42)")
  (test-i7t (i7tlist #t #f i7tnil) "#_blorg (true #_0.32 false nil) #_:->")
  (test-i7t (i7tset 0 1 2 42) "#{0 1 2 42}")
  (test-i7t (i7tmap "foo" 0 "bar" 42) "{\"foo\" 0 \"bar\" 42}")
  (test-i7t (i7tkw "foo") ":foo")
  (test-i7t 42 "#_:foo 42")
  (test-i7t (i7tlist 1 2 3 (i7tlist 4 5 6)) "(1 2 #_true #_() 3 (4 #_\"blah\" 5 6))")
  (test-i7t (i7tlambda + %1 1) "#(+ %1 1)")
  (test-end) (if #f #f))

(define (nil? x) (equal? nil))

(define (vector-drop v index)
  (let ((n (vector-length v)))
    (let loop ((i 0) (xs '()))
      (cond
       ((>= i n) (reverse-list->vector xs))
       ((< i index) (loop (+ i 1) xs))
       (else (loop (+ i 1) (cons (vector-ref v i) xs)))))))

(define (vector-split-at v index)
  (let ((n (vector-length v)))
    (let loop ((i 0) (xs '()) (ys '()))
      (cond
       ((>= i n) (values (reverse-list->vector xs) (reverse-list->vector ys)))
       ((< i index) (loop (+ i 1) (cons (vector-ref v i) xs) ys))
       (else (loop (+ i 1) xs (cons (vector-ref v i) ys)))))))

(define (*-ref col i . o)
  (let ((default (if (pair? o) (car o) nil)))
    (cond ((string? col)
           (let ((n (string-length col)))
             (cond ((and (> i -1) (< i n))
                    (string-ref col i))
                   (else default))))
          ((list? col)
           (let ((n (length col)))
             (cond ((and (> i -1) (< i n))
                    (list-ref col i))
                   (else default))))
          ((vector? col)
           (let ((n (vector-length col)))
             (cond ((and (> i -1) (< i n))
                    (vector-ref col i))
                   (else default))))
          ((hash-table? col)
           (hash-table-ref/default col i o))
          ((set? col)
           (cond ((set-contains? col i) i)
                 (else nil)))
         (else (error (show #f "Unknown refferable " col))))))

(define (*-drop col i)
  (cond ((list? col) (drop col i))
        ((vector? col) (vector-drop col i))
        (else (error (show #f "Unknown sequential " col))))  )

(define (*-length col)
  (cond ((list? col) (length col))
        ((vector? col) (vector-length col))
        ((hash-table? col) (hash-table-size col))
        (else (error (show #f "Unknown collection " col)))))

(define (*-empty? col)
  (= (*-length col) 0))

(define (seq-accessor a i) `(*-ref ,a ,i))
(define (rest-accessor a skip) `(*-drop ,a ,skip))

(define (destructure accessor var bindings)
  (match var
         ('_ bindings)
         ((? symbol? x)
          (cons `(,x ,accessor) bindings))
         (('__VEC as ...)
          (make-bindings as '_ '_ accessor bindings))))

(define (make-args n)
  (map (lambda (i) (string->symbol (show #f "__arg" i))) (iota n)))

(define (make-bindings arg-names rest-name as-name parent-accessor bindings)
  (let* ((n (*-length arg-names))
         (arg-accessors (map (lambda (i) (seq-accessor parent-accessor i))
                             (iota (+ n 1))))
         (arg-bindings (fold destructure bindings arg-accessors arg-names))
         (rest-binding (destructure (rest-accessor parent-accessor n)
                                    rest-name arg-bindings))
         (as-bindings (destructure parent-accessor as-name rest-binding)))
    `(,@as-bindings)))

(define (build-lambda arg-names rest-name as-name body)
  (let* ((root-accessor (if (equal? as-name '_) '__args as-name))
         (bindings (make-bindings arg-names rest-name '_ root-accessor '())))
    (cond ((*-empty? bindings)
           `(lambda () (let ((,root-accessor '())) ,@body)))
          (else
           `(lambda ,root-accessor (let (,@bindings) ,@body))))))

(define (optimize-sexp sexp)
  (ast->sexp (optimize (analyze sexp))))

(define (lambda-clause args expr exprs)
  (let ((body (cons (translate-i7t expr) (map translate-i7t exprs))))
    (match args

           (('& rest)
            `(,rest (apply ,(build-lambda '() rest '_ body) rest)))

           ((a1 ... '& rest ('__KW "as") as)
            `(,as (apply ,(build-lambda a1 rest as body) ,as)))

           ((a1 ... ('__KW "as") as)
            (let ((arg-names (make-args (*-length a1))))
             `(,arg-names (,(build-lambda a1 '_ as body) ,@arg-names))))

           ((a1 ... '& rest)
            `(__args (apply ,(build-lambda a1 rest '__args body) __args)))

           ((a1 ...)
            (let ((arg-names (make-args (*-length a1))))
             `(,arg-names (,(build-lambda a1 '_ '_ body) ,@arg-names))))

           (()
            `(() ((build-lambda '() '_ '_ body)))))))

(define (make-applicable procish)
  (cond ((procedure? procish) procish)
        ((or (string? procish) (vector? procish)
             (list? procish) (hash-table? procish)
             (set? procish))
         (lambda args (apply *-ref procish args)))
        (else (error (show #f "Non-applicable object " (written procish))))))

(define i7t-quote-level (make-parameter 0))

(define-syntax enquoted
  (syntax-rules ()
    ((_ e1 e2 ...)
     (parameterize ((i7t-quote-level (+ (i7t-quote-level) 1)))
       e1 e2 ...))))

(define (should-quote?)
  (> (i7t-quote-level) 0))

(define-record-type <nil> (construct-nil) nil?)
(define nil (construct-nil))

(define (translate-i7t form)
  (match form

         (('__LIST 'define name value)
          `(define ,name ,(translate-i7t value)))

         (('__LIST 'define-proc name
                    ('__LIST ('__VEC a1s ...) e1s e2s ...) ...)
          (let* ((as-name '__args)
                 (n (*-length a1s))
                 (placeholder-arg-names (make-args n)))
            `(define ,name (match-lambda* ,@(map lambda-clause a1s e1s e2s)))))

         (('__LIST 'define-proc name ('__VEC a1 ...) e1 e2 ...)
          `(define ,name (lambda ,@(lambda-clause a1 e1 e2))))

         (('__LAMBDA ('__LIST ('__VEC a1s ...) e1s e2s ...) ...)
          `(match-lambda* ,@(map lambda-clause a1s e1s e2s)))

         (('__LAMBDA ('__VEC a1 ...) e1 e2 ...)
          `(lambda ,@(lambda-clause a1 e1 e2)))

         (('__LIST 'quote a)
          (enquoted (translate-i7t a)))

         (('__LIST proc a1 ...)
          (if (should-quote?)
              `(list ,(translate-i7t proc) ,@(map translate-i7t a1))
              `((make-applicable ,(translate-i7t proc)) ,@(map translate-i7t a1))))

         (('__VEC e1 ...)
          `(vector ,@(map translate-i7t e1)))

         (('__MAP e1 ...)
          `(let ((m (make-hash-table i7t-comparator)))
             (hash-table-set! m ,@(map translate-i7t e1))
             m))

         ((? symbol? sym) (if (should-quote?) `(quote  ,sym) sym))
         ((? number? num) num)
         ((? string? str) str)

         (('__TRUE) #t)
         (('__FALSE) #f)
         (('__NIL) nil)
         (() '())

         (#f (error "Parse error"))
         (x (error (show #f "Unknown object " (written x))))))

(define (read-file-i7t filename)
  (let* ((stream (file->parse-stream filename))
         (i7t-exprs (parse-fold i7t-object cons '() stream 0)))
    (reverse! i7t-exprs)))

(define (expand-file-i7t filename)
  (map translate-i7t (read-file-i7t filename)))

(define (load-i7t filename)
  (let ((exprs (expand-file-i7t filename)))
    (for-each (lambda (expr) (eval expr)) exprs)))

(define inc-all
  (lambda #0=(__arg0)
          ((lambda __args
             (let ((xs (*-ref __args 0)))
               ((make-applicable map)
                (lambda args
                  (apply (lambda args
                           (let ((rest (*-drop args 1))
                                 (x (*-ref (*-ref args 0) 0)))
                             ((make-applicable show) #t
                              "rest args: " rest ", all args: " args nl)
                             ((make-applicable +) x 1)))
                         args))
                xs
                ((make-applicable iota) 100))))
           . #0#)))
