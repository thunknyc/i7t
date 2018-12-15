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

(define i7t-object-grammar
  (grammar/unmemoized
   i7t-object
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

   (i7t-object ((: ,i7t-space (-> o ,i7t-datum) ,i7t-space) o))))

(define (parse-i7t source . o)
  (let ((index (if (pair? o) (car o) 0)))
    (parse i7t-object-grammar source index)))

(define (read-file-i7t filename)
  (let* ((stream (file->parse-stream filename))
         (i7t-exprs (parse-fold i7t-object-grammar cons '() stream 0)))
    (reverse! i7t-exprs)))
