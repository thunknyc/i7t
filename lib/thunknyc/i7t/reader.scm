(define sym-char-set
  (let ((cs (char-set-delete char-set:graphic
                             #\# #\( #\) #\[ #\] #\{ #\}
                             #\' #\` #\, #\@ #\^ #\;)))
    cs))

(define post-sign-sym-char-set
  (let ((cs (char-set-delete sym-char-set
                             #\0 #\1 #\2 #\3 #\4 #\5
                             #\6 #\7 #\8 #\9)))
    cs))

(define first-sym-char-set
  (let ((cs (char-set-delete sym-char-set
                             #\+ #\-
                             #\0 #\1 #\2 #\3 #\4 #\5
                             #\6 #\7 #\8 #\9)))
    cs))

(define comment-char-set
  (let ((cs (char-set-delete char-set:full
                             #\newline #\return #\x000a)))
    cs))

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

(define (i7ttagged tag v)
  `(__TAGGED ,tag ,v))

(define (i7tannotated meta v)
  `(__ANNOTATED ,meta ,v))

(define (i7tdereffed v)
  `(__DEREFFED ,v))

(define (i7tvarquoted v)
  `(__VARQUOTED ,v))

(define i7t-object-grammar
  (grammar/unmemoized
   object
   (discardable ((: "#_" ,space ,datum ,space)))

   (space ((* (or ,(parse-char char-whitespace?)
                  #\,
                  ,discardable
                  (: #\; (* ,comment-char-set))))))

   (num ((: (-> sign (or #\- #\+))
            (-> digit1 ,(parse-char char-numeric?))
            (-> digits (* (or ,(parse-char char-numeric?)
                              #\. #\- #\+ #\e #\E #\i #\/))))
         (string->number (apply string sign digit1 digits)))

        ((: (-> digit1 ,(parse-char char-numeric?))
            (-> digits (* (or ,(parse-char char-numeric?)
                              #\. #\- #\+ #\e #\E #\i #\/))))
         (string->number (apply string digit1 digits))))

   (quoted
    ((: #\' (-> d ,datum))
     (i7tlist 'quote d)))

   (keyword
    ((: #\: (-> s (+ ,(parse-char sym-char-set))))
     (i7tkw (apply string s))))

   (sym
    ((-> s (: (or #\+ #\-)
              (+ ,(parse-char post-sign-sym-char-set))
              (* ,(parse-char sym-char-set))))
     (string->symbol (apply string (car s) (concatenate (cdr s)))))

    ((-> s (or #\+ #\-)) (string->symbol (string s)))

    ((-> s (: ,(parse-char first-sym-char-set)
              (* ,(parse-char sym-char-set))))
     (string->symbol (apply string (car s) (cadr s)))))

   (str ((: ,(parse-char #\")
            (-> s (* ,(parse-not-char #\")))
            ,(parse-char #\"))
         (list->string s)))

   (seq-el ((: ,space (-> el ,datum)) el))

   (lambda ((: "#(" ,space (-> el ,datum) ,space
               (-> els (* ,seq-el)) ,space ")")
            (apply i7tlambda (cons el els)))
     ((: "#(" ,space ")") (i7tlambda)))

   (set ((: "#{" ,space (-> el ,datum) ,space
            (-> els (* ,seq-el)) ,space "}")
         (apply i7tset el els))
        ((: "#{" ,space "}") (i7tset)))

   (vec ((: "[" ,space (-> el ,datum) ,space
            (-> els (* ,seq-el)) ,space "]")
         (apply i7tvector el els))
        ((: "[" ,space "]") (i7tvector)))

   (seq ((: "(" ,space (-> el ,datum) ,space
            (-> els (* ,seq-el)) ,space ")")
         (apply i7tlist el els))
        ((: "(" ,space ")") (i7tlist)))

   (mapping-el ((: ,space (-> k ,datum) ,space
                   ,space (-> v ,datum)) (list k v)))

   (mapping ((: "{" ,space (-> k ,datum) ,space
                ,space (-> v ,datum) ,space
                (-> els (* ,mapping-el)) ,space "}")
             (apply i7tmap k v (concatenate els)))
            ((: "{" ,space "}") (i7tmap)))

   (atom ("true" i7ttrue)
         ("false" i7tfalse)
         ("nil" i7tnil)
         ((-> k ,keyword) k)
         ((-> ds ,num))
         ((-> s ,str) s)
         ((-> s ,sym)))

   (annotated ((: #\^ ,space (-> m ,datum) ,space (-> val ,datum))
               (i7tannotated m val)))

   (dereffed ((: #\@ ,space (-> name ,sym)) (i7tdereffed name)))

   (varquoted ((: "#'" ,space (-> name ,sym)) (i7tvarquoted name)))

   (tagged ((: #\# ,space (-> tag ,sym)
               ,space (-> datum ,datum))
            (i7ttagged tag datum)))

   (datum ((or ,annotated ,varquoted ,dereffed
               ,tagged ,quoted ,atom
               ,vec ,seq ,mapping
               ,set ,sym ,lambda)))

   (object ((: ,space (-> o ,datum) ,space) o))))

(define (parse-i7t source . o)
  (let ((index (if (pair? o) (car o) 0)))
    (parse i7t-object-grammar source index)))

(define (read-file-i7t filename)
  (let* ((stream (file->parse-stream filename))
         (i7t-exprs (parse-fold i7t-object-grammar cons '() stream 0)))
    (reverse! i7t-exprs)))
