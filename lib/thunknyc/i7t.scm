(define i7t-comparator (make-default-comparator))

(define (nil? x) (equal? nil))

(define (inc x) (+ x 1))
(define (dec x) (- x 1))

(define (identity x) x)

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

(define (*-drop col i)
  (cond ((list? col) (drop col i))
        ((vector? col) (vector-drop col i))
        (else (error "Unknown sequential" col))))

(define (*-length col)
  (cond ((list? col) (length col))
        ((vector? col) (vector-length col))
        ((hash-table? col) (hash-table-size col))
        (else (error "Unknown collection" col))))

(define (*-empty? col)
  (= (*-length col) 0))

(define (arg-accessor a i) `(get ,a ,i))
(define (rest-accessor a skip) `(*-drop ,a ,skip))

(define (positional-args arg-names)
  (map (lambda (args keys) (list args keys))
       arg-names (iota (*-length arg-names))))

(define (map-key-args arg-names)
  (map (lambda (arg) `(,arg (quote ,arg))) arg-names))

(define (chunk n col)
  (let loop ((col col) (even '()) (out '()) (i 0))
    (cond ((null? col) (reverse! out))
          ((= (remainder (inc i) n) 0)
           (loop (cdr col)
                 '()
                 (cons (reverse! (cons (car col) even)) out)
                 (inc i)))
          (else
           (loop (cdr col)
                 (cons (car col) even)
                 out
                 (inc i))))))

(define (map-namekey-args nks)
  (map (lambda (nk)
         (let ((name (car nk))
               (key (cadr nk)))
           `(,name ,(translate-i7t key))))
       (chunk 2 nks)))

(define (destructure accessor var bindings)
  (match var
         ('_ bindings)
         ((? symbol? x)
          (cons `(,x ,accessor) bindings))
         (('__VEC as ... '& rest ('__KW "as") all)
          (make-bindings (positional-args as) rest all accessor bindings))
         (('__VEC as ... ('__KW "as") all)
          (make-bindings (positional-args as) '_ all accessor bindings))
         (('__VEC as ... '& rest)
          (make-bindings (positional-args as) rest '_ accessor bindings))
         (('__VEC as ...)
          (make-bindings (positional-args as) '_ '_ accessor bindings))
         (('__MAP ('__KW "keys") ('__VEC ks ...) ('__KW "as") all)
          (make-bindings (map-key-args ks) '_ all accessor bindings))
         (('__MAP ('__KW "keys") ('__VEC ks ...))
          (make-bindings (map-key-args ks) '_ '_ accessor bindings))
         (('__MAP nks ... ('__KW "as") all)
          (make-bindings (map-namekey-args nks) '_ all accessor bindings))
         (('__MAP nks ...)
          (make-bindings (map-namekey-args nks) '_ '_ accessor bindings))
         (x (error "No matching destructuring" x))))

(define (make-args n)
  (map (lambda (i) (string->symbol (show #f "__arg" i))) (iota n)))

(define (make-bindings args rest-name as-name parent-accessor bindings)
  (let* ((n (*-length args))
         (arg-names (map car args))
         (arg-keys (map cadr args))
         (arg-accessors (map (lambda (k) (arg-accessor parent-accessor k))
                             arg-keys))
         (arg-bindings (fold destructure bindings arg-accessors arg-names))
         (rest-binding (destructure (rest-accessor parent-accessor n)
                                    rest-name arg-bindings))
         (as-bindings (destructure parent-accessor as-name rest-binding)))
    `(,@as-bindings)))

(define (build-lambda arg-names rest-name as-name body)
  (let* ((root-accessor (if (equal? as-name '_) '__args as-name))
         (bindings (make-bindings (map (lambda (name key) (list name key))
                                       arg-names
                                       (iota (inc (length arg-names))))
                                  rest-name '_ root-accessor '())))
    (cond ((*-empty? bindings)
           `(lambda () (let ((,root-accessor '())) ,@body)))
          (else
           `(lambda ,root-accessor (let (,@bindings) ,@body))))))

(define (optimize-sexp sexp)
  (ast->sexp (optimize (analyze sexp))))

(define (lambda-clause args expr exprs)
  (let ((body (map translate-i7t (cons expr exprs))))
    (match args

           (('& rest)
            `(,rest (apply ,(build-lambda '() rest '_ body) ,rest)))

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
            `(() ((build-lambda '() '_ '_ body))))

           (x (error "Unsupported argument specification" x)))))

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

(define (empty-map) (hash-table i7t-comparator))

(define-record-type <keyword>
  (keyword name)
  keyword?
  (name keyword-name))

(define string->keyword keyword)
(define keyword->string keyword-name)
(define keyword->symbol (lambda (k) (string->symbol (keyword-name k))))
(define symbol->keyword (lambda (s) (keyword (symbol->string s))))

(define borrowed-macros (apply set i7t-comparator
                               '(and or if
                                 test)))

(define (borrowed? name)
  (set-contains? borrowed-macros name))

(define (translate-cond test-exprs else-expr)
  (let ((tests (chunk 2 (map translate-i7t test-exprs))))
    `(cond ,@tests ,@(if else-expr `((else ,(translate-i7t else-expr))) '()))))

(define (translate-i7t form)
  (match form

         ;; Borrowed special forms
         (('__LIST (? borrowed? name) args ...)
          `(,name ,@(map translate-i7t args)))

         ;; Cond
         (('__LIST 'cond test-exprs ... ('__KW "else") else-expr)
          (translate-cond test-exprs else-expr))

         (('__LIST 'cond test-exprs ...)
          (translate-cond test-exprs #f))

         ;; Definitions
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

         ;; Lambda forms
         (('__LAMBDA ('__LIST ('__VEC a1s ...) e1s e2s ...) ...)
          `(match-lambda* ,@(map lambda-clause a1s e1s e2s)))

         (('__LAMBDA ('__VEC a1 ...) e1 e2 ...)
          `(lambda ,@(lambda-clause a1 e1 e2)))

         (('__LAMBDA as ...)
          (error "Malformed lambda" form))

         ;; Let (let* in Scheme)
         (('__LIST 'let ('__VEC nvs ...) e es ...)
          (let ((namevals (map-namekey-args nvs))
                (body (map translate-i7t (cons e es))))
            `(let* (,@namevals) ,@body)))

         ;; Loop (loop in Clojure, named let in Scheme)
         (('__LIST 'loop recur ('__VEC nvs ...) e es ...)
          (let ((namevals (map-namekey-args nvs))
                (body (map translate-i7t (cons e es))))
            `(let ,recur (,@namevals) ,@body)))

         ;; Quotation
         (('__LIST 'quote a)
          (enquoted (translate-i7t a)))

         ;; Procedure application
         (('__LIST proc a1 ...)
          (if (should-quote?)
              `(list ,(translate-i7t proc) ,@(map translate-i7t a1))
              `(apply-protocol 'core.applicable 'apply
                               ,(translate-i7t proc)
                               ,@(map translate-i7t a1))))

         ;; Literals
         (('__SET e1 ...)
          `(set i7t-comparator ,@(map translate-i7t e1)))

         (('__VEC e1 ...)
          `(vector ,@(map translate-i7t e1)))

         (('__MAP e1 ...)
          `(hash-table i7t-comparator ,@(map translate-i7t e1)))

         (('__KW name) `(keyword ,name))

         ((? symbol? sym) (if (should-quote?) `(quote  ,sym) sym))
         ((? number? num) num)
         ((? string? str) str)

         (('__TRUE) #t)
         (('__FALSE) #f)
         (('__NIL) nil)
         (() '())

         (#f (error "Parse error"))
         (x (error "Unknown object" x))))

(define (eval-i7t s)
  (let* ((parsed (parse-i7t s))
         (translated (translate-i7t parsed)))
    (eval translated)))

(define (i7t . els)
  (eval-i7t (apply show #f els)))

(define (expand-file-i7t filename)
  (map translate-i7t (read-file-i7t filename)))

(define (load-i7t filename)
  (let ((exprs (expand-file-i7t filename)))
    (for-each (lambda (expr) (eval expr)) exprs)))

;;; Immutable map operations

(define (map-assoc m . kvs)
  (let ((new-m (if (nil? m) (empty-map) m)))
    (hash-table-union! (apply hash-table i7t-comparator kvs) new-m)))

(define (map-merge m . ms)
  (let loop ((ms ms)
             (new-m (if (nil? m) (empty-map) (hash-table-copy m))))
    (cond ((null? ms) new-m)
          (else (loop (cdr ms)
                      (hash-table-union! (hash-table-copy (car ms)) new-m))))))

;;; Protocols

(define protocol-type-procs (empty-map))

(define (extend-1 type protocol proc-spec-list)
  (let* ((proc-specs (apply hash-table i7t-comparator proc-spec-list))
         (protocol-map (hash-table-ref/default
                        protocol-type-procs protocol (empty-map)))
         (type-map (hash-table-ref/default protocol-map type (empty-map))))
    (set! protocol-type-procs
          (map-assoc protocol-type-procs protocol
                     (map-assoc protocol-map type
                                (map-merge type-map proc-specs))))))

(define (extend type . proto-procspecs)
  (let loop ((pps (chunk 2 proto-procspecs)))
    (when (not (null? pps))
      (let ((protocol (caar pps))
            (proc-specs (cadar pps)))
        (extend-1 type protocol proc-specs))
      (loop (cdr pps)))))

(define (protocol-proc-maybe protocol proc-name this)
  (let* ((protocol-map (hash-table-ref/default
                        protocol-type-procs protocol (empty-map)))
         (type (type-of this))
         (type-map (hash-table-ref/default
                    protocol-map type (empty-map))))
    (hash-table-ref/default type-map proc-name #f)))

(define (apply-protocol protocol proc-name this . args)
  (let* ((proc (or (protocol-proc-maybe protocol proc-name this)
                   (lambda args
                     (error
                      (show #f "Object of type " (written (type-of this))
                            " does not support " proc-name
                            " method of protocol " protocol)
                      this)))))
    (apply proc this args)))

(define (len col)
  (apply-protocol 'core.col 'length col))

(define (get col i . o)
  (apply apply-protocol 'core.col 'ref col i o))

;; I/O implementations

(define (hash->str h)
  (let ((elements (concatenate (hash-table-map->list
                                (lambda (k v) (list (edn-str k) (edn-str v)))
                                h))))
    (show #f "{" (joined displayed elements " ") "}")))

(define (string->str s)
  (show #f (written s)))

;; Integer
(extend <integer>
        'core.io `(->str ,(lambda (x) (show #f x))))

;; Hash-table

(let ((ref (lambda (col i . o)
             (let ((default (if (pair? o) (car o) nil)))
               (hash-table-ref/default col i default)))))

  (extend <hash-table>
          'core.col `(length ,hash-table-size ref ,ref)
          'core.io `(->str ,hash->str)
          'core.applicable `(apply ,ref)))

;; Vector
(let ((ref (lambda (col i . o)
             (let ((default (if (pair? o) (car o) nil))
                   (n (vector-length col)))
               (cond ((and (> i -1) (< i n))
                      (vector-ref col i))
                     (else default))))))

  (extend <vector>
         'core.col `(length ,vector-length ref ,ref)
         'core.applicable `(apply ,ref)))

;; Pair
(let ((ref (lambda (col i . o)
             (let ((default (if (pair? o) (car o) nil))
                   (n (length col)))
               (cond ((and (> i -1) (< i n))
                      (list-ref col i))
                     (else default))))))

    (extend <pair>
         'core.col `(length ,length ref ,ref)
         'core.applicable `(apply ,ref)))

;; String
(let ((ref (lambda (col i . o)
             (let ((default (if (pair? o) (car o) nil))
                   (n (string-length col)))
               (cond ((and (> i -1) (< i n))
                      (string-ref col i))
                     (else default))))))

    (extend <string>
         'core.col `(length ,string-length ref ,ref)
         'core.applicable `(apply ,ref)
         'core.io `(->str ,string->str)))

;; Set
(let ((ref (lambda (col i . o)
             (let ((default (if (pair? o) (car o) nil)))
               (cond ((set-contains? col i) i)
                     (else default))))))

  (extend <set>
          'core.col `(length ,set-size ref ,ref)
          'core.applicable `(apply ,ref)))

;; Opcode
(extend <opcode>
        'core.applicable `(apply ,(lambda (proc . xs) (apply proc xs))))

;; Procedure
(extend <procedure>
        'core.applicable `(apply ,(lambda (proc . xs) (apply proc xs))))

;; Keyword
(let ((ref (lambda (i col . o)
             (let ((default (if (pair? o) (car o) nil))) (get col i)))))

 (extend <keyword> 'core.applicable `(apply ,ref)))

;; Writing

(define (edn-str x)
  (apply-protocol 'core.io '->str x))
