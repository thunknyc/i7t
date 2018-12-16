(define-library (thunknyc types)
  (import (scheme red) (only (chibi ast) type-of))
  (export <opcode> <procedure> <set> <string> <pair> <vector> <hash-table>
          <integer> <flonum> <bignum> <ratio> <complex>)
  (begin
    (define <opcode> (type-of +))
    (define <procedure> (type-of write))
    (define <set> (type-of (set equal?)))
    (define <string> (type-of ""))
    (define <pair> (type-of (cons 'a 'b)))
    (define <vector> (type-of #()))
    (define <hash-table> (type-of (hash-table equal?)))
    (define <integer> (type-of 42))
    (define <flonum> (type-of 42.0))
    (define <bignum> (type-of 424242424242424242424242424242424242))
    (define <ratio> (type-of 22/7))
    (define <complex> (type-of 3.0+4.0i))))