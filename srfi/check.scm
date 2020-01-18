;; tests helpers

(define (pk . args) ;; peek stuff, debug helper.
  (write args (current-error-port))
  (display #\newline (current-error-port))
  (flush-output-port (current-error-port))
  (car (reverse args)))

(define-syntax define-syntax-rule
  (syntax-rules ()
    ((define-syntax-rule (keyword args ...) body)
     (define-syntax keyword
       (syntax-rules ()
         ((keyword args ...) body))))))

(define-syntax-rule (check expected actual)
  (lambda ()
    (let ((expected* expected))
      (guard (ex (else (vector #f 'exception-raised expected* ex)))
        (let ((actual* actual))
          (if (equal? expected* actual*)
              (vector #t)
              (vector #f 'unexpected-result expected* actual*)))))))

(define-syntax-rule (check-raise predicate? actual)
  (lambda ()
    (let ((predicate?* predicate?))
      (guard (ex ((predicate?* ex) (vector #t))
                 (else (vector #f 'unexpected-exception predicate?* ex)))
        (let ((actual* actual))
          (vector #f 'no-exception predicate?* actual*))))))

(define-syntax-rule (skip test expected actual)
  (lambda ()
    (vector #t)))

(define (success? v)
  (vector-ref v 0))

(define (failure? v)
  (not (success? v)))

(define (failure-expected v)
  (vector-ref v 1))

(define (failure-actual v)
  (vector-ref v 2))
