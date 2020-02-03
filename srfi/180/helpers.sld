(define-library (srfi 180 helpers)

  (export %read-error? valid-number?)

  (import (scheme base)
          (chibi ast)
          (chibi regexp))

  (begin

    (define (%read-error? x)
      (and (error-object? x) (memq (exception-kind x) '(user read read-incomplete)) #t))

    (define (valid-number? string)
      ;; based on https://stackoverflow.com/a/13340826/140837
      (regexp-matches '(seq
                        (? #\-)
                        (or #\0 (seq (- numeric #\0)
                                     (* numeric)))
                        (? (seq #\. (+ numeric)))
                        (? (seq (or #\e #\E)
                                (? (or #\- #\+))
                                (+ numeric))))
                      string))))
