;; test framework
(define-library (check)
  (export check check-raise skip pk failure? failure-expected failure-actual)

  (import (scheme base))
  (import (scheme write))

  (include "check.scm"))
