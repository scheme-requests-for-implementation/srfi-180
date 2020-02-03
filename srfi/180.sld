(define-library (srfi 180)

  (export json-null?
          json-error?
          json-error-reason
          json-fold
          json-generator-read
          json-read
          json-write)

  (import (scheme base)
          (scheme inexact)
          (scheme case-lambda)
          (scheme char)
          (scheme text)
          (scheme write)
          (check)
          (srfi 145)
          (srfi 151)
          (srfi 180 helpers))

  (include "180/body.scm"))
