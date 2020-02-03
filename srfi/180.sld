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
          (srfi 180 helpers))

  (cond-expand ((library (srfi 60))
                (import (only (srfi 60) arithmetic-shift bitwise-ior)))
               ((library (srfi 151))
                (import (only (srfi 151) arithmetic-shift bitwise-ior))))

  (include "180/body.scm"))
