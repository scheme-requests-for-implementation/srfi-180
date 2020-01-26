(define-library (json)

  (export json-null?
          json-error?
          json-stream-read
          json-error-reason
          json-read
          json-write)

  (import (scheme base)
          (scheme case-lambda)
          (scheme char)
          (scheme text)
          (scheme write)
          (check)
          (srfi 145)
          (chibi regexp))

  (cond-expand ((library (srfi 60))
                (import (only (srfi 60) arithmetic-shift bitwise-ior)))
               ((library (srfi 151))
                (import (only (srfi 151) arithmetic-shift bitwise-ior))))

  (cond-expand (chibi (import (chibi ast)))
               (else))

  (include "json.scm"))
