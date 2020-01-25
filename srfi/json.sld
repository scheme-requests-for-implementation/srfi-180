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
          (srfi 151)
          (chibi ast)
          (chibi regexp))

  (include "json.scm"))
