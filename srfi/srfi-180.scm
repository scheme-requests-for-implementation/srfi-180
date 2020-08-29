(library (srfi srfi-180)

  (export json-number-of-character-limit
	  json-nesting-depth-limit
	  json-null?
	  json-error?
	  json-error-reason
	  json-fold
	  json-generator
	  json-read
	  json-lines-read
	  json-sequence-read
	  json-accumulator
	  json-write)

  (import (scheme base)
	  (scheme inexact)
	  (scheme case-lambda)
	  (scheme char)
	  (scheme write)
	  (srfi srfi-145)
	  (only (srfi srfi-60) arithmetic-shift bitwise-ior))

    (define (%read-error? x)
	(read-error? x))
    (define (valid-number? string)
	(number? (string->number string)))
    (include "180/body.scm"))
