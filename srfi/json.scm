(define (pk . args)
  (car (reverse args)))

(define (json-null? obj)
  (eq? obj 'null))

(define-record-type <json-error>
  (make-json-error reason)
  json-error?
  (reason json-error-reason))

(define (json-whitespace? char)
  (case char
    ((#\x20 ; Space
      #\x09 ; Horizontal tab
      #\x0A ; Line feed or New line
      #\x0D)
     #t)
    (else #f)))

(define (expect value other)
  (when (eof-object? value)
    (raise (make-json-error "Unexpected end-of-file.")))
  (assume (char? value))
  (assume (char? other))
  (unless (char=? value other)
    (raise (make-json-error "Unexpected character."))))

(define (port->generator port)
  (define (%read-error? x)
    ;; TODO: move to a helper library
    (and (error-object? x) (memq (exception-kind x) '(user read read-incomplete)) #t))

  (define offset 0)

  (lambda ()
    (guard (ex ((%read-error? ex) (raise (make-json-error "Read error!"))))
      (pk 'read-char (read-char port)))))

(define (gcons head generator)
  ;; returns a generator that will yield, HEAD the first time, and
  ;; after than, it will yield items from GENERATOR.
  (let ((head? #t))
    (lambda ()
      (if head?
          (begin (set! head? #f) head)
          (generator)))))

(define (json-tokens generator)

  (define (maybe-ignore-whitespace generator)
    (let loop ((char (generator)))
      (if (json-whitespace? char)
          (loop (generator))
          char)))

  (define (expect-null generator)
    (expect (generator) #\u)
    (expect (generator) #\l)
    (expect (generator) #\l))

  (define (expect-true generator)
    (expect (generator) #\r)
    (expect (generator) #\u)
    (expect (generator) #\e))

  (define (expect-false generator)
    (expect (generator) #\a)
    (expect (generator) #\l)
    (expect (generator) #\s)
    (expect (generator) #\e))

  (define (maybe-char generator)
    (let ((char (generator)))
      (when (eof-object? char)
        (raise (make-json-error "Unexpected end-of-file.")))
      (when (char=? char #\")
        (raise (make-json-error "Unexpected end of string.")))
      char))

  (define (read-unicode-escape generator)
    (let* ((one (maybe-char generator))
           (two (maybe-char generator))
           (three (maybe-char generator))
           (four (maybe-char generator)))
      (let ((out (string->number (list->string (list one two three four)) 16)))
        (if out
            out
            (raise (make-json-error "Invalid code point."))))))

  (define ash arithmetic-shift)

  (define (read-json-string generator)
    (let loop ((char (generator))
               (out '()))

      (when (eof-object? char)
        (raise (make-json-error "Unexpected end of file.")))

      (when (or (char=? char #\x00)
                (char=? char #\newline)
                (char=? char #\tab))
        (raise (make-json-error "Unescaped control char.")))

      ;; XXX: Here be dragons.
      (if (char=? char #\\)
          (begin
            (let loop-unescape ((char (generator))
                                (chars-unescaped '()))
              (case char
                ((#\" #\\ #\/) (loop (generator)
                                     (cons char (append chars-unescaped
                                                        out))))
                ((#\b) (loop (generator) (cons #\backspace
                                                    (append chars-unescaped
                                                            out))))
                ((#\f) (loop (generator) (cons #\x0C
                                                    (append chars-unescaped
                                                            out))))
                ((#\n) (loop (generator) (cons #\newline
                                                    (append chars-unescaped
                                                            out))))
                ((#\r) (loop (generator) (cons #\x0D
                                                    (append chars-unescaped
                                                            out))))
                ((#\t) (loop (generator) (cons #\tab
                                                    (append chars-unescaped
                                                            out))))
                ((#\u) (let loop-unicode ((code1 (read-unicode-escape generator))
                                          (chars chars-unescaped))
                         (let ((next-char (generator)))
                           (if (and (<= #xd800 code1 #xdbff)
                                    (char=? next-char #\\))
                               (if (char=? (generator) #\u)
                                   (let ((code2 (read-unicode-escape generator)))
                                     (if (<= #xdc00 code2 #xdfff)
                                         (let ((integer
                                                (+ #x10000 (bitwise-ior
                                                            (ash (- code1 #xd800) 10)
                                                            (- code2 #xdc00)))))
                                           ;; full escape of unicode is parsed...
                                           (loop (generator)
                                                 (cons (integer->char integer)
                                                       (append chars
                                                               out))))
                                         ;; This is another unicode char
                                         (loop-unicode (read-unicode-escape generator)
                                                       (cons (integer->char code1) chars))))
                                   ;; The escaped unicode char is
                                   ;; parsed, need to parse another
                                   ;; escape that is not a unicode
                                   ;; escape sequence
                                   (loop-unescape char (cons (integer->char code1)
                                                             chars)))
                             ;; This is not a big-ish unicode char and
                             ;; the next thing is some other char.
                             (loop next-char
                                   (cons (integer->char code1) (append chars out)))))))
                (else (raise (make-json-error "Unexpected escaped sequence."))))))
          (cond
           ((char=? char #\")
            (list->string (reverse out)))
           (else
            (loop (generator) (cons char out)))))))

  (define (valid-number? string)
    ;; TODO: move to a helper library
    ;; based on https://stackoverflow.com/a/13340826/140837
    (regexp-matches '(seq
                      (? #\-)
                      (or #\0 (seq (- numeric #\0)
                                   (* numeric)))
                      (? (seq #\. (+ numeric)))
                      (? (seq (or #\e #\E)
                              (? (or #\- #\+))
                              (+ numeric))))
                    string))

  (define (maybe-read-number generator)
    ;; accumulate chars until a control char or whitespace is reached,
    ;; validate that it is JSON number, then intrepret it as Scheme
    ;; number using string->number
    (let loop ((char (generator))
               (out '()))
      (if (or (eof-object? char)
              (json-whitespace? char)
              (char=? char #\,)
              (char=? char #\])
              (char=? char #\}))
          (let ((string (list->string (reverse out))))
            (if (valid-number? string)
                (let ((number (string->number string)))
                  (if number
                      (values number char)
                      (raise (make-json-error "Invalid number."))))
                (raise (make-json-error "Invalid number."))))
          (loop (generator) (cons char out)))))

  ;; gist
  (assume (procedure? generator))

  (let ((char (generator)))
    (if (eof-object? char)
        eof-object  ;; return an empty generator
        (begin

          (unless (char=? char #\xFEFF)
            ;; if it is not a UTF-8 BOM, put back the char in front of
            ;; the generator
            (set! generator (gcons char generator)))

          (lambda ()

            (define char (maybe-ignore-whitespace generator))

            (if (eof-object? char)
                char ;; return that eof-object
                (case char
                  ((#\n) (expect-null generator) 'null)
                  ((#\t) (expect-true generator) #t)
                  ((#\f) (expect-false generator) #f)
                  ((#\:) 'colon)
                  ((#\,) 'comma)
                  ((#\[) 'array-start)
                  ((#\]) 'array-end)
                  ((#\{) 'object-start)
                  ((#\}) 'object-end)
                  ((#\") (read-json-string generator))
                  (else
                   (call-with-values (lambda () (maybe-read-number (gcons char generator)))
                     (lambda (number next)
                       (set! generator (gcons next generator))
                       number))))))))))

(define (%json-generator-read tokens)

  (define (array-maybe-continue tokens k)
    (lambda ()
      (let ((token (tokens)))
        (case token
          ((comma) (start tokens (array-maybe-continue tokens k)))
          ((array-end) (values '(json-structure . array-end) k))
          (else (raise (make-json-error "Invalid array, expected comma or array close.")))))))

  (define (array-start tokens k)
    (lambda ()
      (let ((token (tokens)))
        (if (eq? token 'array-end)
            (values '(json-structure . array-end) k)
            (start (gcons token tokens) (array-maybe-continue tokens k))))))

  (define (object-maybe-continue tokens k)
    (lambda ()
      (let ((token (tokens)))
        (case token
          ((object-end) (values '(json-structure . object-end) k))
          ((comma) (let ((token (tokens)))
                     (unless (string? token)
                       (raise (make-json-error "Invalid object, expected an object key")))
                     (values (cons 'json-value token)
                             (object-value tokens k))))
          (else (raise (make-json-error "Invalid object, expected comma or object close.")))))))

  (define (object-value tokens k)
    (lambda ()
      (let ((token (tokens)))
        (if (eq? token 'colon)
            (start tokens (object-maybe-continue tokens k))
            (raise (make-json-error "Invalid object, expected colon."))))))

  (define (object-start tokens k)
    (lambda ()
      (let ((token (tokens)))
        (cond
         ((eq? token 'object-end) (values '(json-structure . object-end) k))
         ((string? token)
          (values (cons 'json-value token)
                  (object-value tokens k)))
         (else (raise (make-json-error "Invalid object, expected object key or object close.")))))))

  (define (start tokens k)
    (let ((token (tokens)))
      (cond
       ((or (json-null? token)
            (number? token)
            (string? token)
            (boolean? token))
        (values (cons 'json-value token) k))
     ((eq? token 'array-start)
      (values '(json-structure . array-start) (array-start tokens k)))
     ((eq? token 'object-start)
      (values '(json-structure . object-start) (object-start tokens k)))
     (else (raise (make-json-error "Is it JSON text?!"))))))

  (define (end-of-json-sequence)
    ;; json-generator-read returns a generator that reads one
    ;; top-level json. If there is more than one top-level json value
    ;; in the generator separated with space as it is the case of
    ;; json-lines, you need to call json-generator-read with the same
    ;; port or generator.
    (values (eof-object) #f))

  (define (make-trampoline-generator tokens)
    (let ((continuation (lambda () (start tokens end-of-json-sequence))))
      (lambda ()
        (when continuation
          (call-with-values continuation
            (lambda (event new-continuation)
              (set! continuation new-continuation)
              (pk 'event event)))))))

  ;; gist

  (assume (procedure? tokens))

  (make-trampoline-generator tokens))

(define json-generator-read-error
  "Argument does not look like a generator and is not a textual input port.")

(define json-generator-read
  (case-lambda
    (() (json-generator-read (current-input-port)))
    ((port-or-generator)
     (cond
      ((procedure? port-or-generator)
       (%json-generator-read (json-tokens port-or-generator)))
      ((and (textual-port? port-or-generator) (input-port? port-or-generator))
       (%json-generator-read (json-tokens (port->generator port-or-generator))))
      (else (error 'json json-generator-read-error port-or-generator))))))

;; XXX: procedure foldts is not used as-is. It was copied here for
;; documentation purpose (public domain, by Oleg Kiselyov).
(define (foldts fdown fup fhere seed tree)
  ;; - fhere is applied to the leafs of the tree
  ;;
  ;; - fdown is invoked when a non-leaf node is entered before any of
  ;; the node's children are visited. fdown action has to generate a
  ;; seed to be passed to the first visited child of the node.
  ;;
  ;; - fup is invoked after all the children of a node have been
  ;; seen. The first argument is the local state at the moment the
  ;; traversal process enters the branch rooted at the current node. The
  ;; second argument is the result of visiting all child branches.  The
  ;; action of fup isto produce a seed that is taken to be the state of
  ;; the traversal after the process leave the currents the current
  ;; branch.
  (cond
   ((null? tree) seed)
   ((not (pair? tree))      ; An atom
    (fhere seed tree))
   (else
    (let loop ((kid-seed (fdown seed tree))
               (kids (cdr tree)))
      (if (null? kids)
      (fup seed kid-seed tree)
      (loop (foldts fdown fup fhere kid-seed (car kids))
        (cdr kids)))))))

(define (json-fold array-start array-end object-start object-end fhere seed events)

  ;; json-fold is inspired from the above foldts definition, it is
  ;; built in continuation-passing-style.

  (define (ruse seed k)
    (lambda ()
      (let loop ((seed seed))
        (let ((event (events)))
          (pk 'ruse-event event)
          (if (eof-object? event)
              (begin (k seed) #f)
              (case (car event)
                ((json-value) (loop (fhere (cdr event) seed)))
                ((json-structure)
                 (case (cdr event)
                   ;; termination cases
                   ((array-end) (k seed) #f)
                   ((object-end) (k seed) #f)
                   ;; recursion
                   ((array-start) (ruse (array-start seed)
                                        (lambda (out) (loop (array-end out seed)))))
                   ((object-start) (ruse (object-start seed)
                                         (lambda (out) (loop (object-end out seed)))))
                   (else (error 'json "Oops0!"))))))))))

  (define (make-trampoline-fold k)
    (let ((thunk (ruse seed k)))
      (let loop ((thunk thunk))
        (when thunk
          (loop (thunk))))))

  (define %unset '(unset))

  (let ((out %unset))
    (make-trampoline-fold (lambda (out*) (set! out out*)))
    (if (eq? out %unset)
        (error 'json "Oops1!")
        out)))

(define (%json-read port-or-generator)

  (define %root '(root))

  (define (array-start seed)
    '())

  (define (array-end items seed)
    (let ((out (list->vector (reverse items))))
      (if (eq? seed %root)
          out
          (cons out seed))))

  (define (object-start seed)
    '())

  (define (plist->alist plist)
    ;; PLIST is a list of even items, otherwise json-read-generator
    ;; would have raised a json-error.  json-generator-read is
    ;; validating.
    (let loop ((plist plist)
               (out '()))
      (if (null? plist)
          out ;; TODO: maybe use reverse
          (loop (cddr plist) (cons (cons (string->symbol (cadr plist)) (car plist)) out)))))

  (define (object-end plist seed)
    (let ((alist (plist->alist plist)))
      (if (eq? seed %root)
          alist
          (cons alist seed))))

  (define (fhere obj seed)
    (if (eq? seed %root)
        obj
        (cons obj seed)))

  (let ((events (json-generator-read port-or-generator)))
    (json-fold array-start array-end object-start object-end fhere %root events)))

(define json-read
  (case-lambda
    (() (json-read (current-input-port)))
    ((port-or-generator) (%json-read port-or-generator))))

(define (%json-write obj port)

  (define (void)
    (if #f #f))

  (define (raise-unless-valid? obj)
    (cond
     ((null? obj) (void))
     ((eq? obj 'null) (void))
     ((boolean? obj) (void))
     ((string? obj) (void))
     ((and (number? obj)
           (not (infinite? obj))
           (not (nan? obj))
           (real? obj)
           (or (and (exact? obj) (= (denominator obj) 1))
               (inexact? obj)))
      (void))
     ((vector? obj)
      (vector-for-each (lambda (obj) (raise-unless-valid? obj)) obj))
     ;; XXX: use pair? then recursively check the tail.
     ((pair? obj)
      (for-each (lambda (obj)
                  (unless (pair? obj)
                    (raise (make-json-error "Unexpected object, not a pair.")))
                  (unless (symbol? (car obj))
                    (raise (make-json-error "Unexpected object, not a symbol key.")))
                  (raise-unless-valid? (cdr obj)))
                obj))
     (else (raise (make-json-error "Unexpected object")))))

  (define (write-json-char char port)
    (case char
      ((#\x00) (write-string "\\u0000" port))
      ((#\") (write-string "\\\"" port))
      ((#\\) (write-string "\\\\" port))
      ((#\/) (write-string "\\/" port))
      ((#\return) (write-string "\\r" port))
      ((#\newline) (write-string "\\n" port))
      ((#\tab) (write-string "\\t" port))
      ((#\backspace) (write-string "\\b" port))
      ((#\x0c) (write-string "\\f" port))
      ((#\x0d) (write-string "\\r" port))
      (else (write-char char port))))

  (define (write-json-string string port)
    (write-char #\" port)
    (string-for-each
     (lambda (char) (write-json-char char port))
     string)
    (write-char #\" port))

  (define (write obj port)
    (cond
     ((eq? obj 'null) (write-string "null" port))
     ((boolean? obj) (if obj
                         (write-string "true" port)
                         (write-string "false" port)))
     ((string? obj) (write-json-string obj port))
     ((number? obj) (write-string (number->string obj) port))
     ((vector? obj)
      (write-char #\[ port)
      (unless (zero? (vector-length obj))
        (let loop ((index 0))
          (unless (zero? (- (vector-length obj) index 1))
            (let ((obj (vector-ref obj index)))
              (write obj port)
              (write-string ", " port)
              (loop (+ index 1)))))
        (write (vector-ref obj (- (vector-length obj) 1)) port))
      (write-char #\] port))
     ;; XXX: pair? instead of list? because it is faster.
     ((pair? obj)
      (write-char #\{ port)
      (let ((last (let loop ((obj obj))
                    (if (null? (cdr obj))
                        (car obj)
                        (let ((key (caar obj))
                              (value (cdar obj)))
                          (write-json-string (symbol->string key) port)
                          (write-string ": " port)
                          (write value port)
                          (write-string ", " port)
                          (loop (cdr obj)))))))
        (let ((key (car last))
              (value (cdr last)))
          (write-json-string (symbol->string key) port)
          (write-string ": " port)
          (write value port)))
      (write-char #\} port))
     ((null? obj) (write-string "{}" port))
     (else (raise (cons 'programming-error obj)))))

  (assume (and (textual-port? port) (output-port? port)))
  (raise-unless-valid? obj)
  (write obj port))

(define json-write
  (case-lambda
    ((obj) (json-write obj (current-output-port)))
    ((obj port) (%json-write obj port))))
