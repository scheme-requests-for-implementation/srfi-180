(define (json-null? obj)
  (eq? obj 'null))

(define-record-type <json-error>
  (make-json-error reason)
  json-error?
  (reason json-error-reason))

(define (json-whitespace? char)
  (assume (char? char))
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
    ;; XXX: non portable
    (and (error-object? x) (memq (exception-kind x) '(user read read-incomplete)) #t))

  (lambda ()
    (guard (ex ((%read-error? ex) (raise (make-json-error "Read error!"))))
      (read-char port))))

;;
;; TODO: return a generator.
;;
(define (json-tokens generator)

  (define (maybe-ignore-whitespace generator)
    (let loop ((char (generator)))
      (if (json-whitespace? char)
          (loop (generator))
          char)))

  (define (expect-null generator)
    (expect (read-char port) #\u)
    (expect (read-char port) #\l)
    (expect (read-char port) #\l))

  (define (expect-true generator)
    (expect (read-char port) #\r)
    (expect (read-char port) #\u)
    (expect (read-char port) #\e))

  (define (expect-false generator)
    (expect (read-char port) #\a)
    (expect (read-char port) #\l)
    (expect (read-char port) #\s)
    (expect (read-char port) #\e))

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
                                         (loop-unicode (read-unicode-escape port)
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
            (callback (list->string (reverse out))))
           (else
            (loop (generator) (cons char out)))))))

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
            (or (and (valid-number? string)
                     (string->number string))
                (raise (make-json-error "Invalid number."))))
          (loop (generator) (cons char out)))))

  ;; gist
  (assume (procedure? generator))

  (let ((char (generator)))
    (if (eof-object? char)
        eof-object  ;; return an empty generator
        (begin

          ;; ignore UTF-8 BOM if any
          (when (char=? char #\xFEFF)
            (set! char (generator)))

          (when (json-whitespace? char)
            (set! char (maybe-ignore-whitespace generator)))

          (let loop ((char char))
            (lambda ()
              (if (eof-object? char)
                  char ;; return that eof-object
                  (begin
                    (case char
                      ((#\n) (expect-null generator) 'null)
                      ((#\t) (expect-true generator) #t)
                      ((#\f) (expect-false generator) #f)
                      ((#\:) 'colon)
                      ((#\,) 'comma)
                      ((#\[) 'array-open)
                      ((#\]) 'array-close)
                      ((#\{) 'object-open)
                      ((#\}) 'object-close)
                      ((#\") (read-json-string generator))
                      (else (maybe-read-number generator)))
                    (loop (maybe-ignore-whitespace generator))))))))))

(define (%json-generator-read tokens)

  (define (gcons head generator)
    ;; returns a generator that will yield, HEAD the first time, and
    ;; after than, it will yields items from GENERATOR.
    (let ((head? #t))
      (lambda ()
        (if head?
            (begin (set! head? #f) head)
            (generator)))))

  (define (array-maybe-continue tokens k)
    (lambda ()
      (let ((token (tokens)))
        (case token
          ((comma) (start tokens (array-maybe-continue tokens k)))
          ((array-close) (values '(json-structure . array-close) k))
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
          ((object-close) (values '(json-structure . object-close) k))
          ((comma) (let ((token (tokens)))
                     (unless (string? token)
                       (raise "Invalid object, expected an object key"))
                     (values (cons 'json-value (string->symbol token))
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
         ((eq? token 'object-close) (values '(json-structure . object-close) k))
         ((string? token)
          (values (cons 'json-value (string->symbol token))
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
     ((eq? token 'array-open)
      (values '(json-structure . array-start) (array-start tokens k)))
     ((eq? token 'object-open)
      (values '(json-structure . object-start) (object-start tokens k)))
     (else (raise (make-json-error "Is it JSON text?!"))))))

  (define (top-level-json-eof)
    ;; json-generator-read returns a generator that reads one
    ;; top-level json. If there is more than one top-level json value
    ;; in the generator separated with space as it is the case of
    ;; json-lines, you need to call json-generator-read with the same
    ;; port or generator.
    (values eof-object top-level-json-eof))

  (define (make-trampoline-generator tokens)
    (let loop ((continuation (lambda () (start tokens top-level-json-eof))))
      (lambda ()
        (call-with-values continuation
          (lambda (new-continuation event)
            (set! continuation new-continuation)
            event)))))

  ;; gist

  (assume? (procedure? generator))

  (make-trampoline-generator tokens))

(define json-generator-read-error
  "Argument does not look like a generator and is not a textual input port.")

(define json-generator-read
  (case-lambda
    (() (json-generator-read (current-input-port)))
    ((port-or-generator)
     (cond
      ((procedure? port-or-generator)
       (%json-generator-read (json-tokenize port-or-generator)))
      ((and (textual-port? port-or-generator) (input-port? port-or-generator))
       (%json-generator-read (json-tokenize (port->generator port-or-generator))))
      (else (error 'json json-generator-read-error port-or-generator))))))

;; XXX: foldts is not used. It was copied here for documentation
;; purpose (public domain, by Oleg Kiselyov).
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
  (let loop ((seed seed))
    (let ((event (events)))
      (if (eof-object? event)
          seed
          (case (car (event))
            ((json-value) (loop (fhere (cdr event) seed)))
            ((json-structure)
             (case (cdr event)
               ((array-end) seed)
               ((array-start) (array-end (json-fold array-start
                                                    array-end
                                                    object-start
                                                    object-end
                                                    fhere
                                                    (array-start seed)
                                                    events)
                                         seed))
               ((object-end) seed)
               ((object-start) (object-end (json-fold array-start
                                                      array-end
                                                      object-start
                                                      object-end
                                                      fhere
                                                      (object-start seed)
                                                      events)
                                           seed))
               (else (error 'json "Oops!")))))))))

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

  (define (plist->alist items)
    ;; items is a list of even items, otherwise json-read-generator
    ;; would have raised a json-error?
    (let loop ((items items)
               (out '()))
      (if (null? items)
          out
          (loop (cddr items) (cons (cons (car items) (cadr items)) out)))))

  (define (object-end plist seed)
    (let ((alist (plist->alist plist)))
      (if (eq? seed %root)
          alist
          (cons alist seed))))

  (define fhere values)

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
     ;; XXX: pair? instead of list? because it is faster.
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
