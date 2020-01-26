(define (json-null? obj)
  (eq? obj 'null))

(define-record-type <json-error>
  (make-json-error reason)
  json-error?
  (reason json-error-reason))

(define (written obj)
  (call-with-port (open-output-string)
                  (lambda (out) (write obj out) (get-output-string out))))

(define (invalid-object-value obj)
  (raise (make-json-error
          (string-append "Invalid object value: " (written obj) "."))))

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

(define (json-tokenize callback port)

  (define (maybe-ignore-whitespace port)
    (let loop ((char (peek-char port)))
      (unless (eof-object? char)
        (when (json-whitespace? char)
          (read-char port)
          (loop (peek-char port))))))

  (define (read-null callback port)
    (expect (read-char port) #\n)
    (expect (read-char port) #\u)
    (expect (read-char port) #\l)
    (expect (read-char port) #\l)
    (callback 'null))

  (define (read-true callback port)
    (expect (read-char port) #\t)
    (expect (read-char port) #\r)
    (expect (read-char port) #\u)
    (expect (read-char port) #\e)
    (callback #t))

  (define (read-false callback port)
    (expect (read-char port) #\f)
    (expect (read-char port) #\a)
    (expect (read-char port) #\l)
    (expect (read-char port) #\s)
    (expect (read-char port) #\e)
    (callback #f))

  (define (maybe-read-char port)
    (let ((char (read-char port)))
      (when (eof-object? char)
        (raise (make-json-error "Unexpected end-of-file.")))
      (when (char=? char #\")
        (raise (make-json-error "Unexpected end of string.")))
      char))

  (define (read-unicode-escape port)
    (let* ((one (maybe-read-char port))
           (two (maybe-read-char port))
           (three (maybe-read-char port))
           (four (maybe-read-char port)))
      (let ((out (string->number (list->string (list one two three four)) 16)))
        (if out
            out
            (raise (make-json-error "Invalid code point."))))))

  (define ash arithmetic-shift)

  (define (read-json-string callback port)
    (read-char port) ;; "
    (let loop ((char (peek-char port))
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
            (read-char port)
            (let loop-unescape ((char (read-char port))
                                (chars-unescaped '()))
              (case char
                ((#\" #\\ #\/) (loop (peek-char port)
                                     (cons char (append chars-unescaped
                                                        out))))
                ((#\b) (loop (peek-char port) (cons #\backspace
                                                    (append chars-unescaped
                                                            out))))
                ((#\f) (loop (peek-char port) (cons #\x0C
                                                    (append chars-unescaped
                                                            out))))
                ((#\n) (loop (peek-char port) (cons #\newline
                                                    (append chars-unescaped
                                                            out))))
                ((#\r) (loop (peek-char port) (cons #\x0D
                                                    (append chars-unescaped
                                                            out))))
                ((#\t) (loop (peek-char port) (cons #\tab
                                                    (append chars-unescaped
                                                            out))))
                ((#\u) (let loop-unicode ((code1 (read-unicode-escape port))
                                          (chars chars-unescaped))
                         (if (and (<= #xd800 code1 #xdbff)
                                  (char=? (read-char port) #\\))
                             (let ((char (read-char port)))
                               (if (char=? char #\u)
                                   (let ((code2 (read-unicode-escape port)))
                                     (if (<= #xdc00 code2 #xdfff)
                                         (let ((integer
                                                (+ #x10000 (bitwise-ior
                                                            (ash (- code1 #xd800) 10)
                                                            (- code2 #xdc00)))))
                                           ;; full escape of unicode is parsed...
                                           (loop (peek-char port)
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
                                                             chars))))
                             ;; This is not a big-ish unicode char and
                             ;; the next thing is some other char.
                             (loop (peek-char port)
                                   (cons (integer->char code1) (append chars out))))))
                (else (raise (make-json-error "Unexpected escaped sequence."))))))
          (cond
           ((char=? char #\")
            (read-char port) ; "
            (callback (list->string (reverse out))))
           (else
            (read-char port) ;; char
            (loop (peek-char port) (cons char out)))))))

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

  (define (maybe-read-number callback port)
    ;; TODO: implement real json number parser, do not rely on scheme
    ;; string->number.

    ;; accumulate chars until a control char or whitespace is reached,
    ;; then try to intrepret it as number using string->number

    ;; escape early in case of bad input
    (when (char=? (peek-char port) #\+)
      (raise (make-json-error "Unexpected #\\+ character.")))

    (let loop ((char (peek-char port))
               (out '()))
      (if (or (eof-object? char)
              (json-whitespace? char)
              (char=? char #\,)
              (char=? char #\])
              (char=? char #\}))
          (callback
           (or (guard (ex (else (raise (make-json-error "Invalid number."))))
                 (let ((string (list->string (reverse out))))
                   (and (valid-number? string)
                        (string->number string))))
               ;; XXX: apparantly string->number can return #f
               ;; instead of raising.
               (raise (make-json-error "Invalid number."))))
          (begin (read-char port)
                 (loop (peek-char port) (cons char out))))))


  (define (%read-error? x)
    (and (error-object? x)
         (cond-expand
           (chibi (memq (exception-kind x) '(user read read-incomplete)))
           (else #f))
         #t))

  (assume (procedure? callback))
  (assume (and (textual-port? port) (input-port? port)))

  ;; gist
  (guard (ex ((%read-error? ex) (raise (make-json-error "Read error!"))))

    (when (eof-object? (peek-char port))
      (raise (make-json-error "Empty JSON text.")))

    ;; ignore UTF-8 BOM if any
    (let ((char (peek-char port)))
      (when (char=? char #\xFEFF)
        (read-char port)))

    (maybe-ignore-whitespace port)
    (let loop ((char (peek-char port)))
      (if (eof-object? char)
          (callback 'eof)
          (begin
            (case char
              ((#\n) (read-null callback port))
              ((#\t) (read-true callback port))
              ((#\f) (read-false callback port))
              ((#\:) (read-char port) (callback 'colon))
              ((#\,) (read-char port) (callback 'comma))
              ((#\[) (read-char port) (callback 'array-open))
              ((#\]) (read-char port) (callback 'array-close))
              ((#\{) (read-char port) (callback 'object-open))
              ((#\}) (read-char port) (callback 'object-close))
              ((#\") (read-json-string callback port))
              (else (maybe-read-number callback port)))
            (maybe-ignore-whitespace port)
            (loop (peek-char port)))))))

(define (%json-stream-read proc port)

  (define (read-array-continue callback obj k)
    (cond
     ((or (json-null? obj)
          (boolean? obj)
          (string? obj)
          (number? obj))
      (callback 'json-value obj)
      (lambda (obj)
        (read-array-maybe-continue callback obj k)))
     ((eq? obj 'array-open)
      (lambda (obj)
        (read-array-start callback
                           obj
                           ;; continue!
                           (lambda (obj) (read-array-maybe-continue callback obj k)))))
     ((eq? obj 'object-open)
      (lambda (obj)
        (read-object-start callback
                            obj
                            ;; continue!
                            (lambda (obj) (read-array-maybe-continue callback obj k)))))
     (else (raise (make-json-error "Invalid array.")))))

  (define (read-array-maybe-continue callback obj k)
    (case obj
      ((comma)
       (lambda (obj) (read-array-continue callback obj k)))
      ((array-close)
       (callback 'json-structure 'array-close)
       k)
      (else (raise (make-json-error "Invalid array continuation.")))))

  (define (read-array-start callback obj k)
    (callback 'json-structure 'array-open)
    (cond
     ((or (json-null? obj)
          (boolean? obj)
          (string? obj)
          (number? obj))
      (callback 'json-value obj)
      (lambda (obj) (read-array-maybe-continue callback obj k)))
     ((eq? obj 'array-open)
      (lambda (obj)
        (read-array-start callback
                           obj
                           ;; continue!
                           (lambda (obj) (read-array-maybe-continue callback obj k)))))
     ((eq? obj 'object-open)
      (lambda (obj)
        (read-object-start callback
                            obj
                            ;; continue!
                            (lambda (obj) (read-array-maybe-continue callback obj k)))))
     ((eq? obj 'array-close)
      (callback 'json-structure 'array-close)
      k)
     (else (raise (make-json-error "Invalid array!")))))

  (define (read-object-continue callback obj k)
    (cond
     ((string? obj)
      (let ((key (string->symbol obj)))
        (callback 'json-value key)
        (lambda (obj) (read-object-colon callback obj k))))
     (else (raise (make-json-error "Invalid object continuation.")))))

  (define (read-object-maybe-continue callback obj k)
    (cond
     ((eq? obj 'object-close) (callback 'json-structure 'object-close) k)
     ((eq? obj 'comma)
      (lambda (obj)
        (read-object-continue callback obj k)))
     (else (raise (make-json-error "Invalid object.")))))

  (define (read-object-value callback obj k)
    (cond
     ((or (json-null? obj)
          (boolean? obj)
          (string? obj)
          (number? obj))
      (callback 'json-value obj)
      (lambda (obj) (read-object-maybe-continue callback obj k)))
     ((eq? obj 'array-open)
      (lambda (obj)
        (read-array-start callback
                           obj
                           ;; continue!
                           (lambda (obj)
                             (read-object-maybe-continue callback obj k)))))
     ((eq? obj 'object-open)
      (lambda (obj)
        (read-object-start callback
                            obj
                            ;; continue!
                            (lambda (obj)
                              (read-object-maybe-continue callback obj k)))))
     (else (invalid-object-value obj))))

  (define (read-object-colon callback obj k)
    (if (eq? obj 'colon)
        (lambda (obj) (read-object-value callback obj k))
        (raise (make-json-error "Invalid object, expected colon."))))

  (define (read-object-start callback obj k)
    (callback 'json-structure 'object-open)
    (cond
     ((eq? obj 'object-close) (callback 'json-structure 'object-close) k)
     ((string? obj)
      (callback 'json-value (string->symbol obj))
      (lambda (obj) (read-object-colon callback obj k)))
     (else (raise (make-json-error "Invalid object.")))))

  (define (start callback obj)
    (cond
     ((or (json-null? obj)
          (number? obj)
          (string? obj)
          (boolean? obj))
      (callback 'json-value obj)
      (lambda (obj) (raise (make-json-error "Expected end of JSON text"))))
     ((eq? obj 'array-open)
      (lambda (obj)
        (read-array-start callback
                          obj
                          (lambda _
                            (raise (make-json-error "Expected end of JSON text"))))))
      ((eq? obj 'object-open)
       (lambda (obj)
         (read-object-start callback
                            obj
                            (lambda _ (raise (make-json-error "Expected end of JSON text"))))))
      (else (raise (make-json-error "Is is JSON text?!")))))

  (define (make-machine callback)
    (let ((k (lambda (obj) (start callback obj))))
      (lambda (obj)
        ;(pk 'parse obj)
        (unless (eq? obj 'eof)
          (set! k (k obj))))))

  ;; gist
  (assume (and (textual-port? port) (input-port? port)))
  (assume (procedure? proc))
  (json-tokenize (make-machine proc) port))

(define json-stream-read
  (case-lambda
    ((proc) (json-stream-read proc (current-input-port)))
    ((proc port) (%json-stream-read proc port))))

(define (%json-read port)

  (define (read-array out type obj return)
    (case type
      ((json-structure)
       (case obj
         ((array-close) (return (list->vector (reverse out))))
         ((array-open)
          (lambda (type obj)
            (read-array '()
                         type
                         obj
                         (lambda (other)
                           (lambda (type obj)
                             (read-array (cons other out) type obj return))))))
         ((object-open)
          (lambda (type obj)
            (read-object-maybe-key '()
                                    type
                                    obj
                                    (lambda (other)
                                      (lambda (type obj)
                                        (read-array (cons other out) type obj return))))))
         (else (raise (make-json-error "Invalid array.")))))
      ((json-value)
       (let ((value obj))
         (lambda (type obj) (read-array (cons value out) type obj return))))
      (else (raise (make-json-error "Invalid array.")))))

  (define (read-object-value key out type obj return)
    (case type
      ((json-structure)
       (case obj
         ((object-open)
          (lambda (type obj)
            (read-object-maybe-key '()
                                   type
                                   obj
                                   (lambda (value)
                                     (return (cons (cons key value) out))))))
         ((array-open)
          (lambda (type obj)
            (read-array '()
                         type
                         obj
                         (lambda (value)
                           (lambda (type obj)
                             (read-object-maybe-key (cons (cons key value) out)
                                                     type
                                                     obj
                                                     return))))))
         (else (invalid-object-value obj))))
      ((json-value)
       (let ((value obj))
         (lambda (type obj) (read-object-maybe-key (cons (cons key value) out)
                                                    type
                                                    obj
                                                    return))))
      (else (invalid-object-value obj))))

  (define (read-object-maybe-key out type obj return)
    (case type
      ((json-structure)
       (case obj
         ((object-close) (return out))
         (else (raise (make-json-error "Invalid object.")))))
      ((json-value)
       (let ((key obj))
         (lambda (type obj) (read-object-value key out type obj return))))
      (else (raise (make-json-error "Invalid object.")))))

  (define (start type obj return)
    (case type
      ((json-value) (return obj))
      ((json-structure)
       (case obj
         ((object-open)
          (lambda (type obj)
            (read-object-maybe-key '() type obj return)))
         ((array-open)
          (lambda (type obj)
            (read-array '() type obj return)))
         (else (raise (make-json-error "Is this JSON text?!")))))
      (else (raise (make-json-error "Is this JSON text?!")))))

  (define (make-machine return)
    (let ((k (lambda (type obj) (start type obj return))))
      (lambda (type obj)
        ;(pk 'machine type obj)
        (when k
          (set! k (k type obj))))))

  (assume (and (textual-port? port) (input-port? port)))

  (let ((out 'unset))
    (json-stream-read (make-machine (lambda (other) (set! out other) #f)) port)
    (if (eq? out 'unset)
        (raise (make-json-error "json-read oops!"))
        out)))

(define json-read
  (case-lambda
    (() (json-read (current-input-port)))
    ((port) (%json-read port))))

(define (%json-write obj port)

  (define (void)
    (if #f #f))

  (define (raise-unless-valid? obj)
    (cond
     ((null? obj) (void))
     ((eq? obj 'null) (void))
     ((boolean? obj) (void))
     ((string? obj) (void))
     ((number? obj)
      (when (= (abs obj) +inf.0)
        (raise (make-json-error "Infinity is not a valid JSON number"))))
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
