(import (scheme base))
(import (scheme cxr))
(import (scheme eval))
(import (scheme write))
(import (scheme read))
(import (scheme file))
(import (scheme sort))
(import (scheme process-context))

(import (check))


(define filename (cadr (command-line)))

(define (filename->library-name filename)
  ;; TODO: try to guess ;)
  '(srfi 180 checks))

(define (filename->library-exports filename)
  (define library (call-with-input-file filename read))
  (let loop ((forms (cddr library))
             (out '()))
    (if (null? forms)
        out
        (if (and (pair? (car forms))
                 (eq? (caar forms) 'export))
            (loop (cdr forms) (append out (cdar forms)))
            (loop (cdr forms) out)))))

(define library-name (filename->library-name filename))

(define (check-one? library-name symbol)
  (pk library-name symbol)
  (let* ((proc (eval `,symbol (environment library-name)))
         (out (proc)))
    (if (failure? out)
        (begin (pk out) #f)
        #t)))

(if (null? (cddr (command-line)))
    (let loop ((symbols (filename->library-exports filename))
               (errors? #f))
      (if (null? symbols)
          (exit (if errors? 1 0))
          (if (check-one? library-name (car symbols))
              (begin (loop (cdr symbols) #f))
              (loop (cdr symbols) #t))))
    (check-one? library-name (string->symbol (caddr (command-line)))))
