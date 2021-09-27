#!

(import (scheme base) (scheme file) (scheme read) (scheme write)
        (srfi 1) (srfi 193))

(define rest cdr)

(define-record-type ff-type
  (make-ff-type name)
  ff-type?
  (name ff-type-name))

(define-record-type ff-constant
  (make-ff-constant name value)
  ff-constant?
  (name ff-constant-name)
  (value ff-constant-value))

(define-record-type ff-proc
  (make-ff-proc name tuple-in tuple-out)
  ff-proc?
  (name ff-proc-name)
  (tuple-in ff-proc-tuple-in)
  (tuple-out ff-proc-tuple-out))

(define (for-each/between visit between xs)
  (unless (null? xs)
    (visit (first xs))
    (for-each (lambda (x) (between) (visit x)) (rest xs))))

(define (string-index string char)
  (let loop ((i 0))
    (cond ((= i (string-length string)) #f)
          ((char=? char (string-ref string i)) i)
          (else (loop (+ i 1))))))

(define (string-join list delimiter)
  (if (null? list) ""
      (let loop ((result (car list)) (list (cdr list)))
        (if (null? list)
            result
            (loop (string-append result delimiter (car list))
                  (cdr list))))))

(define (generator->list g)
  (let loop ((xs '()))
    (let ((x (g)))
      (if (eof-object? x) (reverse xs) (loop (cons x xs))))))

(define (read-all) (generator->list read))

(define (disp . xs) (for-each display xs) (newline))

(define (halve-at sep xs)
  (let loop ((left '()) (xs xs))
    (cond ((null? xs)
           (values (reverse left) '()))
          ((equal? sep (first xs))
           (values (reverse left) (rest xs)))
          (else
           (loop (cons (first xs) left) (rest xs))))))

(define (with-input-from-string string proc)
  (call-with-port (open-input-string string)
                  (lambda (port)
                    (parameterize ((current-input-port port))
                      (proc)))))

(define (with-output-to-string proc)
  (call-with-port (open-output-string)
                  (lambda (port)
                    (parameterize ((current-output-port port))
                      (proc))
                    (get-output-string port))))

(define (with-string-io string proc)
  (with-input-from-string string (lambda () (with-output-to-string proc))))

(define (mangle-ascii from-chars to-chars)
  (lambda (symbol)
    (with-string-io
     (symbol->string symbol)
     (lambda ()
       (let loop ()
         (let ((char (read-char)))
           (unless (eof-object? char)
             (let ((i (string-index from-chars char)))
               (write-char (if i (string-ref to-chars i) char))
               (loop)))))))))

;;

(define ff-module-name (make-parameter #f))

;;

(define c-mangle (mangle-ascii "-" "_"))

(define (emit-c-header)
  (disp "/*! Generator: superglue */")
  (disp))

(define (emit-c-type x)
  (disp "struct ff_" (c-mangle (ff-module-name))
        "_" (c-mangle (ff-type-name x)) ";"))

(define (emit-c-constant x)
  (values))

(define (emit-c-procedure x)
  (define mapping
    '((string "const char *")
      (symbol "const char *")
      (nat "uint64_t ")
      (int "int64_t ")
      (real "double ")
      (bool "bool ")))
  (define (c-name-type->string addition)
    (lambda (name-type)
      (let ((name (first name-type))
            (t (second name-type)))
        (if (and (list? t) (= 2 (length t)) (eq? 'list (first t)))
            ((c-name-type->string (string-append addition "*"))
             (list name (second t)))
            (let ((entry (assq t mapping)))
              (cond (entry
                     (string-append (second entry) addition
                                    (c-mangle name)))
                    ((symbol? t)
                     (string-append "struct ff_" (c-mangle (ff-module-name))
                                    "_" (c-mangle t)
                                    " *" addition (c-mangle name)))
                    (else
                     (error "Foo" t))))))))
  (disp "struct ff_error *"
        "ff_" (c-mangle (ff-module-name)) "_" (c-mangle (ff-proc-name x))
        "("
        (string-join (append
                      (map (c-name-type->string "")
                           (ff-proc-tuple-in  x))
                      (map (c-name-type->string "*out_")
                           (ff-proc-tuple-out x)))
                     ", ")
        ");"))

;;

(define sml-mangle (mangle-ascii "-" "_"))

(define (emit-sml-header)
  (disp "(*! Generator: superglue *)")
  (disp))

(define (emit-sml-type x)
  (values))

(define (emit-sml-constant x)
  (values))

(define (emit-sml-procedure x)
  (define (sml-type->string t)
    (cond ((and (list? t) (= 2 (length t)) (eq? 'list (first t)))
           (string-append (sml-type->string (second t)) " list"))
          ((memq t '(string symbol nat int real bool))
           (sml-mangle t))
          ((symbol? t)
           (string-append
            "ff." (sml-mangle (ff-module-name)) "." (sml-mangle t)))
          (else
           (error "Foo" t))))
  (define (sml-tuple->string tuple)
    (let ((tuple (map sml-type->string (map second tuple))))
      (case (length tuple)
        ((0) "unit")
        ((1) (first tuple))
        (else (string-join tuple " * ")))))
  (define (sml-result-tuple->string tuple)
    (string-append (sml-tuple->string tuple) " ff.result"))
  (disp "val " (sml-mangle (ff-proc-name x)) " : "
        (sml-tuple->string (ff-proc-tuple-in  x)) " -> "
        (sml-result-tuple->string (ff-proc-tuple-out x)) ";"))

(define make-emitter list)
(define emitter-name first)
(define emitter-emit-header second)
(define emitter-emit-type third)
(define emitter-emit-constant fourth)
(define emitter-emit-procedure fifth)
(define (emitter-by-name name) (assoc name emitters))

(define emitters
  (list (make-emitter 'c
                      emit-c-header
                      emit-c-type
                      emit-c-constant
                      emit-c-procedure)
        (make-emitter 'sml
                      emit-sml-header
                      emit-sml-type
                      emit-sml-constant
                      emit-sml-procedure)))

(define (emit emitter x)
  (cond ((ff-type? x)
         ((emitter-emit-type emitter) x))
        ((ff-constant? x)
         ((emitter-emit-constant emitter) x))
        ((ff-proc? x)
         ((emitter-emit-procedure emitter) x))
        (else
         (error "Bad" x))))

(define (parse form)
  (case (first form)
    ((type)
     (let ((name (second form)))
       (make-ff-type name)))
    ((constant)
     (let ((name (second form))
           (value 0))  ; TODO
       (make-ff-constant name value)))
    ((procedure)
     (let ((name (second form)))
       (let-values (((tuple-in tuple-out) (halve-at '-> (list-tail form 2))))
         (make-ff-proc name tuple-in tuple-out))))
    (else
     (error "Bad form:" form))))

(define (main)
  (let* ((target (first (command-args)))
         (modules (rest (command-args)))
         (emitter (or (emitter-by-name (string->symbol target))
                      (error "No such target" target))))
    (for-each (lambda (module)
                (let* ((pose (string-append "ff_" module ".pose"))
                       (module (string->symbol module))
                       (parsed (map parse (with-input-from-file pose
                                            read-all))))
                  (parameterize ((ff-module-name module))
                    ((emitter-emit-header emitter))
                    (for-each (lambda (x) (emit emitter x))
                              parsed))))
              modules)))

(main)
