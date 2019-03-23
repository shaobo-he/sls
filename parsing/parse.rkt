#lang racket

(require racket/set)
(require racket/file)
(require "../data/bit-vec.rkt")
(require "../data/fp.rkt")
(require "transform.rkt")

(provide (all-defined-out))

(define string->sexp
  (λ (str)
    (let ([fd (open-input-string str)])
      (define parse-string^
        (λ (fd)
          (let ([c (read fd)])
            (cond
              [(eq? c eof) '()]
              [else (cons c (parse-string^ fd))]))))
      (parameterize ([current-readtable smt-read-table])
        (parse-string^ fd)))))

(define file->sexp
  (λ (fn)
    (string->sexp (file->string fn))))

(define get-formula
  (λ (cmds)
    (let ([get-expr
              (λ (cmd result)
                (match cmd
                  [`(assert, expr) (list '∧ (transform-expr expr) result)]
                  [else result]))])
      (foldl get-expr '⊤ cmds))))

(define get-vars
  (λ (cmds)
    (let ([get-var
           (λ (cmd result)
             (match cmd
               [`(declare-const ,id (_ BitVec ,size)) (cons (cons id size) result)]
               [else result]))])
      (foldl get-var '() cmds))))

(define atom?
  (λ (sexp)
    (match sexp
      [`(= ,ops ...) #t]
      [`(bvult ,ops ...) #t]
      [`(,op ...) #f]
      [else #t])))

(define get/assertions
  (λ (F)
    (match F
      [`(∧ ,as ...) (filter list? as)]
      [_ (error "not a valid SMT formula")])))

(define get/vars
  (λ (F assignment)
    (letrec ([get/vars/do (λ (F)
                            (match F
                              [`(,op ,fs ...) (apply append (map (λ (f) (get/vars/do f)) fs))]
                              [_ (if (number? F)
                                     '()
                                     (if (hash-has-key? assignment (symbol->string F))
                                         `(,F)
                                         '()))]))])
      (set->list (list->set (get/vars/do F))))))

(define build/bvconst
  (λ (v w)
    `(_
      ,(string->symbol
        (string-append "bv" (number->string v)))
      ,w)))

(define read-hex
  (λ (v p o1 o2 o3 o4)
    (define read-hex^
      (λ (p vs)
        (define c (peek-char p))
        (if (or
             (and (char<=? #\0 c)
                  (char<=? c #\9))
             (or
              (and (char<=? #\a c)
                   (char<=? c #\f))
              (and (char<=? #\A c)
                   (char<=? c #F))))
            (begin
              (read-char p)
              (read-hex^ p (string-append vs (string c))))
            vs)))
    (let ([str (read-hex^ p "")])
      (build/bvconst
       (string->number (string-append "#x" str))
       (* 4 (string-length str))))))

(define read-bin
  (λ (v p o1 o2 o3 o4)
    (define read-bin^
      (λ (p vs)
        (define c (peek-char p))
        (if (and (char<=? #\0 c)
                 (char<=? c #\1))
            (begin
              (read-char p)
              (read-bin^ p (string-append vs (string c))))
            vs)))
    (let ([str (read-bin^ p "")])
      (build/bvconst
       (string->number (string-append "#b" str))
       (string-length str)))))

(define smt-read-table
  (make-readtable
   (make-readtable
    #f
    #\x
    'dispatch-macro
    read-hex)
   #\b
   'dispatch-macro
   read-bin))