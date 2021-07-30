#lang racket
(provide comp_)

;; returns a function that composes parameters in order,
;; using a placeholder _ for passing values between functions.
(define-syntax (comp_ stx)
  ; macro to compose functions passing an '_' parameter
  (syntax-case stx ()
    ((_ f1 ...)
     (with-syntax ([x-var (datum->syntax stx '_)])
       #'(apply compose1 (reverse (list (λ (x-var) f1) ...)))))))