(define ∅? null)
(define-rec (foldl f s l)
  (if (∅? (cdr l))
    (f s (car l))
    (f (foldl f s (cdr l)) (car l))))
(define (∘ f g) (λ (x) (f (g x))))
(define ->> (foldl ∘ (λ (x) x)))
(define (main args) (print ((->> '((+ 1) (+ 3) (- _ 4) (* 2))) 10)))
; prints 20
