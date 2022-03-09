(import "examples/test.lisp")

(define (compose f g) (λ (x) (f (g x))))
(define ∘ compose)
(define pipe (foldl ∘ id))
(define ->> pipe)

(define ¬₂ (∘ (∘ ¬)))
(define ≤ (¬₂ >))
(define (≥ α β) (∨ (> α β) (= α β)))
(define ≠ (¬₂ =))
(define (< α β) (∧ (≤ α β) (≠ α β)))

(define naive-join (∘ (foldl ++ "") reverse))

(define n→s num->string)
(define (flip f x y) (f y x))
(define (main args) (foldl (flip write) stdout
  (reverse '("hs-lisp test script (incomplete edition™)\n\n"
             "Arithmetic test:\n"
             "10 + 20 = " (n→s (+ 10 20)) "\n"
             "60 / 3 = " (n→s (/ 60 3)) "\n"
             "Args test: \n" (naive-join args) "\n"
             "->> & _ test:\n((->> (* 2) (- _ 5) (/ _ 3)) 10) = "
             (n→s ((->> '((* 2) (- _ 5) (/ _ 3))) 10)) "\n"))))
