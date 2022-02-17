(import "examples/test.lisp")

(defun compose (f g) (λ (x) (f (g x))))
(setq ∘ compose)
(setq pipe (foldl ∘ id))
(setq ->> pipe)

(print ((->> '((+ 1) (+ 3) (- _ 4) (* 2))) 10))

(setq < (∘ (∘ not) >))
(defun >= (a b) (∨ (> a b) (= a b)))
(defun <= (a b) (∨ (< a b) (= a b)))

(print (> 10 20))
(print (< 10 20))
(print (>= 10 20))
(print (<= 30 20))
