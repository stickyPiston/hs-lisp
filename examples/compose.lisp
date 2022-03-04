(import "examples/test.lisp")

(define (compose f g) (λ (x) (f (g x))))
(define ∘ compose)
(define pipe (foldl ∘ id))
(define ->> pipe)

(printₙ '("->> & ∘ test:" ((->> '((+ 1) (+ 3) (- _ 4) (* 2))) 10) ""))

(define ¬₂ (∘ (∘ ¬)))
(define ≤ (¬₂ >))
(define (≥ α β) (∨ (> α β) (= α β)))
(define ≠ (¬₂ =))
(define (< α β) (∧ (≤ α β) (≠ α β)))

(print ">, <, ≤, ≥ test:")
(print (> 10 20))
(print (< 10 20))
(print (≥ 10 20))
(print (≤ 30 20))
(print "")

(printₙ '("I/O test:" "Type 11 characters"))
(write (++ "\n" (++ (read 11 stdin) "\n")) stderr)
(->> '((open 'write) (write "Hello\n") close) "output.txt")
(write "Wrote to output.txt\n" stdout)
