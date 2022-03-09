(import "examples/test.lisp")

(define (compose f g x) (f (g x)))
(define ∘ compose)
(define pipe (foldl ∘ id))
(define ->> pipe)

(define ¬₂ (∘ (∘ ¬)))
(define ≤ (¬₂ >))
(define (≥ α β) (∨ (> α β) (= α β)))
(define ≠ (¬₂ =))
(define (< α β) (∧ (≤ α β) (≠ α β)))

(define naive-join (∘ (foldl ++ "") reverse))
(define (join-with c l)
  (if (∅? l) ""
    ((->> '(cdr
            reverse
            (foldl (λ (ac) (λ (s) (++ ac (++ c s)))) "")
            (++ (car l)))) l)))
(define ←ascii from-ascii)
(define →ascii to-ascii)

(define-rec (n→s n)
  (if (zero? n) ∅
    (++ (n→s (/ n 10)) (←ascii (+ (% n 10) 48)))))

(define (bool-e t e b) (if b t e))
(define b→s (bool-e "#t" "#f"))

(define all (foldl ∧ #t))
(define (all-p p xs) (all (map p xs)))
(define (l→s l)
  (if (all-p (∘ (= 'char) typeof) l) l
    (++ (++ "(" (join-with ", " (map →s l))) ")")))

(define (→s t)
  (let ((ty (typeof t)))
    (if (= ty 'number) (n→s t)
    (if (= ty 'list) (l→s t)
    (if (= ty 'boolean) (b→s t)
    (if (= ty 'symbol) (++ "'" (sym->s t))
    (if (= ty 'char) (++ "'" (++ '(t) "'"))
    (if (= ty 'lambda) "(lambda)"
    (if (= ty 'intrinsic) "(intrinsic)"
    (if (= ty 'handle) "(handle)" ∅))))))))))
(define to-string →s)

(define (flip f x y) (f y x))
(define (main args) (foldl (flip write) stdout
  (reverse '("hs-lisp test script (incomplete edition™)\n\n"
             "Arithmetic test:\n"
             "10 + 20 = " (→s (+ 10 20)) "\n"
             "60 / 3 = " (→s (/ 60 3)) "\n"
             "15 % 4 = " (→s (% 15 4)) "\n"
             "Args test: \n" (join-with ", " args) "\n"
             "->> & _ test:\n((->> (* 2) (- _ 5) (/ _ 3)) 10) = "
             (→s ((->> '((* 2) (- _ 5) (/ _ 3))) 10)) "\n"
             "→s test: " (→s '(1 2 3)) " " (→s 'symbol)
             " " (→s stdout) "\n"))))
