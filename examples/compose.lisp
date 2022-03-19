(import-some-as (∧ ∨ ¬ ∅ ∅? id foldl reverse ++ map) lib "examples/test.lisp")

(define (compose f g x) (f (g x)))
(define ∘ compose)
(define pipe (lib:foldl ∘ lib:id))
(define ->> pipe)

(define ¬₂ (∘ (∘ lib:¬)))
(define ≤ (¬₂ >))
(define (≥ α β) (lib:∨ (> α β) (= α β)))
(define ≠ (¬₂ =))
(define (< α β) (lib:∧ (≤ α β) (≠ α β)))

(define naive-join (∘ (lib:foldl lib:++ "") lib:reverse))
(define (join-with c l)
  (if (lib:∅? l) ""
    ((->> '(cdr
            lib:reverse
            (lib:foldl (λ (ac) (λ (s) (lib:++ ac (lib:++ c s)))) "")
            (lib:++ (car l)))) l)))
(define ←ascii from-ascii)
(define →ascii to-ascii)

(define-rec (n→s n)
  (if (= 0 n) lib:∅
    (lib:++ (n→s (/ n 10)) (←ascii (+ (% n 10) 48)))))

(define (bool-e t e b) (if b t e))
(define b→s (bool-e "#t" "#f"))

(define all (lib:foldl lib:∧ #t))
(define (all-p p xs) (all (lib:map p xs)))
(define (l→s l)
  (if (all-p (∘ (= 'char) typeof) l) l
    (lib:++ (lib:++ "(" (join-with ", " (lib:map →s l))) ")")))

(define (→s t)
  (let ((ty (typeof t)))
    (if (= ty 'number) (n→s t)
    (if (= ty 'list) (l→s t)
    (if (= ty 'boolean) (b→s t)
    (if (= ty 'symbol) (lib:++ "'" (sym->s t))
    (if (= ty 'char) (lib:++ "'" (lib:++ '(t) "'"))
    (if (= ty 'lambda) "(lambda)"
    (if (= ty 'intrinsic) "(intrinsic)"
    (if (= ty 'handle) "(handle)" lib:∅))))))))))
(define to-string →s)

(define (flip f x y) (f y x))
(define (main args) (lib:foldl (flip write) stdout 
  (lib:reverse '("hs-lisp test script (incomplete edition™)\n\n"
             "Arithmetic test:\n"
             "10 + 20 = " (→s (+ 10 20)) "\n"
             "60 / 3 = " (→s (/ 60 3)) "\n"
             "15 % 4 = " (→s (% 15 4)) "\n"
             "Args test: \n" (join-with ", " args) "\n"
             "->> & _ test:\n((->> (* 2) (- _ 5) (/ _ 3)) 10) = "
             (→s ((->> '((* 2) (- _ 5) (/ _ 3))) 10)) "\n"
             "→s test: " (→s '(1 2 3)) " " (→s 'symbol)
             " " (→s stdout) "\n"
             "Another _ test: ((_ 10) (+ 5)) = " (→s ((_ 10) (+ 5))) "\n"
             "_ param test: " ((λ (_) "works") 10) "\n"))))
