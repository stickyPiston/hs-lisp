(import "examples/test.lisp")

(defun compose (f g) (λ (x) (f (g x))))
(setq ∘ compose)
(setq pipe (foldl ∘ id))
(setq ->> pipe)

(print ((->> '((+ 1) (+ 3) (- _ 4) (* 2))) 10))

(setq <= (∘ (∘ not) >))
(defun >= (a b) (∨ (> a b) (= a b)))
(setq ≠ (∘ (∘ not) =))
(defun < (a b) (∧ (<= a b) (≠ a b)))

(print (> 10 20))
(print (< 10 20))
(print (>= 10 20))
(print (<= 30 20))

(write (++ "\n" (++ (read stdin 11) "\n")) stderr)
(->> '((open 'write) (write "Hello\n") close) "output.txt")
(write "Wrote to output.txt\n" stdout)

(setq char car)
(setq ascii-table
  '(
    '(32 (char " ")) '(33 (char "!"))
    '(34 (char "\"")) '(35 (char "#"))
    '(36 (char "$")) '(37 (char "%"))
    '(38 (char "&")) '(39 (char "'"))
    '(40 (char "(")) '(41 (char ")"))
    '(42 (char "*")) '(43 (char "+"))
    '(44 (char ",")) '(45 (char "-"))
    '(46 (char ".")) '(47 (char "/"))
    '(48 (char "0")) '(49 (char "1"))
    '(50 (char "2")) '(51 (char "3"))
    '(52 (char "4")) '(53 (char "5"))
    '(54 (char "6")) '(55 (char "7"))
    '(56 (char "8")) '(57 (char "9"))))

(defun to-ascii (c)
  (fst (find (λ (x) (= c (snd x))) ascii-table)))

(print (to-ascii (char ",")))
