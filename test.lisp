(setq incf (+ 1))
(setq decf (- _ 1))

(defun id (x) x)

; Aliases
(defun not (a) (if a #f #t))
(setq ¬ not)
(setq ∧ and)
(setq ∨ or)
(setq : cons)

; Pair helper functions

(defun pair (a b) (list a b))
(defun fst (p) (car p))
(defun snd (p) (car (cdr p)))

; List helper functions

(defun length (l)
  (if (null l)
    0
    (incf (length (cdr l)))))

(defun map (f l)
  (if (null l)
    '()
    (: (f (car l)) (map f (cdr l)))))

(defun filter (f l)
  (if (null l)
    '()
    (if (f (car l))
      (: (car l) (filter f (cdr l)))
      (filter f (cdr l)))))

(defun zerop (n) (= n 0))

(defun take (n l)
  (if (∨ (null l) (zerop n))
    '()
    (: (car l) (take (decf n) (cdr l)))))

(defun drop (n l)
  (if (null l) '()
    (if (zerop n)
      l
      (drop (decf n) (cdr l)))))

(defun index (n l)
  (if (zerop n)
    (car l)
    (index (decf n) (cdr l))))

(defun foldl (f s l)
  (if (= 1 (length l))
    (f s (car l))
    (f (foldl f s (cdr l)) (car l))))

(defun last (l)
  (if (= 1 (length l))
    (car l)
    (last (cdr l))))

(defun init (l)
  (if (= 1 (length l))
    '()
    (: (car l) (init (cdr l)))))

(defun append (a b)
  (if (null a)
    b
    (: (car a) (append (cdr a) b))))
(setq ++ append)

(defun reverse (l)
  (if (null l)
    '()
    (++ (reverse (cdr l)) '((car l)))))

(defun intersperse (d l)
  (if (= 1 (length l))
    '((car l))
    (: (car l) d (intersperse d (cdr l)))))

(setq sum (foldl + 0))
(setq Σ sum)

(defun zip (a b)
  (if (or (null a) (null b))
    '()
    (:
      (pair (car a) (car b))
      (zip (cdr a) (cdr b)))))

(defun find (p l)
  (if (null l)
    nil
    (if (p (car l))
      #t
      (find p (cdr l)))))
(defun elem (e l) (¬ (= nil (find (= e) l))))
(setq ∈ elem)

; Map helper functions

(defun lookup (k m)
  (if (null m) nil
    (let ((p (car m)))
      (if (= (fst p) k)
        (snd p)
        (lookup k (cdr m))))))
(setq !! lookup)

(defun union (a b) 
  (if (null b)
    a
    (let ((c (car b)))
      (if (= nil (lookup (fst c) a))
        (union (: c a) (cdr b))
        (union a (cdr b))))))
(setq ∪ union)

(defun insert (p m) (if (= nil (!! (fst p) m)) (: p m) m))

(defun difference (a b)
  (if (null a)
    '()
    (if (= nil (lookup (fst (car a)) b))
      (: (car a) (difference (cdr a) b))
      (difference (cdr a) b))))

(setq keys (map fst))
(setq values (map snd))

; Main logic

(print (∪
         '((pair 'a 10) (pair 'b 20))
         '((pair 'b 30) (pair 'c 30))))
(print (difference
         '((pair 'a 10) (pair 'b 20))
         '((pair 'b 30) (pair 'c 30))))
(print (keys '((pair 'a 10) (pair 'b 20))))
(print (values '((pair 'a 10) (pair 'b 20))))

(setq m '((pair 'a 10) (pair 'b 20)))
(print (zip (keys m) (values m)))

(defun compose₂ (f g) (λ (x) (f (g x)))) 
(defun compose fs (foldl compose₂ id fs))
(setq ∘ compose)

(print ((∘ (+ 1) (- _ 10) (+ 8)) 20))
(print (∈ "Hello" '("hello" "world")))
