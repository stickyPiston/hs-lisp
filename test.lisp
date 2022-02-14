(defun incf (x) (+ x 1))
(defun decf (x) (- x 1))
(defun zerop (n) (= n 0))

(defun id (x) x)

(defun not (a) (if a #f #t))
(setq ¬ not)

(defun and₂ (a b)
  (if a (if b #t #f) #f))
(defun and as (foldl and₂ #t as))
(setq ∧ and)

(defun or₂ (a b)
  (if a #t (if b #t #f)))
(defun or as (foldl or₂ #f as))
(setq ∨ or)

(defun pair (a b) '(a b))
(defun fst (p) (car p))
(defun snd (p) (car (cdr p)))

(defun length (l)
  (if (null l) 0
    (incf (length (cdr l)))))

(defun map (f l)
  (if (null l)
    '()
    (cons₁ (f (car l)) (map f (cdr l)))))

(defun filter (f l)
  (if (null l)
    '()
    (if (f (car l))
      (cons₁ (car l) (filter f (cdr l)))
      (filter f (cdr l)))))

(defun take (n l)
  (if (∨ (null l) (zerop n))
    '()
    (cons₁ (car l) (take (decf n) (cdr l)))))

(defun drop (n l)
  (if (null l) '()
    (if (zerop n)
      l
      (drop (decf n) (cdr l)))))

(defun index (n l)
  (if (zerop n)
    (car l)
    (index (decf n) (cdr l))))
(setq ! index)

(defun last (l)
  (if (= 1 (length l))
    (car l)
    (last (cdr l))))

(defun init (l)
  (if (= 1 (length l))
    '()
    (cons₁ (car l) (init (cdr l)))))

(defun foldr (f s l)
  (if (null l) s
    (f (car l) (foldr f s (cdr l)))))

(defun foldl (f s l)
  (if (= 1 (length l))
    (f s (car l))
    (f (foldl f s (cdr l)) (car l))))

(defun append (a b)
  (if (null a)
    b
    (: (car a) (append (cdr a) b))))
(setq ++ append)

(defun reverse (l)
  (if (null l)
    '()
    (++ (reverse (cdr l)) '((car l)))))

(defun cons as (foldr cons₁ (last as) (init as)))
(setq : cons)

(defun intersperse (d l)
  (if (= 1 (length l))
    '((car l))
    (: (car l) d (intersperse d (cdr l)))))

(defun sum (l) (foldl + 0 l))
(setq Σ sum)

(defun zip (a b)
  (if (or (null a) (null b))
    '()
    (:
      (pair (car a) (car b))
      (zip (cdr a) (cdr b)))))

(defun find (p l)
  (if (null l)
    #nil
    (if (p (car l))
      (car l)
      (find p (cdr l)))))
(defun elem (e l) (¬ (= nil (find (= e) l))))
(setq ∈ elem)

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

(defun keys (m) (map fst m))
(defun values (m) (map snd m))

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
(print (snd (pair 10 20)))

