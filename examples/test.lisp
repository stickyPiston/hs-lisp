(setq incf (+ 1))
(setq decf (- _ 1))
(setq zerop (= 0))

(defun id (x) x)
(setq ∅ '())

(setq : cons₁)

(defun pair (a b) '(a b))
(defun fst (p) (car p))
(defun snd (p) (car (cdr p)))

(defun-rec length (l)
  (if (null l) 0
    (incf (length (cdr l)))))

(defun-rec map (f l)
  (if (null l) ∅
    (cons₁ (f (car l)) (map f (cdr l)))))

(defun-rec filter (f l)
  (if (null l) ∅
    (if (f (car l))
      (cons₁ (car l) (filter f (cdr l)))
      (filter f (cdr l)))))

(defun-rec take (n l)
  (if (∨ (null l) (zerop n)) ∅
    (cons₁ (car l) (take (decf n) (cdr l)))))

(defun-rec drop (n l)
  (if (null l) ∅
    (if (zerop n)
      l
      (drop (decf n) (cdr l)))))

(defun-rec index (n l)
  (if (zerop n)
    (car l)
    (index (decf n) (cdr l))))
(setq ! index)

(defun-rec last (l)
  (if (= 1 (length l))
    (car l)
    (last (cdr l))))

(defun-rec init (l)
  (if (= 1 (length l)) ∅
    (cons₁ (car l) (init (cdr l)))))

(defun-rec foldr (f s l)
  (if (null l) s
    (f (car l) (foldr f s (cdr l)))))

(defun-rec foldl (f s l)
  (if (null (cdr l))
    (f s (car l))
    (f (foldl f s (cdr l)) (car l))))

(defun-rec append (a b)
  (if (null a) b
    (: (car a) (append (cdr a) b))))
(setq ++ append)

(defun-rec reverse (l)
  (if (null l) ∅
    (++ (reverse (cdr l)) '((car l)))))

(defun-rec intersperse (d l)
  (if (= 1 (length l))
    '((car l))
    (: (car l) d (intersperse d (cdr l)))))

(setq sum (foldl + 0))
(setq Σ sum)

(defun not (a) (if a #f #t))
(setq ¬ not)

(defun and (a b)
  (if a (if b #t #f) #f))
(setq ∧ and)

(defun or (a b)
  (if a #t (if b #t #f)))
(setq ∨ or)

(defun-rec zip (a b)
  (if (or (null a) (null b)) ∅
    (:
      (pair (car a) (car b))
      (zip (cdr a) (cdr b)))))

(defun-rec find (p l)
  (if (null l)
    nil
    (if (p (car l))
      (car l)
      (find p (cdr l)))))
(defun elem (e l) (¬ (= nil (find (= e) l))))
(setq ∈ elem)

(defun-rec lookup (k m)
  (if (null m) nil
    (let ((p (car m)))
      (if (= (fst p) k)
        (snd p)
        (lookup k (cdr m))))))
(setq !! lookup)

(defun-rec union (a b)
  (if (null b) a
    (let ((c (car b)))
      (if (= nil (lookup (fst c) a))
        (union (: c a) (cdr b))
        (union a (cdr b))))))
(setq ∪ union)

(defun insert (p m) (if (= nil (!! (fst p) m)) (: p m) m))

(defun-rec difference (a b)
  (if (null a) ∅
    (if (= nil (lookup (fst (car a)) b))
      (: (car a) (difference (cdr a) b))
      (difference (cdr a) b))))

(setq keys (map fst))
(setq values (map snd))

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
(print (! 3 '(1 2 3 4 5 6)))
(defun add₅ (a b c d e) (sum '(a b c d e)))
(print ((add₅ 1 _ 3 _ 5) 10 20))
