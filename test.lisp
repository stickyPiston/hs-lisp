(setq incf (+ 1))
(defun decf (a) (- a 1))

(defun length (l)
  (if (null l)
    0
    (incf (length (cdr l)))))

(defun map (f l)
  (if (null l)
    '()
    (cons (f (car l)) (map f (cdr l)))))

(defun filter (f l)
  (if (null l)
    '()
    (if (f (car l))
      (cons (car l) (filter f (cdr l)))
      (filter f (cdr l)))))

(defun zerop (n) (= n 0))

(defun take (n l)
  (if (or (null l) (zerop n))
    '()
    (cons (car l) (take (decf n) (cdr l)))))

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
    (cons (car l) (init (cdr l)))))

(defun append (a b)
  (if (null a)
    b
    (cons (car a) (append (cdr a) b))))

(defun reverse (l)
  (if (null l)
    '()
    (append (reverse (cdr l)) '((car l)))))

(defun intersperse (d l)
  (if (= 1 (length l))
    '((car l))
    (cons (car l) d (intersperse d (cdr l)))))

(setq sum (foldl + 0))

(print (sum '(1 2 3 4)))
