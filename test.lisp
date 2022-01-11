(defun incr (n) (+ n 1))
(defun decr (n) (- n 1))

(defun len (l)
  (if (empty? l) 0
    (incr (len (cdr l)))))

(defun map (f l)
  (if (empty? l) ()
    (cons (f (car l)) (map f (cdr l)))))

(defun filter (f l)
  (if (empty? l) ()
  (if (f (car l))
    (cons (car l) (filter f (cdr l)))
    (filter f (cdr l)))))

(defun zero? (n) (= n 0))

(defun take (n l)
  (if (or (empty? l) (zero? n)) ()
    (cons (car l) (take (decr n) (cdr l)))))

(defun drop (n l)
  (if (empty? l) ()
    (if (zero? n) l
      (drop (decr n) (cdr l)))))

(print (drop 2 '(1 2 3 4)))
