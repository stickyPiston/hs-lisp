(define (id x) x)
(define ∅ '())
(define ∅? null)
(define zero? (= 0))

(define : cons)

(define (pair a b) '(a b))
(define (fst p) (car p))
(define (snd p) (car (cdr p)))

(define-rec (length l)
  (if (∅? l) 0
    (+ 1 (length (cdr l)))))

(define-rec (map f l)
  (if (∅? l) ∅
    (: (f (car l)) (map f (cdr l)))))

(define-rec (filter f l)
  (if (∅? l) ∅
    (if (f (car l))
      (: (car l) (filter f (cdr l)))
      (filter f (cdr l)))))

(define-rec (take n l)
  (if (∨ (∅? l) (zero? n)) ∅
    (: (car l) (take (- n 1) (cdr l)))))

(define-rec (drop n l)
  (if (∅? l) ∅
    (if (zero? n)
      l
      (drop (- n 1) (cdr l)))))

(define-rec (index n l)
  (if (zero? n)
    (car l)
    (index (- n 1) (cdr l))))
(define ! index)

(define-rec (last l)
  (if (= 1 (length l))
    (car l)
    (last (cdr l))))

(define-rec (init l)
  (if (= 1 (length l)) ∅
    (: (car l) (init (cdr l)))))

(define-rec (foldr f s l)
  (if (∅? l) s
    (f (car l) (foldr f s (cdr l)))))

(define-rec (foldl f s l)
  (if (∅? l) s
    (if (∅? (cdr l))
      (f s (car l))
      (f (foldl f s (cdr l)) (car l)))))

(define-rec (append a b)
  (if (∅? a) b
    (: (car a) (append (cdr a) b))))
(define ++ append)

(define-rec (reverse l)
  (if (∅? l) ∅
    (++ (reverse (cdr l)) '((car l)))))

(define-rec (intersperse d l)
  (if (= 1 (length l))
    '((car l))
    (: (car l) (: d (intersperse d (cdr l))))))

(define sum (foldl + 0))
(define Σ sum)
(define product (foldl * 1))
(define Π product)

(define (not a) (if a #f #t))
(define ¬ not)

(define (and a b)
  (if a (if b #t #f) #f))
(define ∧ and)

(define (or a b)
  (if a #t (if b #t #f)))
(define ∨ or)

(define-rec (zip a b)
  (if (∨ (∅? a) (∅? b)) ∅
    (:
      (pair (car a) (car b))
      (zip (cdr a) (cdr b)))))

(define-rec (find p l)
  (if (∅? l) ∅
    (if (p (car l))
      (car l)
      (find p (cdr l)))))
(define (elem e l) (¬ (= ∅ (find (= e) l))))
(define ∈ elem)

(define-rec (lookup k m)
  (if (∅? m) ∅
    (let ((p (car m)))
      (if (= (fst p) k)
        (snd p)
        (lookup k (cdr m))))))
(define !! lookup)

(define-rec (union a b)
  (if (∅? b) a
    (let ((c (car b)))
      (if (= ∅ (!! (fst c) a))
        (union (: c a) (cdr b))
        (union a (cdr b))))))
(define ∪ union)

(define (insert p m) (if (= ∅ (!! (fst p) m)) (: p m) m))

(define-rec (difference a b)
  (if (∅? a) ∅
    (if (= ∅ (!! (fst (car a)) b))
      (: (car a) (difference (cdr a) b))
      (difference (cdr a) b))))
(define \ difference)

(define keys (map fst))
(define values (map snd))

(define (const a b) a)
(define-rec (printₙ xs)
  (if (∅? xs) ∅
    (printₙ (const (cdr xs)
      (print (car xs))))))
