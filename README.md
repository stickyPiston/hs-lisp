# hs-lisp

Another flavour of lisp created in haskell

```lisp
(defun union (a b) 
  (if (null b)
    a
    (let ((c (car b)))
      (if (= nil (lookup (fst c) a))
        (union (cons c a) (cdr b))
        (union a (cdr b))))))
(print (union
         '((pair 'a 10) (pair 'b 20))
         '((pair 'b 30) (pair 'c 30))))
; prints (('a 10) ('b 20) ('c 30))
```

## Building

Make sure you have `cabal` installed, then run `cabal run`
