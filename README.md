# hs-lisp

Another flavour of lisp created in haskell

## Features

This lisp has
- Built-in currying
- Unicode name support
- Definition of recursive functions
- Wildcards (like `(- _ 1) ≡ (λ (x) (- x 1))`)
- Still lots of bugs

## Example

```lisp
(defun-rec foldl (f s l)
  (if (null (cdr l))
    (f s (car l))
    (f (foldl f s (cdr l)) (car l))))
(defun ∘ (f g) (λ (x) (f (g x))))
(setq ->> (foldl ∘ (λ (x) x)))
(print ((->> '((+ 1) (+ 3) (- _ 4) (* 2))) 10))
; prints 20
```

## Building

Make sure you have `cabal` installed, then run `cabal run` or `cabal repl`
