# hs-lisp

Another flavour of lisp created in haskell

## Features

This lisp has
- Built-in currying
- Unicode name support
- Definition of recursive functions
- Wildcards (like `(- _ 1) ≡ (λ (x) (- x 1))`)
- Functions for I/O (`read`, `write`, `open`, `close`)
- Still lots of bugs

## Example

```lisp
(define ∅? null)
(define-rec (foldl f s l)
  (if (∅? (cdr l))
    (f s (car l))
    (f (foldl f s (cdr l)) (car l))))
(define (∘ f g) (λ (x) (f (g x))))
(define ->> (foldl ∘ (λ (x) x)))
(print ((->> '((+ 1) (+ 3) (- _ 4) (* 2))) 10))
; prints 20
```

## Building

Make sure you have `cabal` installed, then run `cabal run` or `cabal repl`
