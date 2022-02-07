module Grammar where

import Parser
import Value
import Data.Either (isLeft, lefts)

type Errorable a = Either [String] a

verifyGrammar :: [Atom] -> Errorable [Atom]
verifyGrammar = sequence . map verifyGrammarForAtom

verifyGrammarForAtom :: Atom -> Errorable Atom
verifyGrammarForAtom a@(List [Identifier "lambda", List args, e]) = 
  let params = map verifyName args
      be     = verifyGrammarForAtom e 
      es     = either (lefts params ++) (const $ lefts params) be
   in if length es > 0
        then Left es
        else Right a
  where
    verifyName (Identifier _) = Right ()
    verifyName v              = Left $ "Expected identifier, got " ++ typeof v

verifyGrammarForAtom a@(List [Identifier "defun", Identifier n, List args, e]) =
  either (Left . id) (const $ Right a) $ verifyGrammarForAtom (List [Identifier "setq", Identifier n, List [Identifier "lambda", List args, e]])

verifyGrammarForAtom a@(List [Identifier "setq", n, e]) =
  case n of
    Identifier _ -> either (Left . id) (const $ Right a) $ verifyGrammarForAtom e
    _ -> Left ["Expected identifier, but got " ++ typeof n]

verifyGrammarForAtom a@(List [Identifier "if", cond, t, e]) =
  let es = map verifyGrammarForAtom [cond, t, e]
   in if any isLeft es
         then Left . concat $ lefts es
         else Right a

verifyGrammarForAtom a@(List [Identifier "let", List bindings, prog]) =
  let vars = map verifyBinding bindings
      pe   = verifyGrammarForAtom prog
      es   = either ((concat $ lefts vars) ++) (const . concat $ lefts vars) pe
   in if length es > 0
        then Left es
        else Right a
  where
    verifyBinding (List [Identifier _, e]) = verifyGrammarForAtom e
    verifyBinding e = Left $ ["Expected let binding, got " ++ typeof e]

verifyGrammarForAtom a@(Identifier _) = Right a
verifyGrammarForAtom a@(Number _) = Right a
verifyGrammarForAtom a@(StringLiteral _) = Right a
verifyGrammarForAtom a@(Bool _) = Right a
verifyGrammarForAtom a@(Wildcard) = Right a

verifyGrammarForAtom a@(Quote v) =
  if any (== typeof v) ["identifier", "list", "number"]
     then Right a
     else Left ["Quoting this value is not allowed"]

verifyGrammarForAtom a@(List (f : args)) =
  case f of
    Identifier _ -> verifyArguments args
    List _ -> verifyArguments args
    v -> Left ["Expected an identifier or list, but got " ++ typeof v]
    where
      verifyArguments args =
        let eas = map verifyGrammarForAtom args
            es  = concat $ lefts eas
         in if length es > 0
               then Left es
               else Right a

verifyGrammarForAtom v = Left ["Unexpected " ++ typeof v]
