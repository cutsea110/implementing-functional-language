module PrettyPrint
  ( pprint
  ) where

import Data.List (intersperse)

import Language
import Utils

data Iseq = INil
          | IStr String
          | IAppend Iseq Iseq
          | IIndent Iseq
          | INewline

iNil     :: Iseq                             -- ^ The empty iseq
iStr     :: String -> Iseq                   -- ^ Turn a string into an iseq
iAppend  :: Iseq -> Iseq -> Iseq             -- ^ Append two iseqs
iNewline :: Iseq                             -- ^ New line with indentation
iIndent  :: Iseq -> Iseq                     -- ^ Indent an iseq
iDisplay :: Iseq -> String                   -- ^ Turn an iseq into a string

iNil     = INil
iStr str = IStr str
iAppend seq1 seq2 = IAppend seq1 seq2
iNewline = INewline
iIndent seq = IIndent seq
iDisplay seq = flatten 0 [(seq, 0)]

flatten :: Int                   -- ^ Current column; 0 for first column
        -> [(Iseq, Int)]         -- ^ Work list
        -> String                -- ^ Result
flatten col [] = ""
flatten col ((INil, indent):seqs)
  = flatten indent seqs
flatten col ((IStr s, indent):seqs)
  = s ++ flatten (length s + indent) seqs
flatten col ((IAppend seq1 seq2, indent):seqs)
  = flatten indent ((seq1, indent):(seq2, indent):seqs)
flatten col ((IIndent seq, indent):seqs)
  = flatten col ((seq, col):seqs)
flatten col ((INewline, indent):seqs)
  = '\n' : (space indent) ++ (flatten indent seqs)

space :: Int -> String
space n = take n (repeat ' ')

pprExpr :: CoreExpr -> Iseq
pprExpr (ENum n) = iStr (show n)
pprExpr (EVar v) = iStr v
pprExpr (EAp e1 e2) = (pprExpr e1) `iAppend` (iStr " ") `iAppend` (pprAExpr e2)
pprExpr (ELet isrec defns expr)
  = iConcat [ iStr keyword, iNewline
            , iStr "  ", iIndent (pprDefns defns), iNewline
            , iStr "in ", pprExpr expr
            ]
  where keyword = if isrec then "letrec" else "let"

pprAExpr :: CoreExpr -> Iseq
pprAExpr e | isAtomicExpr e = pprExpr e
           | otherwise      = iConcat [iStr "(", pprExpr e, iStr ")"]

pprDefns :: [(Name, CoreExpr)] -> Iseq
pprDefns defns = iInterleave sep (map pprDefn defns)
  where sep = iConcat [ iStr ";", iNewline ]

pprDefn :: (Name, CoreExpr) -> Iseq
pprDefn (name, expr)
  = iConcat [ iStr name, iStr " = ", iIndent (pprExpr expr) ]

iConcat :: [Iseq] -> Iseq
iConcat seqs = foldl iAppend iNil seqs

iInterleave :: Iseq -> [Iseq] -> Iseq
iInterleave sep seqs = iConcat (intersperse sep seqs)

pprint :: CoreProgram -> String
pprint prog = iDisplay (pprProgram prog)

pprProgram :: CoreProgram -> Iseq
pprProgram scdefns = iInterleave iNewline (map pprScDefn scdefns)

pprScDefn :: (Name, [Name], CoreExpr) -> Iseq
pprScDefn (name, args, expr)
  = iConcat [ iInterleave (iStr " ") (map iStr (name:args))
            , iStr " = "
            , iIndent (pprExpr expr)
            ]
