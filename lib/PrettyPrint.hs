module PrettyPrint where

import Data.List (intersperse)

import Language
import Utils

data Iseq = INil
          | IStr String
          | IAppend Iseq Iseq

iNil     :: Iseq                             -- ^ The empty iseq
iStr     :: String -> Iseq                   -- ^ Turn a string into an iseq
iAppend  :: Iseq -> Iseq -> Iseq             -- ^ Append two iseqs
iNewline :: Iseq                             -- ^ New line with indentation
iIndent  :: Iseq -> Iseq                     -- ^ Indent an iseq
iDisplay :: Iseq -> String                   -- ^ Turn an iseq into a string

iNil     = INil
iStr str = IStr str
iAppend seq1 seq2 = IAppend seq1 seq2
iNewline = IStr "\n"
iIndent seq = seq
iDisplay seq = flatten [seq]

flatten :: [Iseq] -> String
flatten []                       = ""
flatten (INil:seqs)              = flatten seqs
flatten (IStr s:seqs)            = s ++ flatten seqs
flatten (IAppend seq1 seq2:seqs) = flatten (seq1:seq2:seqs)

pprExpr :: CoreExpr -> Iseq
pprExpr (ENum n) = iStr (show n)
pprExpr (EVar v) = iStr v
pprExpr (EAp e1 e2) = (pprExpr e1) `iAppend` (iStr " ") `iAppend` (pprAExpr e2)
pprExpr (ELet isrec defns expr)
  = iConcat [ iStr keyword, iNewline
            , iStr "  ", iIndent (pprDefns defns), iNewline
            , iStr "in", pprExpr expr
            ]
  where keyword | not isrec = "let"
                | isrec     = "letrec"

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
pprProgram scdefns = iConcat (map pprScDefn scdefns)

pprScDefn :: (Name, [Name], CoreExpr) -> Iseq
pprScDefn (v, xs, e)
  = iConcat [ iStr v
            , iConcat (map iStr xs)
            , iStr " = "
            , pprExpr e
            ]

