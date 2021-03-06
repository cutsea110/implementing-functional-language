module Parser where

import Data.Char (isDigit, isSpace, isAlpha)

import Language

type LineNumber = Int
type Token = (LineNumber, String)   -- A token is never empty

clex :: LineNumber -> String -> [Token]
clex n (c:cs) | isWhiteSpace c = clex n cs
clex n ('\n':cs) = clex (n+1) cs
clex n ('-':'-':cs) = clex n rest_cs
  where rest_cs = dropWhile (/= '\n') cs
clex n (c:cs) | isDigit c = (n, c:num_token) : clex n rest_cs
  where (num_token, rest_cs) = span isDigit cs
clex n (c:cs) | isAlpha c = (n, c:var_tok) : clex n rest_cs
  where (var_tok, rest_cs) = span isIdChar cs
clex n (c:c':cs) | [c,c'] `elem` twoCharOps = (n, [c,c']) : clex n cs
clex n (c:cs) = (n, [c]) : clex n cs
clex n [] = []

isIdChar :: Char -> Bool
isIdChar c = isAlpha c || isDigit c || c == '_'

isWhiteSpace :: Char -> Bool
isWhiteSpace c = c `elem` " \t"

twoCharOps :: [String]
twoCharOps = ["==", "~=", ">=", "<=", "->"]

type Parser a = [Token] -> [(a, [Token])]

pLit :: String -> Parser String
pLit s = pSat (== s)

pVar :: Parser String
pVar = pSat ((&&) <$> all isIdChar <*> (`notElem` keywords))

keywords :: [String]
keywords = ["let", "letrec", "case", "in", "of", "Pack"]

pNum :: Parser Int
pNum = pSat (all isDigit) `pApply` read

pSat :: (String -> Bool) -> Parser String
pSat p ((n, tok):toks) | p tok = [(tok, toks)]
pSat p [] = []

pAlt :: Parser a -> Parser a -> Parser a
pAlt p1 p2 toks = p1 toks ++ p2 toks

pThen :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
pThen combine p1 p2 toks
  = [ (combine v1 v2, toks2)
    | (v1, toks1) <- p1 toks
    , (v2, toks2) <- p2 toks1
    ]

pZeroOrMore :: Parser a -> Parser [a]
pZeroOrMore p = pOneOrMore p `pAlt` pEmpty []

pEmpty :: a -> Parser a
pEmpty x toks = [(x, toks)]

pOneOrMore :: Parser a -> Parser [a]
pOneOrMore p = pThen (:) p (pZeroOrMore p)

pOneOrMoreWithSep :: Parser a -> Parser b -> Parser [a]
pOneOrMoreWithSep p sep = pThen (:) p sub
  where
    sub = pThen (:) (sep *> p) sub

pApply :: Parser a -> (a -> b) -> Parser b
pApply p f toks
  = [ (v2, toks1)
    | (v1, toks1) <- p toks
    , let v2 = f v1
    ]

pProgram :: Parser CoreProgram
pProgram = pOneOrMoreWithSep pSc (pLit ";")

pSc :: Parser CoreScDefn
pSc = pThen4 mkSc pVar (pZeroOrMore pVar) (pLit "=") pExpr

pThen4 :: (a -> b -> c -> d -> e)
       -> Parser a
       -> Parser b
       -> Parser c
       -> Parser d
       -> Parser e
pThen4 f p1 p2 p3 p4 toks
  = [ (f v1 v2 v3 v4, toks4)
    | (v1, toks1) <- p1 toks
    , (v2, toks2) <- p2 toks1
    , (v3, toks3) <- p3 toks2
    , (v4, toks4) <- p4 toks3
    ]

mkSc :: a -> b -> c -> d -> e
mkSc = undefined

pExpr = pThen EAp pExpr pAexpr

pAexpr = undefined


parse :: String -> CoreProgram
parse = undefined

syntax = take_first_parse . pProgram
  where
    take_first_parse ((prog, []):others) = prog
    take_first_parse (parse:others)      = take_first_parse others
    take_first_parse other               = error "Syntax error"
