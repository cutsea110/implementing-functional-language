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
pLit s ((_, tok):toks) | s == tok = [(s, toks)]
pLit s []         = []

pVar :: Parser String
pVar [] = []

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
pEmpty x toks= [(x, toks)]

pOneOrMore :: Parser a -> Parser [a]
pOneOrMore p = pThen (:) p (pOneOrMore p)

pOneOrMoreWithSep :: Parser a -> Parser b -> Parser [a]
pOneOrMoreWithSep p sep = pThen (:) p (sub p sep)
  where
    sub p sep = pThen (:) (sep *> p) (sub p sep)

pApply :: Parser a -> (a -> b) -> Parser b
pApply p f toks
  = [ (v2, toks1)
    | (v1, toks1) <- p toks
    , let v2 = f v1
    ]

parse :: String -> CoreProgram
parse = undefined
