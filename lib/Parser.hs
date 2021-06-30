module Parser where

import Data.Char (isDigit, isSpace, isAlpha)

import Language

type Token = String   -- A token is never empty

clex (c:cs) | isWhiteSpace c = clex cs
clex ('-':'-':cs) = clex rest_cs
  where rest_cs = dropWhile (/= '\n') cs
clex (c:cs) | isDigit c = (c:num_token) : clex rest_cs
  where (num_token, rest_cs) = span isDigit cs
clex (c:cs) | isAlpha c = (c:var_tok) : clex rest_cs
  where (var_tok, rest_cs) = span isIdChar cs
clex (c:c':cs) | [c,c'] `elem` twoCharOps = [c,c'] : clex cs
clex (c:cs) = [c]:clex cs
clex [] = []

isIdChar :: Char -> Bool
isIdChar c = isAlpha c || isDigit c || c == '_'

isWhiteSpace :: Char -> Bool
isWhiteSpace c = c `elem` " \t\n"

twoCharOps :: [String]
twoCharOps = ["==", "~=", ">=", "<=", "->"]

type Parser a = [Token] -> [(a, [Token])]

pLit :: String -> Parser String
pLit s (tok:toks) | s == tok = [(s, toks)]
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
pEmpty = undefined

pOneOrMore :: Parser a -> Parser [a]
pOneOrMore = undefined

pApply :: Parser a -> (a -> b) -> Parser b
pApply = undefined

parse :: String -> CoreProgram
parse = undefined
