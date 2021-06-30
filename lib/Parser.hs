module Parser where

import Data.Char (isDigit, isSpace, isAlpha)

import Language

type Token = String   -- A token is never empty

clex (c:cs) | isWhiteSpace c = clex cs
clex (c:cs) | isDigit c = num_token : clex rest_cs
  where num_token = c:takeWhile isDigit cs
        rest_cs   = dropWhile isDigit cs
clex (c:cs) | isAlpha c = var_tok : clex rest_cs
  where var_tok = c:takeWhile isIdChar cs
        rest_cs = dropWhile isIdChar cs
clex (c:cs) = [c]:clex cs
clex [] = []

isIdChar :: Char -> Bool
isIdChar c = isAlpha c || isDigit c || c == '_'
isWhiteSpace :: Char -> Bool
isWhiteSpace c = c `elem` " \t\n"

parse :: String -> CoreProgram
parse = undefined
