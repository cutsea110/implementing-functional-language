module Main where

import Language (CoreProgram, Expr (..))
import PrettyPrint (pprint)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"

preludeDefs :: CoreProgram
preludeDefs
  = [ ("I",  ["x"], EVar "x")
    , ("K",  ["x", "y"], EVar "x")
    , ("K1", ["x", "y"], EVar "y")
    , ("S",  ["f", "g", "x"], EAp
                              (EAp (EVar "f") (EVar "x"))
                              (EAp (EVar "g") (EVar "x")))
    , ("compose", ["f", "g", "x"], EAp
                                   (EVar "f")
                                   (EAp (EVar "g") (EVar "x")))
    , ("twice", ["f"], EAp (EAp (EVar "compose") (EVar "f")) (EVar "f"))
    , ("quadruple", ["x"],
       ELet False
        [ ("twice_x", EAp (EAp (EVar "+") (EVar "x")) (EVar "x"))
        , ("double", EAp (EVar "*") (ENum 2))
        ]
        (EAp (EAp (EVar "+") (EVar "twice_x")) (EVar "twice_x")))
    ]
