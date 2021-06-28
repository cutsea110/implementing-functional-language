module Language
  ( Expr (..)
  , CoreExpr
  , Name
  , IsRec
  , recursive, nonRecursive
  , bindersOf, rhssOf
  , Alter
  , CoreAlt
  , isAtomicExpr
  , Program, CoreProgram
  , ScDefn, CoreScDefn
  ) where

import Utils

data Expr a
  = EVar Name                           -- ^ Variables
  | ENum Int                            -- ^ Numbers
  | EConstr Int Int                     -- ^ Constructor tag arity
  | EAp (Expr a) (Expr a)               -- ^ Applications
  | ELet                                -- ^ Let(rec) expressions
    IsRec                               -- ^   boolean with True = recursive,
    [(a, Expr a)]                       -- ^   Definitions
    (Expr a)                            -- ^   Body of let(rec)
  | ECase                               -- ^ Case expressions
    (Expr a)                            -- ^   Expression to scrutinise
    [Alter a]                           -- ^   Alternatives
  | ELam [a] (Expr a)                   -- ^ Lambda abstractions
  deriving (Show)

type CoreExpr = Expr Name
type Name = String
type IsRec = Bool
recursive, nonRecursive :: IsRec
recursive    = True
nonRecursive = False

bindersOf :: [(a, b)] -> [a]
bindersOf defns = [name | (name, rhs) <- defns]

rhssOf :: [(a, b)] -> [b]
rhssOf defns = [rhs | (name, rhs) <- defns]

type Alter a = (Int, [a], Expr a)
type CoreAlt = Alter Name

isAtomicExpr :: Expr a -> Bool
isAtomicExpr (EVar v) = True
isAtomicExpr (ENum n) = True
isAtomicExpr e        = False

type Program a = [ScDefn a]
type CoreProgram = Program Name

type ScDefn a = (Name, [a], Expr a)
type CoreScDefn = ScDefn Name


-- here ???
mkMultiAp :: Int -> CoreExpr -> CoreExpr -> CoreExpr
mkMultiAp n e1 e2 = foldl EAp e1 (take n e2s)
  where e2s = e2:e2s
