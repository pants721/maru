module Maru.AST 
    ( Expr(..)
    , Stmt(..)
    ) where

data Expr
    = Lit Double
    | Var String
    | Neg Expr
    | Add Expr Expr
    | Sub Expr Expr
    | Mul Expr Expr
    | Div Expr Expr
    | Pow Expr Expr
    deriving (Eq, Ord, Show)

data Stmt
    = Let String Expr
    | ExprStmt Expr
    deriving(Show, Eq)

