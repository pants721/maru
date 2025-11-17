module Maru.AST 
    ( Expr (..)
    , Stmt (..)
    , Dimension (..)
    , UnitExpr (..)
    ) where

data Expr
    = Lit Double
    | LitWithUnit Double UnitExpr
    | Var String
    | Neg Expr
    | Add Expr Expr
    | Sub Expr Expr
    | Mul Expr Expr
    | Div Expr Expr
    | Pow Expr Expr
    deriving (Eq, Show)

data Stmt
    = Let String Expr
    | ExprStmt Expr
    deriving (Show, Eq)

-- Represents a quantities exponent for each dimension
-- Example:
-- `km` would have `Dimension 1 0 0`
-- `N` = `kg*m/s^2` would have `Dimension 1 1 -2`
data Dimension = Dimension
    { lengthDim :: Int
    , massDim :: Int
    , timeDim :: Int
    } deriving (Show, Eq)

data UnitExpr
    = UBase String
    | UMul UnitExpr UnitExpr
    | UDiv UnitExpr UnitExpr
    | UPow UnitExpr Int
    deriving (Show, Eq)

data Unit = Unit
    { unitName :: String
    , scale :: Double -- relative to base unit
    , dimension :: Dimension
    } deriving (Show, Eq)

-- data UnitDimension
--     = Time
--     | Length
--     | Mass
--     | Current
--     | Temperature
--     | AmountOfSubstance
--     | LuminousIntensity
