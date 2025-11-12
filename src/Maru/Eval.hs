module Maru.Eval
    ( evalString
    ) where

import Maru.AST
import Maru.Types
import Maru.Parser
import Text.Megaparsec
import qualified Data.Map.Strict as M
import Data.Text (Text)
import Data.Maybe
import Control.Monad

evalExpr :: Env -> Expr -> Either String Double
evalExpr env expr = case expr of
    Lit n -> Right n
    Var name -> do
        if not $ M.member name env
            then Left ("Variable " ++ name ++ " is undefined")
        else do
            Right (fromJust $ M.lookup name env)


    Neg a -> unOp negate a

    Add a b -> binOp (+) a b
    Sub a b -> binOp (-) a b
    Mul a b -> binOp (*) a b
    Div a b -> do
        bv <- evalExpr env b
        if bv == 0
            then Left "Division by zero"
            else do
                av <- evalExpr env a
                Right (av / bv)

    Pow a b -> binOp (**) a b
         
    where
        binOp op x y = do
            xv <- evalExpr env x
            yv <- evalExpr env y
            Right (op xv yv)
        unOp op x = do
            xv <- evalExpr env x
            Right (op xv)

evalStmt :: Env -> Stmt -> Either String (Double, Env)
evalStmt env stmt =
    case stmt of
        Let name rhs ->
            if M.member name env
                then Left $ "Variable already defined: " ++ name
                else do
                    val <- evalExpr env rhs
                    let env' = M.insert name val env
                    Right (val, env')
        ExprStmt e -> do
            val <- evalExpr env e
            Right (val, env)

evalString :: Env -> Text -> Either String (Double, Env)
evalString env input =
    case parse (between sc eof pStmt) "<input>" input of
        Left err -> Left (errorBundlePretty err)
        Right stmt -> do
            (val, env') <- evalStmt env stmt
            Right (val, env')

