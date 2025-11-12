{-# LANGUAGE OverloadedStrings #-}

module Lib
    (
    Env,
    lexeme,
    symbol,
    integer,
    float,
    signedInteger,
    signedFloat,
    Expr,
    pExpr,
    evalExpr,
    evalString,
    )
    where

import Data.Text (Text)
import Data.Void
import Data.Maybe
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L -- (1)
import Control.Monad.Combinators.Expr
import Data.Monoid (Sum(Sum))
import Control.Monad (void)
import qualified Data.Map.Strict as M

type Parser = Parsec Void Text
type Env = M.Map String Double

floatRem :: (RealFrac a) => a -> a -> a
floatRem x y = x - (fromIntegral (floor (x / y)) * y)

-- space consumer
sc :: Parser ()
sc = L.space
    space1
    (L.skipLineComment "#")
    empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

integer :: Parser Double
integer = lexeme L.decimal

float :: Parser Double
float = lexeme L.float

signedInteger :: Parser Double
signedInteger = L.signed sc integer

signedFloat :: Parser Double
signedFloat = L.signed sc float

reservedWords :: [String]
reservedWords = ["let"]

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
    where
        p = (:) <$> letterChar <*> many alphaNumChar
        check x =
            if x `elem` reservedWords
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x

reserved :: Text -> Parser ()
reserved w = (lexeme . try) (string w *> notFollowedBy alphaNumChar *> sc)

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

pLiteral :: Parser Expr
pLiteral = Lit <$> choice
    [ try signedFloat
    , try signedInteger
    , try float
    , try integer
    ]

pVariable :: Parser Expr
pVariable = Var <$> lexeme
    ((:) <$> letterChar <*> many alphaNumChar <?> "variable")

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

pTerm :: Parser Expr
pTerm = choice
    [ parens pExpr
    , pLiteral
    , pVariable
    ]

pExpr :: Parser Expr
pExpr = makeExprParser pTerm operatorTable

letStmt :: Parser Stmt
letStmt = do
    reserved "let"
    name <- identifier
    void (symbol "=")
    Let name <$> pExpr

pStmt :: Parser Stmt
pStmt =
    choice
        [ try letStmt
        , ExprStmt <$> pExpr
        ]

operatorTable :: [[Operator Parser Expr]]
operatorTable =
    [
        [ binary "^" Pow
        ]
        ,
        [ prefix "-" Neg
        , prefix "+" id
        ]
        ,
        [ binary "*" Mul
        , binary "/" Div
        ]
        ,
        [ binary "+" Add
        , binary "-" Sub
        ]
    ]

binary :: Text -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binary name f = InfixL (f <$ symbol name)

prefix, postfix :: Text -> (Expr -> Expr) -> Operator Parser Expr
prefix name f = Prefix (f <$ symbol name)
postfix name f = Postfix (f <$ symbol name)

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

