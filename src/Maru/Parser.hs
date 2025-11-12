{-# LANGUAGE OverloadedStrings #-}

module Maru.Parser 
    ( sc
    , pStmt
    ) where

import Maru.AST
import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad.Combinators.Expr
import Control.Monad (void)
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Text (Text)
import Data.Void (Void)

type Parser = Parsec Void Text

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
