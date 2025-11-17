{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Data.Text as T
import System.IO (hFlush, stdout)
import qualified Data.Map.Strict as M
import Maru (evalString)
import qualified Text.Megaparsec.Error as E
import Text.Megaparsec (parse, between, eof)
import Maru.Parser

main :: IO ()
main = replAST

repl :: IO ()
repl = loop M.empty
    where
        loop env = do
            putStr ">>> "
            hFlush stdout
            input <- getLine
            case evalString env (T.pack input) of
                Left err -> do
                    print err
                    loop env
                Right (val, env') -> do 
                    putStrLn ("= " ++ show val)
                    loop env'

replAST :: IO ()
replAST = loop
    where
        loop = do
            putStr ">>> "
            hFlush stdout
            input <- getLine
            case input of
                "" -> loop  -- Skip empty input
                _ -> do
                    case parse pStmt "<stdin>" (T.pack input) of
                        Left err -> do
                            putStrLn $ "Parse error: " ++ show err
                            loop
                        Right ast -> do
                            putStrLn $ show ast
                            loop

