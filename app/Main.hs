{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Data.Text as T
import System.IO (hFlush, stdout)
import qualified Data.Map.Strict as M
import Maru (evalString)

main :: IO ()
main = loop M.empty
    where
        loop env = do
            putStr ">>> "
            hFlush stdout
            input <- getLine
            case evalString env (T.pack input) of
                Left err -> do
                    putStrLn ("Error: " ++ err)
                    loop env
                Right (val, env') -> do 
                    putStrLn ("= " ++ show val)
                    loop env'
