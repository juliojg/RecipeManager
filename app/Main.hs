module Main where

import AST
import System.Console.Readline


prompt :: String
prompt = "RM> "

main :: IO ()
main = do putStrLn "Bienvenido a RecipeManager"
          x <- readline prompt
          undefined
