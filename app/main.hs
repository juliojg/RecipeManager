module Main where
import System.Console.Readline

import AST


prompt :: String
prompt = "RM> "

main :: IO ()
main = do putStrLn "Bienvenido a RecipeManager"
          x <- readline prompt
          undefined


 

