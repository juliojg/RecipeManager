module Main where

import Parser
import Types
import System.Console.Readline
import System.Console.ANSI (setCursorPosition, clearScreen)


import Text.ParserCombinators.Parsec 
import Text.Parsec.Token
import Text.Parsec.Language (emptyDef)

prompt :: String
prompt = "RM> "


main :: IO ()
main = do putStrLn "Bienvenido a RecipeManager (:h para ver la ayuda)"
          readevalprint






readevalprint :: IO () -- el tipo deberia ser el de la monada que defini
readevalprint = do line <- readline prompt
                   case line of
                    Nothing -> do putStrLn "\n Saliendo"; return ()
                    Just xs -> case (parse (parseComm) "" xs) of
                                Left er    -> do putStrLn "\n Mal ingresado" ;return ()
                                Right comm -> do handleComm comm ; readevalprint   



handleComm :: Comm -> IO ()
handleComm Help       = showHelp
handleComm (Load str) = undefined
handleComm Save       = undefined
handleComm Close      = undefined
handleComm Display    = undefined





{-
handleRMComm :: RMComm -> IO ()
handleRMComm (Add_Rcp rcp) = 


handleAdd_Rcp :: IO ()
handleAdd_Rcp = do putStrLn "Ingrese los ingredientes necesarios para preparar la"
                   putStrLn "comida, separandolos con enters, al finalizar escriba :f"
                   x <- getLine
                   if x == ":f"
                   then return ()
                   else 
-}

showHelp :: IO ()
showHelp = do setCursorPosition 0 0
              clearScreen
              putStrLn "Lista de comandos disponibles:"
              putStrLn "load [nombre_de_archivo] : Cargar un inventario"
              putStrLn "save                     : Guardar el inventario"
              putStrLn "display                  : Mostrar el inventario"
              putStrLn "close                    : Cerrar RecipeManager"
              putStrLn ":h                       : Mostrar ayuda" 
