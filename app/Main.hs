module Main where

import Parser
import Types
import System.Console.Readline
import System.Console.ANSI (setCursorPosition, clearScreen)


prompt :: String
prompt = "RM> "


main :: IO ()
main = do putStrLn "Bienvenido a RecipeManager (:h para ver la ayuda)"
          x <- readline prompt
          undefined-- handleComm (parseRMComm x)







--readevalprint :: Comm -> 




handleComm :: Comm -> IO ()
handleComm Help       = showHelp
handleComm (Load str) = undefined
handleComm Save       = undefined
handleComm Close      = undefined
handleComm Display    = undefined

showHelp :: IO ()
showHelp = do setCursorPosition 0 0
              clearScreen
              putStrLn "Lista de comandos disponibles:"
              putStrLn "load [nombre_de_archivo] : Cargar un inventario"
              putStrLn "save                     : Guardar el inventario"
              putStrLn "display                  : Mostrar el inventario"
              putStrLn "close                    : Cerrar RecipeManager"
              putStrLn ":h                       : Mostrar ayuda" 
