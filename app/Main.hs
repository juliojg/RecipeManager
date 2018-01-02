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
          handleRMComm (parseRMComm x)







--readevalprint :: Comm -> 




handleRMComm :: Comm -> IO ()
handleRMComm Help       = showHelp
handleRMComm (Load str) = undefined
handleRMComm Save       = undefined
handleRMComm Close      = undefined
handleRMComm Display    = undefined

showHelp :: IO ()
showHelp = do setCursorPosition 0 0
              clearScreen
              putStrLn "Lista de comandos disponibles:"
              putStrLn "load [nombre_de_archivo] : Cargar un inventario"
              putStrLn "save                     : Guardar el inventario"
              putStrLn "display                  : Mostrar el inventario"
              putStrLn "close                    : Cerrar RecipeManager"
              putStrLn ":h                       : Mostrar ayuda" 
