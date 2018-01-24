module Main where

import Parser
import Types
import Monads
import System.Console.Readline
import System.Console.ANSI (setCursorPosition, clearScreen)

import Data.Dates

import Text.ParserCombinators.Parsec 
import Text.Parsec.Token
import Text.Parsec.Language (emptyDef)


import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)

prompt :: String
prompt = "RM> "


main :: IO ()
main = do putStrLn "Bienvenido a RecipeManager (escriba \"help\" para ver la ayuda)"
          readevalprint

readevalprint :: IO ()
readevalprint = do line <- readline prompt
                   case line of 
                     Nothing -> putStrLn "Escriba un comando (puede usar \"help\" para ver los disponibles)"
                     Just xs -> do addHistory xs
                                   case (parse (parseComm) "" xs) of
                                     Left er      -> do putStrLn "Comando mal ingresado"; readevalprint
                                     Right comm   -> case comm of 
                                                       Help -> do showHelp; readevalprint
                                                       _    -> handleComm comm 
                                                      
 





readevalprintRM :: StateError ()
readevalprintRM = do line <- liftIO $ readline prompt
                     case line of
                       Nothing -> do liftIO $ putStrLn "Saliendo"; return ()
                       Just xs -> do liftIO $ addHistory xs
                                     case (parse (parseRMComm) "" xs) of
                                       Left er    -> do liftIO $ putStrLn "Mal ingresado" ;return ()
                                       Right comm -> do handleRMComm comm
                                                        readevalprintRM   



handleComm :: Comm -> IO ()
handleComm Help       = showHelp
handleComm (Load str) = undefined
handleComm Save       = undefined
handleComm Close      = undefined
handleComm Display    = undefined -- agregar saludo
handleComm (NewInv name) = do putStrLn ("Creado inventario: " ++ name)
                              putStrLn ("Ahora esta en el inventario: " ++ name )
                              putStrLn "Vea los comados de inventario con \"help\"" 
                              void $ runStateError readevalprintRM (Env [] [] 0)

handleRMComm :: RMComm -> StateError ()
handleRMComm (Add_rcp rcp) = do addRcp rcp
                                liftIO $ putStrLn ("Receta agregada: " ++ rcp_name rcp)

handleRMComm (Add_ing ing) = do addInv ing
                                liftIO $ putStrLn ("Ingrediente agregado: " ++ ing_name ing) 

handleRMComm (Rm (name,n)) = do rmInv name n
                                liftIO $ putStrLn ("Eliminado " ++ show n ++ " de " ++ name)

handleRMComm (Rm_rcp name) = do rmRcp name
                                liftIO $ putStrLn ("Receta eliminada: " ++ name)

handleRMComm (CheckV) = do date <- liftIO getCurrentDateTime
                           list <- checkV date
                           liftIO $ putStrLn ("Ingredientes vencidos: " ++ show list)

handleRMComm (IEat name) = undefined

handleRMComm (WhatToEat Nothing) = do list <- whatToEat
                                      liftIO $ putStrLn ("Puede preparar: " ++ show list)
 
handleRMComm (WhatCanDoWith names) = undefined
handleRMComm (RMHelp) = showRMHelp


showHelp :: IO ()
showHelp = do setCursorPosition 0 0
              clearScreen
              putStrLn "Lista de comandos disponibles:"
              putStrLn "load [nombre_de_archivo]       : Cargar un inventario"
              putStrLn "save                           : Guardar el inventario"
              putStrLn "display                        : Mostrar el inventario"
              putStrLn "close                          : Cerrar RecipeManager"
              putStrLn "help                           : Mostrar ayuda" 
              putStrLn "new_inv [nombre_de_inventario] : Crear inventario"

showRMHelp :: StateError ()
showRMHelp = do liftIO $ setCursorPosition 0 0
                liftIO $ clearScreen
                liftIO $ putStrLn "Lista de comandos disponibles:"
                liftIO $ putStrLn "add_ingr nombre-cantidad-dd/mm/aa         : Añadir un ingrediente al inventario"
                liftIO $ putStrLn "add_rcp  nombre...ingr[;i2]...paso1[;p2]  : Añadir receta a la lista"
                liftIO $ putStrLn "rm_ing   nombre cantidad                  : Borrar un ingr del inventario"
                liftIO $ putStrLn "rm_rcp   nombre                           : Eliminar una receta del inventario"
                liftIO $ putStrLn "check                                     : Verifica vencimientos"                

                liftIO $ putStrLn "help                                      : Mostrar ayuda" 




