module Main where

import Parser
import Pretty
import Types
import Monads
import Commands
import System.Console.Readline
import System.Console.ANSI (setCursorPosition, clearScreen)


import Data.Dates

import Text.ParserCombinators.Parsec 
import Text.Parsec.Token
import Text.Parsec.Language (emptyDef)
import Text.PrettyPrint.HughesPJ (render)

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)

prompt :: String
prompt = "RM> "


main :: IO ()
main = do setCursorPosition 0 0
          clearScreen
          putStrLn "Bienvenido a RecipeManager (escriba \"help\" para ver la ayuda)"
          readevalprint

readevalprint :: IO ()
readevalprint = do line <- readline prompt
                   case line of 
                     Nothing -> putStrLn "Escriba un comando (puede usar \"help\" para ver los disponibles)"
                     Just xs -> do addHistory xs
                                   case (parse (parseComm) "" xs) of
                                     Left er      -> do putStrLn "Comando mal ingresado"; readevalprint
                                     Right comm   -> handleComm comm 
                                                      
 

handleComm :: Comm -> IO ()
handleComm Help          = do showHelp ; readevalprint
handleComm (Load str)    = do s <- loadRM str
                              putStrLn ("Cargado archivo " ++ str)
                              void $ runStateError readevalprintRM s
handleComm Quit          = do putStrLn "Cerrando RecipeManger"; return () 
handleComm (NewInv name) = do putStrLn ("Creado inventario: " ++ name)
                              putStrLn ("Ahora esta en el inventario: " ++ name )
                              putStrLn "Vea los comados de inventario con \"help\"" 
                              void $ runStateError readevalprintRM (Env name [] [] 0)


readevalprintRM :: StateError ()
readevalprintRM = do line <- liftIO $ readline prompt
                     case line of
                       Nothing -> do liftIO $ putStrLn "Saliendo"; return ()
                       Just xs -> do liftIO $ addHistory xs
                                     case (parse (parseRMComm) "" xs) of
                                       Left er      -> do liftIO $ putStrLn "Mal ingresado" ; readevalprintRM
                                       Right RMClose -> do s <- get
                                                           case flag_saved s of
                                                             0 -> check_save
                                                             1 -> do liftIO $ putStrLn "Inventario cerrado exitosamente"
                                                                     liftIO readevalprint
                                       Right comm   -> do handleRMComm comm
                                                          readevalprintRM   
                                    

check_save :: StateError ()
check_save = do liftIO $ putStrLn "¿Quiere guardar el inventario? y/n"
                s <- liftIO getLine
                case s of 
                    "y"       -> do saveRM; 
                                    liftIO $ putStrLn "Inventario cerrado exitosamente"
                                    liftIO readevalprint
                    "n"       -> do liftIO $ putStrLn "Inventario cerrado exitosamente"
                                    liftIO readevalprint
                    otherwise -> check_save
 

handleRMComm :: RMComm -> StateError ()
handleRMComm (RMSave)      = saveRM
handleRMComm (Add_rcp rcp) = do addRcp rcp
                                liftIO $ putStrLn ("Receta agregada: " ++ rcp_name rcp)
handleRMComm (Add_ing ing) = do addInv ing
                                liftIO $ putStrLn ("Ingrediente agregado: " ++ ing_name ing) 
handleRMComm (Rm (name,n)) = do rmInv name n
                                liftIO $ putStrLn ("Eliminado " ++ show n ++ " de " ++ name)
handleRMComm (Rm_rcp name) = do rmRcp name
                                liftIO $ putStrLn ("Receta eliminada: " ++ name)
handleRMComm (CheckV)      = do date <- liftIO getCurrentDateTime
                                list <- checkV date
                                liftIO $ putStrLn ("Ingredientes vencidos: " ++ show list)
handleRMComm (IEat name)   = undefined
handleRMComm (WTE Nothing) = do list <- whatToEat
                                liftIO $ putStrLn ("Puede preparar: " ++ show list)
handleRMComm (WCDW names)  = undefined
handleRMComm (RMHelp)      = showRMHelp
handleRMComm (Display)     = do s <- get 
                                liftIO $ putStrLn (show s)

showHelp :: IO ()
showHelp = do setCursorPosition 0 0
              clearScreen
              putStrLn "Lista de comandos disponibles:"
              putStrLn "new_inv [nombre_de_inventario] : Crear inventario"
              putStrLn "load [nombre_de_archivo]       : Cargar un inventario"
              putStrLn "quit                           : Cerrar RecipeManager"
              putStrLn "help                           : Mostrar ayuda" 


showRMHelp :: StateError ()
showRMHelp = do liftIO $ setCursorPosition 0 0
                liftIO $ clearScreen
                liftIO $ putStrLn "Lista de comandos disponibles:"
                liftIO $ putStrLn "add_ingr nombre-cantidad-dd/mm/aa         : Añadir un ingrediente al inventario"
                liftIO $ putStrLn "add_rcp  nombre...ingr[;i2]...paso1[;p2]  : Añadir receta a la lista"
                liftIO $ putStrLn "rm_ing   nombre cantidad                  : Borrar un ingr del inventario"
                liftIO $ putStrLn "rm_rcp   nombre                           : Eliminar una receta del inventario"
                liftIO $ putStrLn "check                                     : Verifica vencimientos"                
                liftIO $ putStrLn "need_food                                 : Mostrar comidas preparables"                
                liftIO $ putStrLn "display                                   : Mostrar el inventario"
                liftIO $ putStrLn "save                                      : Guardar el inventario"
                liftIO $ putStrLn "close                                     : Cerrar inventario"                 
                liftIO $ putStrLn "help                                      : Mostrar ayuda" 




