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
          putStrLn "Bienvenido a RecipeManager, cargue o cree un inventario (escriba \"help\" para ver la ayuda)"
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
handleComm (Load str)    = do res <-loadRM str 
                              case res of 
                                        Left err  -> do putStrLn "Error de lectura"; readevalprint 
                                        Right s -> void $ runStateError readevalprintRM s
handleComm Quit          = do putStrLn "Cerrando RecipeManger"; return () 
handleComm (NewInv name) = do putStrLn ("Creado inventario: " ++ name)
                              putStrLn ("Ahora esta en el inventario: " ++ name ++ 
                                        ", Vea los comados de inventario con \"help\"") 
                              void $ runStateError readevalprintRM (Env name [] [] [] [] 0)


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
handleRMComm comm = 
    case comm of 
        RMSave      -> saveRM
        Add_rcp rcp -> addRcp rcp
        Add_ing ing -> addInv ing 
        Rm (name,n) -> rmInv name n
        Rm_rcp name -> rmRcp name
        CheckV      -> do date <- liftIO getCurrentDateTime
                          checkE date
        IEat name   -> undefined
        WTE cond -> do list <- whatToEat cond
                       liftIO $ putStrLn ("Puede preparar: " ++ foldr (++) "" (map (\r -> (rname r) ++ " ") list) )
        WCDW names  -> undefined
        RMHelp      -> showRMHelp
        Display     -> do s <- get 
                          liftIO $ putStrLn (show s)
        AddTable iv -> addTable iv 
        RmTable n -> rmTable n 




showHelp :: IO ()
showHelp = do setCursorPosition 0 0
              clearScreen
              putStrLn "Lista de comandos disponibles:"
              putStrLn "new_inv [nombre_de_inventario] : Crear inventario"
              putStrLn "load [nombre_de_archivo].rcpm  : Cargar un inventario"
              putStrLn "quit                           : Cerrar RecipeManager"
              putStrLn "help                           : Mostrar ayuda" 


showRMHelp :: StateError ()
showRMHelp = do liftIO $ setCursorPosition 0 0
                liftIO $ clearScreen
                liftIO $ putStrLn "Lista de comandos disponibles:"
                liftIO $ putStrLn "add_ingr nombre-cantidad-dd/mm/aa         : Añadir un ingrediente al inventario"
                liftIO $ putStrLn "rm_ing   nombre cantidad                  : Eliminar un ingr del inventario"
                liftIO $ putStrLn "add_t    nombre-cantidad-carb prot grasas : Añadir un ingrediente a la tabla"
                liftIO $ putStrLn "rm_t     nombre                           : Eliminar un ingrediente al inventario"
                liftIO $ putStrLn "add_rcp  nombre...ingr[;i2]...paso1[;p2]  : Añadir receta a la lista"
                liftIO $ putStrLn "rm_rcp   nombre                           : Eliminar una receta del inventario"
                liftIO $ putStrLn "check                                     : Verifica vencimientos"                
                liftIO $ putStrLn "need_food [con {tag, <n carb}[sin {tag1,}]: Mostrar comidas preparables"                
                liftIO $ putStrLn "display                                   : Mostrar el inventario"
                liftIO $ putStrLn "save                                      : Guardar el inventario"
                liftIO $ putStrLn "close                                     : Cerrar inventario"                 
                liftIO $ putStrLn "help                                      : Mostrar ayuda" 




