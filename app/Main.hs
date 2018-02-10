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
          void $ runStateError readevalprint (Env [] [] [] [] 0)


readevalprint :: StateError ()
readevalprint = do line <- liftIO $ readline prompt
                   case line of 
                     Nothing -> liftIO $ putStrLn "Escriba un comando (puede usar \"help\" para ver los disponibles)"
                     Just xs -> do liftIO $ addHistory xs
                                   case (parse (parseComm) "" xs) of
                                     Left er      -> do liftIO $ putStrLn "Comando mal ingresado"; readevalprint
                                     Right comm   -> handleComm comm 
                                                      
 

handleComm :: Comm -> StateError ()
handleComm comm = 
    case comm of 
        Help        -> do liftIO showHelp; readevalprint
        Load str    -> do loadRM str
                          readevalprintRM 
        Quit        -> do liftIO $ putStrLn "Cerrando RecipeManger" 
                          return () 
        NewInv name -> do liftIO $ putStrLn ("Creado inventario: " ++ name)
                          liftIO $ putStrLn ("Ahora esta en el inventario: " ++ name ++ 
                                            ", vea los comados de inventario con \"help\"") 
                          put (Env name [] [] [] 0)
                          readevalprintRM

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
                                                                     readevalprint
                                       Right comm   -> do handleRMComm comm
                                                          readevalprintRM   
                                    


handleRMComm :: RMComm -> StateError ()
handleRMComm comm = 
    case comm of 
        RMSave      -> saveRM
        Add_rcp rcp -> catchError (addRcp rcp) (\e -> liftIO $ putStrLn $ show e ++ rname rcp)
        Add_ing ing -> catchError (addInv ing) (\e -> liftIO $ putStrLn $ show e ++ iname ing)
        Rm (name,n) -> rmInv name n
        Rm_rcp name -> rmRcp name
        CheckV      -> do date <- liftIO getCurrentDateTime; checkE date
        IEat name   -> undefined
        WTE cond -> do list <- whatToEat cond
                       liftIO $ putStrLn ("Puede preparar: " ++ foldr (++) "" (map (\r -> (rname r) ++ " ") list) )
        WCDW names  -> undefined
        RMHelp      -> showRMHelp
        Display     -> do s <- get; liftIO $ putStrLn (show s)
        AddTable iv -> addTable iv 
        RmTable n -> rmTable n 
        ImportTable file -> catchError (importRM file) (\_ -> return ())

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
                liftIO $ putStrLn "add_ing  nombre-cantidad-dd/mm/aa           : Añadir un ingrediente al inventario"
                liftIO $ putStrLn "rm_ing   nombre-cantidad                    : Eliminar un ingr del inventario"
                liftIO $ putStrLn "add_t    nombre-cantidad-carb prot grasas   : Añadir un ingrediente a la tabla"
                liftIO $ putStrLn "rm_t     nombre                             : Eliminar un ingrediente al inventario"
                liftIO $ putStrLn ("add_rcp  nombre -i ings -p pasos -t tags -f : Añadir receta a la lista" ++ "\n" ++
                                   "(ings : ing1[;ing2] | pasos : paso1;[paso2;] | tags : tag1;[tag2;])")
                liftIO $ putStrLn "rm_rcp   nombre                             : Eliminar una receta del inventario"
                liftIO $ putStrLn "check  S                                     : Verifica vencimientos"                
                liftIO $ putStrLn "need_food [con {tag, <n carb}][sin {tag1,}] : Mostrar comidas preparables"                
                liftIO $ putStrLn "display                                     : Mostrar el inventario"
                liftIO $ putStrLn "save                                        : Guardar el inventario"
                liftIO $ putStrLn "close                                       : Cerrar inventario"                 
                liftIO $ putStrLn "help                                        : Mostrar ayuda" 


check_save :: StateError ()
check_save = do liftIO $ putStrLn "¿Quiere guardar el inventario? y/n"
                s <- liftIO getLine
                case s of 
                    "y"       -> do saveRM; 
                                    liftIO $ putStrLn "Inventario cerrado exitosamente"
                                    readevalprint
                    "n"       -> do liftIO $ putStrLn "Inventario cerrado exitosamente"
                                    readevalprint
                    otherwise -> check_save
