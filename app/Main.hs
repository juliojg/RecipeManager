module Main where

import Parser
import Types
import Monads
import System.Console.Readline
import System.Console.ANSI (setCursorPosition, clearScreen)


import Text.ParserCombinators.Parsec 
import Text.Parsec.Token
import Text.Parsec.Language (emptyDef)

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)

prompt :: String
prompt = "RM> "


main :: IO ()
main = do putStrLn "Bienvenido a RecipeManager (escriba \"help\" para ver la ayuda)"
          void $ runStateError readevalprint (Env [] [] 0)






readevalprint :: StateError () -- el tipo deberia ser el de la monada que defini
readevalprint = do line <- liftIO $ readline prompt
                   case line of
                    Nothing -> do liftIO $ putStrLn "\n Saliendo"; return ()
                    Just xs -> case (parse (parseRMComm) "" xs) of
                                Left er    -> do liftIO $ putStrLn "\n Mal ingresado" ;return ()
                                Right comm -> do handleRMComm comm
                                                 readevalprint   



handleComm :: Comm -> IO ()
handleComm Help       = showHelp
handleComm (Load str) = undefined
handleComm Save       = undefined
handleComm Close      = undefined
handleComm Display    = undefined


handleRMComm :: RMComm -> StateError ()
handleRMComm (Add_rcp rcp) = addRcp rcp
handleRMComm (Add_ing ing) = addInv ing
handleRMComm (Rm (name,n)) = rmInv name n
handleRMComm (Rm_rcp name) = rmRcp name
handleRMComm (CheckV) = undefined -- hecha, ver tipos
handleRMComm (IEat name) = undefined
handleRMComm (WhatToEat Nothing) = undefined -- hecha sin condiciones, ver tipos
handleRMComm (WhatCanDoWith names) = undefined
handleRMComm (NewInv name) = undefined
handleRMComm (RMHelp) = showRMHelp


showHelp :: IO ()
showHelp = do setCursorPosition 0 0
              clearScreen
              putStrLn "Lista de comandos disponibles:"
              putStrLn "load [nombre_de_archivo] : Cargar un inventario"
              putStrLn "save                     : Guardar el inventario"
              putStrLn "display                  : Mostrar el inventario"
              putStrLn "close                    : Cerrar RecipeManager"
              putStrLn "help                       : Mostrar ayuda" 

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




