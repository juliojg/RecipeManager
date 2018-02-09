                                                                                                                                                                                                                                                        module Monads where

import Types

import Control.Monad.IO.Class
import Control.Applicative
import Control.Monad (liftM, ap)
import Data.List
import Data.Ord

data Error = RecetaInexistente | RecetaExistente | IngrInexistente | IngrInsuficiente | CargaFallida

instance Show Error where
    show RecetaExistente = "Nombre de receta existente"
    show RecetaInexistente = "Nombre de receta inexistente"
    show IngrInexistente = "Ingrediente no ingresado"
    show IngrInsuficiente = "No hay tanto de ese ingrediente"
    show CargaFallida = "No se ha podido cargar el archivo"

newtype StateError a = StateError { runStateError :: Env -> IO (Either Error (a, Env)) }

instance Functor StateError where
    fmap = liftM
                                                                                                                                                                                                                                                                                    
instance Applicative StateError where
    pure = return
    (<*>) = ap

instance Monad StateError where
    return x = StateError (\s -> return (Right (x, s)))
    m >>= f  = StateError (\s -> do e <- runStateError m s 
                                    case e of 
                                       Left err      -> return (Left err)
                                       Right (v,s') -> runStateError (f v) s')  


class Monad m => MonadState m where
    get :: m Env
    put :: Env -> m ()

instance MonadState StateError where --ver si son necesarios, considerando add_inv, etc.
    get = StateError (\s -> return (Right (s,s)))
    put s' = StateError (\s -> return (Right ((), s'))) 


--Clase para representar monadas que lanzan errores 
class Monad m => MonadError m where
    throw :: Error -> m a

instance MonadError StateError where
    throw err = StateError (\s -> return (Left err))

instance MonadIO StateError where
    liftIO io = StateError (\s -> do x <- io
                                     return (Right (x, s)))
