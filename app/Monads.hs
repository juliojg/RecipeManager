                                                                                                                                                                                                                                                        module Monads where

import Types

import Control.Monad.IO.Class
import Control.Applicative
import Control.Monad (liftM, ap)

data Error = RecetaInexistente
             | RecetaExistente 
             | IngrInexistente 
             | IngrInsuficiente 
             | CargaFallida 
             | IngrInexistenteT
             | IngrExistenteT

instance Show Error where
 show RecetaInexistente = "La siguiente receta no esta en la lista: "
 show RecetaExistente = "La siguiente receta ya esta en la lista: "
 show IngrInexistente = "El siguiente ingrediente no esta en el inventario: "
 show IngrInexistenteT = "El siguiente ingrediente no esta en la tabla, ingreselo alli primero: "
 show IngrInsuficiente = "No se posee tanto del siguiente ingrediente: "
 show CargaFallida = "No se ha podido cargar el archivo: "
 show IngrExistenteT = "El siguiente ingrediente ya esta en la tabla: "

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
                                       Left err     -> return (Left err)
                                       Right (v,s') -> runStateError (f v) s')  


class Monad m => MonadState m where
    get :: m Env
    put :: Env -> m ()

instance MonadState StateError where
    get = StateError (\s -> return (Right (s,s)))
    put s' = StateError (\s -> return (Right ((), s'))) 


--Class for represent the monads that throw errors.
class Monad m => MonadError m where
    throw :: Error -> m a
    catchError :: m a -> (Error -> m a) -> m a

instance MonadError StateError where
    throw err = StateError (\s -> return (Left err))
    catchError m handler = StateError (\s -> do e <- runStateError m s
                                                case e of 
                                                  Left err -> runStateError (handler err) s
                                                  Right (v,s') -> runStateError (return v) s')

--To use IO actions
instance MonadIO StateError where
    liftIO io = StateError (\s -> do x <- io
                                     return (Right (x, s)))
