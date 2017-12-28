module Monads where

import Types
import Control.Applicative
import Control.Monad (liftM, ap)


--el estado que llevará el programa
--data State = State { inv  :: [Ingr],
--                     rcps :: [Receta]
--                   }
data Error = Definirlos

--instance Show Error where

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



--Clase para representar monadas con mis estados
class Monad m => MonadFoodDB m where
--encontrar una comida preparable
    whatToEat :: m [Comida] -- o todas las comidas preparables?
    --lookfor = undefined
    --agregar ingredientes al inventario
    add_inv :: Ingr -> m ()
    --add_inv = undefined
    --agregar receta
    add_rcp :: Receta -> m ()
    --add_rcp = undefined

instance MonadFoodDB StateError where
    whatToEat = do s <- get
                   return (whatToEat' (inv s) (rcps s))
                   where whatToEat' _ [] = []
                         whatToEat'[] _  = []
                         whatToEat' (i : is) (r : rs) = undefined
                  



class Monad m => MonadState m where
    get :: m Env
    put :: Env -> m ()

instance MonadState StateError where --ver si son necesarios, considerando add_inv, etc.
    get = StateError (\s -> return (Right (s,s)))
    put s' = StateError (\s -> return (Right ((), s'))) 


--Clase para representar monadas que lanzan errores 
class Monad m => MonadError m where
    --lanza un error
    throw :: Error -> m a
    --throw = undefined



