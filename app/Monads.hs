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

newtype StateError a = StateError { runEstateError :: Env -> Either Error (a, Env) }

instance Functor StateError where
    fmap = liftM

instance Applicative StateError where
    pure = return
    (<*>) = ap

instance Monad StateError where
    return = undefined
    (>>=) = undefined



--Clase para representar monadas con mis estados
class Monad m => MonadFoodState m where
--encontrar una comida preparable
    lookfor :: [Ingr] -> m Comida -- o todas las comidas preparables?
    --lookfor = undefined
    --agregar ingredientes al inventario
    add_inv :: Ingr -> m ()
    --add_inv = undefined
    --agregar receta
    add_rcp :: Receta -> m ()
    --add_rcp = undefined


--Clase para representar monadas que lanzan errores 
class Monad m => MonadError m where
    --lanza un error
    throw :: Error -> m a
    --throw = undefined
