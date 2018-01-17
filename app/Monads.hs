module Monads where

import Types
import Control.Applicative
import Control.Monad (liftM, ap)


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


class Monad m => MonadState m where
    get :: m Env
    put :: Env -> m ()

instance MonadState StateError where --ver si son necesarios, considerando add_inv, etc.
    get = StateError (\s -> return (Right (s,s)))
    put s' = StateError (\s -> return (Right ((), s'))) 



--Funciones auxiliares para tratar con estados data Env = Env{inv::[Ingr],rcps::[Receta],flag_saved :Int}
whatToEat :: StateError [Receta]
whatToEat = do s <- get
               return (whatToEat' (inv s) (rcps s))
               where whatToEat' _ [] = []
                     whatToEat'[] _  = []
                     whatToEat' i r = filter (preparable i) r


preparable :: [Ingr] -> Receta -> Bool
preparable inv r = undefined --check_if_have r inv 0   


getIName :: Ingr -> String
getIName = ing_name . id_ingr 

check_if_have :: Ingr -> [Ingr] -> Int -> Bool 
check_if_have r inv@(i:is) acc = if (ing_name . id_ingr . r == ing_name . id_ingr . i) && acc < cant r
                                 then check_if_have r is (acc + cant i)
                                 else undefined  


addInv :: Ingr -> StateError ()
addInv = undefined

addRcp :: Receta -> StateError ()
addRcp = undefined


--Clase para representar monadas que lanzan errores 
class Monad m => MonadError m where
    --lanza un error
    throw :: Error -> m a
    --throw = undefined



