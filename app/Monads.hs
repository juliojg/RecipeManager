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
                     whatToEat' i r = filter (preparable_with i) r


preparable_with :: [Ingr] -> Receta -> Bool
preparable_with inv r = let needed = ingredientes r in
                        foldr (&&) True (map (check_if_have_in inv) needed)
 

check_if_have_in :: [Ingr] -> Ingr -> Bool 
check_if_have_in (i:is) n = if (ing_name n /= ing_name i) 
                            then check_if_have_in is n
                            else (foldr (+) 0 (map snd (stock i)) >= snd (head (stock n))) --sumo lo disponible 
                                                                


 
addInv :: Ingr -> StateError ()
addInv i = do s <- get
              undefined

addRcp :: Receta -> StateError ()
addRcp = undefined


--Clase para representar monadas que lanzan errores 
class Monad m => MonadError m where
    --lanza un error
    throw :: Error -> m a
    --throw = undefined



