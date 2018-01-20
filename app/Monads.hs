                                                                                                                                                                                                                                                        module Monads where

import Types
import Control.Applicative
import Control.Monad (liftM, ap)
import Data.List
import Data.Ord

data Error = RecetaExistente

instance Show Error where
    show RecetaExistente = "Nombre de receta existente."

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

--Ordena segun la fecha
sortByV    :: [(Maybe Vencimiento, Cantidad)] -> [(Maybe Vencimiento, Cantidad)]
sortByV xs = sortBy (comparing fst) xs 


                                 

 


--Lista las comidas preparables con lo disponible
whatToEat :: StateError [Receta]
whatToEat = do s <- get
               return (whatToEat' (inv s) (rcps s))
               where whatToEat' _ [] = []
                     whatToEat'[] _  = []
                     whatToEat' i r = filter (preparable_with i) r


preparable_with :: [Ingr] -> Receta -> Bool
preparable_with inv r = let needed = ingredientes r in
                        foldr (&&) True (map (check_if_have_in inv) needed)
 
--suma lo disponible en el inventario y se fija si alcanza para lo que pide la receta
check_if_have_in :: [Ingr] -> Ingr -> Bool 
check_if_have_in (i:is) n = if (ing_name n /= ing_name i) 
                            then check_if_have_in is n
                            else (foldr (+) 0 (map snd (stock i)) >= snd (head (stock n)))  
                                                                

--Añade un ingrediente dado al inventario
addInv :: Ingr -> StateError ()
addInv i = do s <- get
              put (Env {inv = searchAndPutI (inv s) i, rcps = rcps s, flag_saved = flag_saved s})


--TO DO: chequear con ingresando los ingredientes con vencimientos
--Verificar que ordena bien cada vez que se agrega
searchAndPutI :: [Ingr] -> Ingr -> [Ingr]
searchAndPutI []     i = [i]
searchAndPutI (x:xs) i = if (ing_name x == ing_name i)
                         then (Ingr {ing_name = ing_name x, 
                                     datos = datos x, 
                                     stock =  sortByV ((stock x) ++ (stock i))} : xs)
                         else x : (searchAndPutI xs i)
                           
--Añade una receta dada a la lista de recetas
addRcp :: Receta -> StateError ()
addRcp r = do s <- get
              let t = searchAndPutR (rcps s) r in
               case t of 
                         Left  err -> throw err
                         Right ok  -> put (Env {inv = inv s, rcps = ok, flag_saved = flag_saved s})


checkExistence :: [Receta] -> Receta -> Bool
checkExistence rcps n = case filter (\r -> rcp_name n == rcp_name r) rcps of
                                                                           []     -> True
                                                                           (x:xs) -> False

searchAndPutR :: [Receta] -> Receta -> Either Error [Receta]
searchAndPutR []     r = Right [r]
searchAndPutR (x:xs) r = case checkExistence (x:xs) r of
                                                       True  -> Left RecetaExistente
                                                       False -> Right (r:x:xs)

--Elimina cierta cantidad de un ingrediente dado del inventario
rmInv :: String -> Cantidad -> StateError ()
rmInv i c = do s <- get
               removeIngr i c (inv s)

removeIngr :: String -> Cantidad -> [Ingr] -> StateError ()
removeIngr name c (x:xs) = undefined 



--Clase para representar monadas que lanzan errores 
class Monad m => MonadError m where
    throw :: Error -> m a

instance MonadError StateError where
    throw err = StateError (\s -> return (Left err))





