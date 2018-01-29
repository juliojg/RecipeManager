module Commands where


import Types
import Monads
import Pretty
import Parser

import Text.ParserCombinators.Parsec 
import Text.Parsec.Token
import Text.Parsec.Language (emptyDef)
import Text.Parsec.Char


import Text.PrettyPrint.HughesPJ (render)
import Control.Monad.IO.Class
import Control.Applicative
import Control.Monad (liftM, ap)
import Data.List
import Data.Ord



loadRM :: String -> IO Env
loadRM path = do file <- readFile ("app/save/" ++ path)
                 case parse parserEnv "" file of
                    Left err  -> do putStrLn "Error de lectura"; return (Env "" [] [] [] []  1)
                    Right s   -> return s

saveRM :: StateError ()
saveRM = do s <- get 
            liftIO $ putStrLn ("Guardando archivo: " ++ (file s) ++ ".txt")            
            liftIO $ writeFile ("app/save/" ++ (file s) ++ ".txt") (render (printEnv s))
            put (Env (file s) (inv s) (rcps s) (table s) (tag_list s) 1)



--Lista las comidas preparables con lo disponible

whatToEat :: StateError [Recipe]
whatToEat = do s <- get
               return (whatToEat' (inv s) (rcps s))
               where whatToEat' _ [] = []
                     whatToEat'[] _  = []
                     whatToEat' i r = filter (preparable_with i) r


preparable_with :: [Ingr] -> Recipe -> Bool
preparable_with inv r = foldr (&&) True (map (check_if_have_in inv) (ingrs r))

--suma lo disponible en el inventario y se fija si alcanza para lo que pide la receta
check_if_have_in :: [Ingr] -> Ingr -> Bool 
check_if_have_in inv n = (foldr (+) 0 (map (\i -> if iname i == iname n then quantity i else 0) inv)) >= (quantity n)  
            

--Añade un ingrediente dado al inventario
addInv :: Ingr -> StateError ()
addInv i = do s <- get
              put (Env (file s) (searchPlace i (inv s)) (rcps s) (table s) (tag_list s) 0)


searchPlace :: Ingr -> [Ingr] -> [Ingr]
searchPlace i []     = [i]
searchPlace i (x:xs) = if (iname i == iname x)
                       then (if expire i <= expire x 
                             then (if expire i == expire x 
                                   then (Ingr (iname i) (nutritional_values i) (quantity i + quantity x) (expire i)):xs 
                                   else i:x:xs) 
                             else x : (searchPlace' i xs))  
                       else x : keepTrying
                       where keepTrying = (searchPlace i xs)

searchPlace' :: Ingr -> [Ingr] -> [Ingr]
searchPlace' i [] = [i] 
searchPlace' i (x:xs) = if (iname i == iname x)
                        then (if expire i < expire x then i:x:xs else x : (searchPlace' i xs))  
                        else (i:x:xs)
                        

                         
--Añade una receta dada a la lista de recetas
addRcp :: Recipe -> StateError ()
addRcp r = do s <- get
              let t = searchAndPutR (rcps s) r in
               case t of 
                         Left  err -> throw err
                         Right ok  -> put (Env (file s) (inv s) ok (table s) (tag_list s) 0)


searchAndPutR :: [Recipe] -> Recipe -> Either Error [Recipe]
searchAndPutR []     r = Right [r]
searchAndPutR (x:xs) r = case checkExistenceR (x:xs) r of
                                                       False  -> Right (r:x:xs)
                                                       True   -> Left RecetaExistente

checkExistenceR :: [Recipe] -> Recipe -> Bool
checkExistenceR rcps n = case filter (\r -> rname n == rname r) rcps of
                                                                      []     -> False
                                                                      (x:xs) -> True

--Elimina cierta cantidad de un ingrediente dado del inventario
rmInv :: String -> Grams -> StateError ()
rmInv i q = do s <- get
               case rmInv' i q (inv s) of
                Left err -> throw err
                Right l  -> put $ Env (file s) l (rcps s) (table s) (tag_list s) 0 

--La lista ya tiene agrupados ingredientes
rmInv' :: String -> Grams -> [Ingr] -> Either Error [Ingr]
rmInv' name q []     = Left IngrInsuficiente
rmInv' name q (x:xs) = if (iname x == name) 
                       then (case compare (quantity x) q of
                                                    LT -> rmInv' name (q - quantity x) xs 
                                                    EQ -> Right xs
                                                    GT -> Right $ Ingr name (nutritional_values x) (quantity x - q) (expire x) : xs)
                       else either (Left) (\ns -> Right (x : ns)) (rmInv' name q xs)


--Elimina una receta de la lista de recetas  
rmRcp :: String -> StateError ()
rmRcp name = do s <- get
                let nr = filter (\r -> rname r /= name) (rcps s) in
                    if nr == rcps s 
                    then throw RecetaInexistente
                    else put (Env (file s) (inv s) nr (table s) (tag_list s) 0) 


--Revisa vencimientos
checkE :: ExpireDate -> StateError [Ingr]
checkE today = do s <- get
                  return (filter (checkE' today) (inv s))

checkE' :: ExpireDate -> Ingr -> Bool
checkE' today i = case expire i of 
                               Nothing -> True
                               Just v  -> (today > v)

