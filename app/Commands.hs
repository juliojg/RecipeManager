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
import System.Directory (createDirectoryIfMissing)


-- agregar en el estado ubicacion de guardado, ahora guarda donde se ejecuta

loadRM :: String -> IO (Either Error Env)
loadRM path = do file <- readFile ("save/" ++ path)
                 case parse parserEnv "" file of
                    Left err  -> return (Left CargaFallida)
                    Right s   -> do putStrLn ("Cargado archivo " ++ path) 
                                    return (Right s)

--Guarda el estado del programa, en un archivo de la carpeta /save donde se ejecuto
saveRM :: StateError ()
saveRM = do s <- get 
            liftIO $ createDirectoryIfMissing False "save"
            liftIO $ putStrLn ("Guardando archivo: " ++ (file s) ++ ".txt")             
            liftIO $ writeFile ("save/" ++ (file s) ++ ".txt") (render (printEnv s))
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
              case (getNV (table s) (iname i) (quantity i)) of 
                Left err -> liftIO $ putStrLn ("El ingrediente " ++ iname i ++ " no esta en la tabla, ingreselo alli primero.")
                Right nv -> let ni = Ingr (iname i) (Just nv) (quantity i) (expire i) 
                            in do put (Env (file s) 
                                           (searchPlace ni (inv s)) 
                                           (rcps s) 
                                           (table s) 
                                           (tag_list s) 
                                           0)  
                                  liftIO $ putStrLn ("Ingrediente agregado: " ++ iname i)


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
                         Left  err -> liftIO $ putStrLn ("La receta "++ show (rname r) ++ " ya esta en la lista")
                         Right ok  -> do put (Env (file s) (inv s) ok (table s) (tag_list s) 0)
                                         liftIO $ putStrLn ("Receta agregada: " ++ rname r)

searchAndPutR :: [Recipe] -> Recipe -> Either Error [Recipe]
searchAndPutR []     r = Right [r]
searchAndPutR (x:xs) r = case checkExistenceR (x:xs) r of
                                                       False -> Right (r:x:xs)
                                                       True  -> Left RecetaExistente

checkExistenceR :: [Recipe] -> Recipe -> Bool
checkExistenceR rcps n = case filter (\r -> rname n == rname r) rcps of
                                                                      []     -> False
                                                                      (x:xs) -> True

--Elimina cierta cantidad de un ingrediente dado del inventario
rmInv :: String -> Grams -> StateError ()
rmInv i q = do s <- get
               case rmInv' i q (inv s) of
                Left err -> liftIO $ putStrLn ("No hay esa cantidad del ingrediente: " ++ i)
                Right l  -> do put $ Env (file s) l (rcps s) (table s) (tag_list s) 0 
                               liftIO $ putStrLn ("Eliminado " ++ show q ++ " del ingrediente: " ++ i)         
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
                    then liftIO $ putStrLn ("La receta " ++ name ++ " no esta en la lista:")
                    else do put (Env (file s) (inv s) nr (table s) (tag_list s) 0) 
                            liftIO $ putStrLn ("Receta eliminada: " ++ name)

--Revisa vencimientos
checkE :: ExpireDate -> StateError ()
checkE today = do s <- get
                  liftIO $ putStrLn ("Ingredientes vencidos: " ++ show (filter (checkE' today) (inv s)))
                  

checkE' :: ExpireDate -> Ingr -> Bool
checkE' today i = case expire i of 
                               Nothing -> True
                               Just v  -> (today > v)


--Agrega un ingrediente a la tabla de valores
--VER DE ORDENARLOS ALFABETICAMENTE
--Si ya existe lo piso
addTable :: IngValues -> StateError ()
addTable iv = do s <- get
                 put (Env (file s) (inv s) (rcps s) (verifyPlace iv (table s)) (tag_list s) 0 )
                 liftIO $ putStrLn ("Datos de ingrediente " ++ show (tname iv) ++ " agregados")
verifyPlace :: IngValues -> [IngValues] -> [IngValues]
verifyPlace iv []     = [iv]
verifyPlace iv (x:xs) = if (tname x == tname iv)
                        then iv : xs
                        else x : (verifyPlace iv xs)


--Elimina un ingrediente de la tabla de valores
rmTable :: String -> StateError ()
rmTable name = do s <- get
                  case rmTable' name (table s) of
                    Left err -> liftIO $ putStrLn ("El igrediente no esta en la tabla: " ++ name)
                    Right ns -> do put (Env (file s) (inv s) (rcps s) ns (tag_list s) 0 )
                                   liftIO $ putStrLn ("Datos de ingrediente " ++ name ++ " removidos") 
rmTable' :: String -> [IngValues] -> Either Error [IngValues]
rmTable' name [] = Left IngrInexistente
rmTable' name (x:xs) = if (tname x == name)
                       then Right xs
                       else either (Left) (\ns -> Right (x : ns)) (rmTable' name xs)


--Obtiene los valores nutricionales de un ingr en base a la tabla
getNV :: [IngValues] -> String -> Grams -> Either Error NutritionalValues
getNV []     name w = Left IngrInexistente
getNV (x:xs) name w = if tname x == name 
                      then Right (NV (threeRule (portion x) w (carb (values x)))  
                                     (threeRule (portion x) w (prot (values x)))
                                     (threeRule (portion x) w (fats (values x))))
                      else getNV xs name w



threeRule :: Grams -> Grams -> Grams -> Grams
threeRule x y z = (z * y) / x



{-The Atwater system uses the average values of 4 Kcal/g for protein,
 4 Kcal/g for carbohydrate, and 9 Kcal/g for fat. -}

getCalories :: NutritionalValues -> KiloCalorie
getCalories nv = (carb nv) * 4.0 + (prot nv) * 4.0 + (fats nv) * 9.0










