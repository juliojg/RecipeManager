module Commands where

import Types
import Monads
import Pretty
import Parser

import Data.List
import Data.Dates
import Data.Ord
import Data.Maybe

import Text.ParserCombinators.Parsec 
import Text.PrettyPrint.HughesPJ (render)

import Control.Monad.IO.Class
import Control.Applicative
import Control.Exception (catch,IOException)
import Control.Monad (liftM, ap)

import System.Directory (createDirectoryIfMissing)

 
--Load a file from the directory /save where it is executed.
loadRM :: String -> StateError ()
loadRM path = do file <- liftIO $ catch (readFile ("save/" ++ path)) (\e -> do let err = show (e :: IOException)
                                                                               putStrLn ("No se pudo abrir el archivo " ++ path)
                                                                               return "")
                 case parse parserEnv "" file of
                    Left err -> throw CargaFallida
                    Right s  -> do put s
                                   liftIO $ putStrLn ("Cargado archivo " ++ path) 
                                   return ()


--Saves the program state in a file, creating a directory if necessary.
saveRM :: StateError ()
saveRM = do s <- get 
            liftIO $ createDirectoryIfMissing False "save"
            liftIO $ writeFile ("save/" ++ (file s) ++ ".rcpm") (render (printEnv s))
            liftIO $ putStrLn ("Guardado archivo: " ++ (file s) ++ ".rcpm")             
            put (Env (file s) (inv s) (rcps s) (table s) (logC s) 1)

--Import the table from another inventory previously saved.
importRM :: String -> StateError ()
importRM path = do s <- get
                   r <- liftIO (catch (readFile ("save/" ++ path)) 
                                      (\e -> do let err = show (e :: IOException)
                                                putStrLn ("No se pudo abrir el archivo " ++ path)
                                                return ""))
                   case parse parserTable "" r of
                                  Left err -> throw CargaFallida
                                  Right ts  -> do let nt = filter (\t-> not $ elem t (table s)) ts
                                                  put (Env (file s) (inv s) (rcps s) ((table s) ++ nt) (logC s) 0)
                                                  liftIO $ putStrLn ("Importada tabla de: " ++ path)
             
--Adds an ingredient to the inventory, grouping those with same name and ordering them by expire date.
addInv :: Ingr -> StateError ()
addInv i = do nv <- getNV (iname i) (quantity i)
              s <- get
              let ni = Ingr (iname i) (quantity i) (expire i) in 
               put (Env (file s) (searchPlace ni (inv s)) (rcps s) (table s) (logC s) 0)  
              liftIO $ putStrLn ("Ingrediente agregado: " ++ iname i)



searchPlace :: Ingr -> [Ingr] -> [Ingr]
searchPlace i []     = [i]
searchPlace i (x:xs) = if (iname i == iname x)
                       then (searchPlace' i (x:xs))
                       else x : (searchPlace i xs)



searchPlace' :: Ingr -> [Ingr] -> [Ingr]
searchPlace' i [] = [i] 
searchPlace' i (x:xs) = if (iname i == iname x)
                        then (case compare (expire i) (expire x) of
                                LT -> (i:x:xs)
                                EQ -> (Ingr (iname i) 
                                            (quantity i + quantity x) 
                                            (expire i)) : xs
                                GT -> x : (searchPlace' i xs) )                      
                        else (i:x:xs)
                        

                         
--Adds a recipe to the list.
addRcp :: Recipe -> StateError ()
addRcp r = do s <- get
              if (filter (\n -> rname r == rname n) (rcps s) == [])
              then put (Env (file s) (inv s) (r:(rcps s)) (table s) (logC s) 0)
              else throw RecetaExistente
              liftIO $ putStrLn ("Receta agregada: " ++ rname r)


--Remove a certain amount of a ingredient from the inventory.
rmInv :: String -> Grams -> StateError ()
rmInv i q = do s <- get
               ni <- rmInv' i q (inv s)  
               put (Env (file s) ni (rcps s) (table s) (logC s) 0)


rmInv' :: String -> Grams -> [Ingr] -> StateError [Ingr]
rmInv' _    0 xs     = return xs
rmInv' name q []     = throw IngrInsuficiente
rmInv' name q (x:xs) = if (iname x == name) 
                       then (case compare (quantity x) q of
                                                    LT -> rmInv' name (q - quantity x) xs 
                                                    EQ -> return xs
                                                    GT -> return $ Ingr name (quantity x - q) (expire x) : xs)
                       else do is <- (rmInv' name q xs)
                               return (x : is)


--Removes the ingredients used by a recipe, adding the calories from them to the log.

iEat :: String -> StateError ()
iEat name = do s <- get
               date <- liftIO getCurrentDateTime
               let r = find (\r -> rname r == name) (rcps s)
               rnv <- maybe (throw RecetaInexistente) getRcpNV r
               (mapM_ (\(n,q) -> rmInv n q) . map (\i -> (iname i, quantity i)) . ingrs) (fromJust r)
               s' <- get
               put (Env (file s') (inv s') (rcps s') (table s') (updateLog (logC s') date rnv) (flag_saved s'))
               liftIO $ putStrLn (show (getCalories rnv) ++ " calorias agregadas al log")   


--Removes a recipe from the list.
rmRcp :: String -> StateError ()
rmRcp name = do s <- get
                let nr = filter (\r -> rname r /= name) (rcps s) in
                    if nr == rcps s 
                    then liftIO $ putStrLn ("La siguiente receta no esta en la lista: " ++ name )
                    else do put (Env (file s) (inv s) nr (table s) (logC s) 0) 
                            liftIO $ putStrLn ("Receta eliminada: " ++ name)



--Check the expire dates of the ingredients.
checkE :: StateError ()
checkE = do s <- get
            today <- liftIO getCurrentDateTime
            let res =  intercalate "\n" ((map showSimpleIngr (filter (maybe True (\e -> today > e) . expire) (inv s))))
            case res of 
              [] -> liftIO $ putStrLn ("Ningun ingrediente esta vencido")
              xs -> liftIO $ putStrLn ("Ingredientes vencidos:\n" ++ xs)
              


--Adds an ingredient to the value table.
addTable :: IngValues -> StateError ()
addTable iv = do s <- get
                 if elem iv (table s) 
                 then throw IngrExistenteT
                 else put (Env (file s) (inv s) (rcps s) (iv:(table s)) (logC s) 0)                   


--Removes an ingredient from the value table.
rmTable :: String -> StateError ()
rmTable name = do s <- get
                  nt <- rmTable' name (table s)
                  total <- getTotal name (inv s)
                  rmInv name total
                  s' <- get
                  put (Env (file s) (inv s') (rcps s) nt (logC s) 0 )
                  liftIO $ putStrLn ("Datos de ingrediente " ++ name ++ " eliminados") 


rmTable' :: String -> [IngValues] -> StateError [IngValues]
rmTable' name [] = throw IngrInexistenteT
rmTable' name (x:xs) = if (tname x == name) then (return xs) else do xs' <- (rmTable' name xs)
                                                                     return (x : xs')

getTotal :: String -> [Ingr] -> StateError Grams
getTotal name []     = return 0
getTotal name (x:xs) = if name == iname x 
                       then do q <- getTotal name xs 
                               return (quantity x + q)  
                       else getTotal name xs



--Gets the nutritional values from an ingredient based on the value table.
getNV :: String -> Grams -> StateError NutritionalValues
getNV name w = do s <- get
                  case find (\x -> tname x == name) (table s) of 
                    Nothing -> throw IngrInexistenteT
                    Just x  -> return (NV (threeRule (portion x) w (carb (values x)))  
                                          (threeRule (portion x) w (prot (values x)))
                                          (threeRule (portion x) w (fats (values x))))
                              

--Gets the nutritional values from a recipe based on the ingredients used in her.
getRcpNV :: Recipe -> StateError NutritionalValues
getRcpNV r = do s <- get
                aux <- mapM (\i -> getNV (iname i) (quantity i)) (ingrs r)  
                return (foldr (sumNV) accNV aux)



--Creates a simple dictionary with the recipes an their respective nutritional values.
getRcpsNV :: [Recipe] -> StateError [(Recipe,NutritionalValues)]
getRcpsNV [] = return []
getRcpsNV (r:rs) = do s <- get
                      a <- getRcpNV r
                      aux <- getRcpsNV rs
                      return $ (r,a):aux      
                    

accNV :: NutritionalValues
accNV = NV 0 0 0 

sumNV :: NutritionalValues -> NutritionalValues -> NutritionalValues
sumNV n1 n2 = (NV (carb n1 + carb n2) (prot n1 + prot n2) (fats n1 + fats n2))

diffNV :: NutritionalValues -> NutritionalValues -> NutritionalValues
diffNV n1 n2 = (NV (carb n1 - carb n2) (prot n1 - prot n2) (fats n1 - fats n2))



--List the preparables recipes based on the disponible ingredients.

whatToEat :: Maybe [Cond] -> StateError [Recipe]
whatToEat conds = do s <- get
                     let prep = filter (preparable_with (inv s)) (rcps s) in 
                      case conds of 
                        Nothing -> return prep
                        Just xs -> having xs prep
                        

having :: [Cond] -> [Recipe] -> StateError [Recipe]
having []     rs = return rs
having (c:cs) rs = do res <- (having cs rs)
                      rcps_nvs <- getRcpsNV rs
                      let candidate = map fst (filter (check_cond c) rcps_nvs) in  
                       return $ (intersect candidate res) 
                        --getRcpsNV :: [Recipe] -> StateError [(Recipe,NutritionalValues)]

check_cond :: Cond -> (Recipe,NutritionalValues) -> Bool
check_cond (LessThan c) (r,nv) = case c of Carb g -> carb nv < g
                                           Prot g -> prot nv < g
                                           Fats g -> fats nv < g
check_cond (MoreThan c) (r,nv) = case c of Carb g -> (carb nv) > g
                                           Prot g -> (prot nv) > g
                                           Fats g -> (fats nv) > g
check_cond (MoreThanC c) (r,nv)= getCalories nv > c --anda bien
check_cond (LessThanC c) (r,nv)= getCalories nv < c --anda bien
check_cond (With t)   (r,nv)  = elem t (maybe [] id (tags r))
check_cond (Without t) (r,nv) = not (elem t (maybe [] id (tags r)))  


preparable_with :: [Ingr] -> Recipe -> Bool
preparable_with inv r = foldr (&&) True (map (check_if_have_in inv) (ingrs r))


--Sums the ingredients on the inventory and verify that they are enough for the recipe.
check_if_have_in :: [Ingr] -> Ingr -> Bool 
check_if_have_in inv n = (foldr (+) 0 (map (\i -> if iname i == iname n then quantity i else 0) inv)) >= (quantity n)


--Adds an entry to the log
updateLog :: [Entry] -> ExpireDate -> NutritionalValues -> [Entry]
updateLog []     e nv = [Entry e (getCalories nv)]
updateLog (x:xs) e nv = let rnv = (getCalories nv) in 
                         if isSameDay (date x) e then (Entry e (total x + rnv)):xs else x : (updateLog xs e nv)

--Aux

isSameDay :: ExpireDate -> ExpireDate -> Bool
isSameDay e1 e2 = day e1 == day e2 && month e1 == month e2 && year e1 == year e2

threeRule :: Grams -> Grams -> Grams -> Grams
threeRule x y z = (z * y) / x

getCalories :: NutritionalValues -> Calorie
getCalories nv = (carb nv) * 4.0 + (prot nv) * 4.0 + (fats nv) * 9.0
