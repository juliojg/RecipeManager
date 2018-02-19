module Commands where

import Types
import Monads
import Pretty
import Parser

import Data.List
import Data.Dates
import Data.Ord

import Text.ParserCombinators.Parsec 
import Text.PrettyPrint.HughesPJ (render)
--import Text.Parsec.Token
--import Text.Parsec.Language (emptyDef)
--import Text.Parsec.Char

import Control.Monad.IO.Class
import Control.Applicative
import Control.Exception (catch,IOException)
import Control.Monad (liftM, ap)

import System.Directory (createDirectoryIfMissing)

--Carga un estado de un archivo del directorio /save donde se ejecuto 
loadRM :: String -> StateError ()
loadRM path = do file <- liftIO $ catch (readFile ("save/" ++ path)) (\e -> do let err = show (e :: IOException)
                                                                               putStrLn ("No se pudo abrir el archivo " ++ path)
                                                                               return "")
                 case parse parserEnv "" file of
                    Left err -> throw CargaFallida
                    Right s  -> do put s
                                   liftIO $ putStrLn ("Cargado archivo " ++ path) 
                                   return ()


--Guarda el estado del programa, en un archivo del directorio /save donde se ejecuto
saveRM :: StateError ()
saveRM = do s <- get 
            liftIO $ createDirectoryIfMissing False "save"
            liftIO $ writeFile ("save/" ++ (file s) ++ ".rcpm") (render (printEnv s))
            liftIO $ putStrLn ("Guardado archivo: " ++ (file s) ++ ".rcpm")             
            put (Env (file s) (inv s) (rcps s) (table s) (logC s) 1)

--Importa la tabla de valores de otro inventario al actual
importRM :: String -> StateError ()
importRM path = do r <- liftIO (catch (readFile ("save/" ++ path)) 
                                      (\e -> do let err = show (e :: IOException)
                                                putStrLn ("No se pudo abrir el archivo " ++ path)
                                                return ""))
                   case parse parserTable "" r of
                                  Left err -> throw CargaFallida
                                  Right s  -> do mapM_ addTable s
                                                 liftIO $ putStrLn ("Importada tabla de: " ++ path)
             
--Añade un ingrediente dado al inventario
addInv :: Ingr -> StateError ()
addInv i = do s <- get
              nv <- getNV (table s) (iname i) (quantity i)
              let ni = Ingr (iname i) (Just nv) (quantity i) (expire i) in 
               put (Env (file s) (searchPlace ni (inv s)) (rcps s) (table s) (logC s) 0)  
              liftIO $ putStrLn ("Ingrediente agregado: " ++ iname i)



searchPlace :: Ingr -> [Ingr] -> [Ingr]
searchPlace i []     = [i]
searchPlace i (x:xs) = if (iname i == iname x)
                       then (if expire i <= expire x 
                             then (if expire i == expire x 
                                   then (Ingr (iname i) (nutritional_values i) (quantity i + quantity x) (expire i)):xs 
                                   else i:x:xs) 
                             else x : (searchPlace' i xs))  
                       else x : (searchPlace i xs)
                      

searchPlace' :: Ingr -> [Ingr] -> [Ingr]
searchPlace' i [] = [i] 
searchPlace' i (x:xs) = if (iname i == iname x)
                        then (if expire i < expire x then i:x:xs else x : (searchPlace' i xs))  
                        else (i:x:xs)
                        

                         
--Añade una receta dada a la lista de recetas
addRcp :: Recipe -> StateError ()
addRcp r = do s <- get
              if (filter (\n -> rname r == rname n) (rcps s) == [])
              then put (Env (file s) (inv s) (r:(rcps s)) (table s) (logC s) 0)
              else throw RecetaExistente
              liftIO $ putStrLn ("Receta agregada: " ++ rname r)



--Elimina cierta cantidad de un ingrediente dado del inventario
rmInv :: String -> Grams -> StateError ()
rmInv i q = do s <- get
               ni <- rmInv' i q (inv s)  
               put (Env (file s) ni (rcps s) (table s) (logC s) 0)




--La lista ya tiene agrupados ingredientes

rmInv' :: String -> Grams -> [Ingr] -> StateError [Ingr]
rmInv' name q []     = throw IngrInsuficiente
rmInv' name q (x:xs) = if (iname x == name) 
                       then (case compare (quantity x) q of
                                                    LT -> rmInv' name (q - quantity x) xs 
                                                    EQ -> return xs
                                                    GT -> return $ Ingr name (nutritional_values x) (quantity x - q) (expire x) : xs)
                       else do is <- (rmInv' name q xs)
                               return (x : is)



--Elimina todos los ingredientes usados en una receta, de haberlos.

iEat :: String -> StateError ()
iEat name = do s <- get
               date <- liftIO getCurrentDateTime
               let r = (find (\r -> rname r == name) (rcps s)) 
               rnv <- maybe (throw RecetaInexistente) getRcpNV r
               maybe (throw RecetaInexistente) 
                     (mapM_ (\(n,q) -> rmInv n q) . map (\i -> (iname i, quantity i)) . ingrs) 
                     r
               s' <- get
               put (Env (file s') (inv s') (rcps s') (table s') (uLog (logC s') date rnv) (flag_saved s'))
               liftIO $ putStrLn (show (getCalories rnv) ++ " calorias agregadas al log")   

                




--Elimina una receta de la lista de recetas  
rmRcp :: String -> StateError ()
rmRcp name = do s <- get
                let nr = filter (\r -> rname r /= name) (rcps s) in
                    if nr == rcps s 
                    then liftIO $ putStrLn ("La siguiente receta no esta en la lista: " ++ name )
                    else do put (Env (file s) (inv s) nr (table s) (logC s) 0) 
                            liftIO $ putStrLn ("Receta eliminada: " ++ name)



--Revisa vencimientos
checkE :: StateError ()
checkE = do s <- get
            today <- liftIO getCurrentDateTime
            let res =  intercalate "\n" ((map showSimpleIngr (filter (maybe True (\e -> today > e) . expire) (inv s))))
            case res of 
              [] -> liftIO $ putStrLn ("Ningun ingrediente esta vencido")
              xs -> liftIO $ putStrLn ("Ingredientes vencidos:\n" ++ xs)
              


--Agrega un ingrediente a la tabla de valores
--VER DE ORDENARLOS ALFABETICAMENTE, HASH TABLE?
addTable :: IngValues -> StateError ()
addTable iv = do s <- get
                 ns <- verifyPlace iv (table s)
                 put (Env (file s) (inv s) (rcps s) ns (logC s) 0 )
                 

verifyPlace :: IngValues -> [IngValues] -> StateError [IngValues]
verifyPlace iv []     = return [iv]
verifyPlace iv (x:xs) = if (tname x == tname iv)
                        then throw IngrExistenteT
                        else do ys <- (verifyPlace iv xs)
                                return (x : ys)

--Elimina un ingrediente de la tabla de valores
rmTable :: String -> StateError ()
rmTable name = do s <- get
                  nt <- rmTable' name (table s)
                  total <- getTotal name (inv s)
                  rmInv name total
                  s' <- get
                  put (Env (file s) (inv s') (rcps s) nt (logC s) 0 )
                  liftIO $ putStrLn ("Datos de ingrediente " ++ name ++ " eliminados") 


rmTable' :: String -> [IngValues] -> StateError [IngValues]
rmTable' name [] = throw IngrInexistente
rmTable' name (x:xs) = if (tname x == name) then (return xs) else (rmTable' name xs)

getTotal :: String -> [Ingr] -> StateError Grams
getTotal name []     = return 0
getTotal name (x:xs) = if name == iname x 
                       then do q <- getTotal name xs 
                               return (quantity x + q)  
                       else getTotal name xs


--Obtiene los valores nutricionales de un ingr en base a la tabla
getNV :: [IngValues] -> String -> Grams -> StateError NutritionalValues
getNV []     name w = throw IngrInexistente
getNV (x:xs) name w = if tname x == name 
                      then return (NV (threeRule (portion x) w (carb (values x)))  
                                      (threeRule (portion x) w (prot (values x)))
                                      (threeRule (portion x) w (fats (values x))))
                      else getNV xs name w


threeRule :: Grams -> Grams -> Grams -> Grams
threeRule x y z = (z * y) / x


{-The Atwater system uses the average values of 4 Kcal/g for protein,
 4 Kcal/g for carbohydrate, and 9 Kcal/g for fat. -}

getCalories :: NutritionalValues -> Calorie
getCalories nv = (carb nv) * 4.0 + (prot nv) * 4.0 + (fats nv) * 9.0




getRcpNV :: Recipe -> StateError NutritionalValues
getRcpNV r = do s <- get
                aux <- mapM (\i -> getNV (table s) (iname i) (quantity i)) (ingrs r)  
                return (foldr (sumNV) accNV aux)

{-
foldr sumNV accNV (map (\i -> getNV (table s) (iname i) (quantity i)) (ingrs r)) of
                   Left err -> throw IngrInexistente
                   Right nv -> return nv      
-}

--Crea un diccionario basico con las recetas y sus respectivos valores                    
getRcpsNV :: [Recipe] -> StateError [(Recipe,NutritionalValues)]
getRcpsNV [] = return []
getRcpsNV (r:rs) = do s <- get
                      a <- getRcpNV r
                      aux <- getRcpsNV rs
                      --case foldr sumNV (Right accNV) (map (\i -> getNV (table s) (iname i) (quantity i)) (ingrs r)) of
                      --  Left err -> return aux
     {- Right nv -> -}return $ (r,a):aux      
                    

accNV :: NutritionalValues
accNV = NV 0 0 0 

sumNV :: NutritionalValues -> NutritionalValues -> NutritionalValues
sumNV n1 n2 = (NV (carb n1 + carb n2) (prot n1 + prot n2) (fats n1 + fats n2))

--Lista las comidas preparables con lo disponible

whatToEat :: Maybe [Cond] -> StateError [Recipe]
whatToEat conds = do s <- get
                     let prep = filter (preparable_with (inv s)) (rcps s) in 
                      case conds of 
                        Nothing -> return prep
                        Just xs -> having xs prep
                        

having :: [Cond] -> [Recipe] -> StateError [Recipe]
having []     rs = return rs
having (x:xs) rs = do res <- (having xs rs)
                      has <- getRcpsNV rs
                      let candidates = map fst (filter (check_cond x) has) in  
                       return $ (intersect candidates res) 
                        

check_cond :: Cond -> (Recipe,NutritionalValues) -> Bool
check_cond (LessThan c) (r,nv) = case c of Carb g -> carb nv <= g
                                           Prot g -> prot nv <= g
                                           Fats g -> fats nv <= g
check_cond (MoreThan c) (r,nv) = case c of Carb g -> (carb nv) >= g
                                           Prot g -> (prot nv) >= g
                                           Fats g -> (fats nv) >= g
check_cond (MoreThanC c) (r,nv)= getCalories nv >= c
check_cond (LessThanC c) (r,nv)= getCalories nv <= c
check_cond (With t)   (r,nv)  = elem t (maybe [] id (tags r))
check_cond (Without t) (r,nv) = not (elem t (maybe [] id (tags r)))  


preparable_with :: [Ingr] -> Recipe -> Bool
preparable_with inv r = foldr (&&) True (map (check_if_have_in inv) (ingrs r))

--Suma lo disponible en el inventario y se fija si alcanza para lo que pide la receta
check_if_have_in :: [Ingr] -> Ingr -> Bool 
check_if_have_in inv n = (foldr (+) 0 (map (\i -> if iname i == iname n then quantity i else 0) inv)) >= (quantity n)

uLog :: [Entry] -> ExpireDate -> NutritionalValues -> [Entry]
uLog []     e nv = [Entry e (getCalories nv)]
uLog (x:xs) e nv = let rnv = (getCalories nv) in 
                    if isSameDay (date x) e then (Entry e (total x + rnv)):xs else uLog xs e nv 

isSameDay :: ExpireDate -> ExpireDate -> Bool
isSameDay e1 e2 = day e1 == day e2 && month e1 == month e2 && year e1 == year e2






