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

--data Env = Env{inv::[Ingr],rcps::[Receta],flag_saved :Int}

loadRM :: String -> IO Env
loadRM path = do file <- readFile ("app/save/" ++ path)
                 case parse parserEnv "" file of
                    Left err  -> do putStrLn "Error de lectura"; return (Env "" [] [] 1)
                    Right s   -> return s

saveRM :: StateError ()
saveRM = do s <- get 
            liftIO $ putStrLn ("Guardando archivo: " ++ (file s) ++ ".txt")            
            liftIO $ writeFile ("app/save/" ++ (file s) ++ ".txt") (render (printEnv s))
            put (Env (file s) (inv s) (rcps s) 1)


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
              put (Env {file = file s, inv = searchAndPutI (inv s) i, rcps = rcps s, flag_saved = 0})


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
                         Right ok  -> put (Env {file = file s, inv = inv s, rcps = ok, flag_saved = 0})


checkExistenceR :: [Receta] -> Receta -> Bool
checkExistenceR rcps n = case filter (\r -> rcp_name n == rcp_name r) rcps of
                                                                            []     -> False
                                                                            (x:xs) -> True

searchAndPutR :: [Receta] -> Receta -> Either Error [Receta]
searchAndPutR []     r = Right [r]
searchAndPutR (x:xs) r = case checkExistenceR (x:xs) r of
                                                       False  -> Right (r:x:xs)
                                                       True   -> Left RecetaExistente

--Elimina cierta cantidad de un ingrediente dado del inventario
rmInv :: String -> Cantidad -> StateError ()
rmInv i c = do s <- get
               case filter (\e -> ing_name e == i) (inv s) of
                                                            [] -> throw IngrInexistente
                                                            [x]  -> removeIngr i x c (inv s) s 
                                                                    

removeIngr :: String -> Ingr -> Cantidad -> [Ingr] -> Env -> StateError ()
removeIngr n i c xs s = let a = stock i in 
                        case checkStock a c of
                                             Left err -> throw err
                                             Right [] -> put $ Env (file s) (deleteI n (inv s)) (rcps s) 0
                                             Right newstock -> put $ Env (file s) (replace n newstock xs) (rcps s) 0


deleteI :: String -> [Ingr] -> [Ingr]
deleteI n []     = []
deleteI n (x:xs) = if ing_name x == n then xs else x : (deleteI n xs)

replace :: String -> [(Maybe Vencimiento, Cantidad)] -> [Ingr] -> [Ingr]
replace name ns []     = []
replace name ns (x:xs) = if name == ing_name x 
                         then (Ingr {ing_name = ing_name x, datos = datos x, stock = ns}) : xs
                         else x : (replace name ns xs)



checkStock :: [(Maybe Vencimiento, Cantidad)] -> Int -> Either Error [(Maybe Vencimiento, Cantidad)]
checkStock a          0    = Right a
checkStock []         need = Left IngrInsuficiente
checkStock [(v,c)]    need = let n = c - need in
                             if n >= 0 
                             then (if (n > 0) then Right [(v, n)] else Right [] )
                             else Left IngrInsuficiente 

checkStock ((v,c):xs) need = let n = c - need in
                             if n >= 0 
                             then (if (n > 0) then Right ((v, n) : xs) else Right xs )
                             else checkStock xs (- n) 

--Elimina una receta de la lista de recetas  
rmRcp :: String -> StateError ()
rmRcp name = do s <- get
                let nr = filter (\r -> rcp_name r /= name) (rcps s) in
                    if nr == rcps s 
                    then throw RecetaInexistente
                    else put (Env {file = file s, inv = inv s, rcps = nr, flag_saved = 0}) 


--Revisa vencimientos
checkV :: Vencimiento -> StateError [Ingr]
checkV hoy = do s <- get
                return (filter (checkV1 hoy) (inv s))

checkV1 :: Vencimiento -> Ingr -> Bool
checkV1 hoy i = case stock i of
                    []           -> True
                    (mv, c):xs   -> case mv of 
                                             Nothing -> True
                                             Just v  -> (hoy > v)


