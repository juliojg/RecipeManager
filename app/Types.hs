module Types where

import Data.Dates
import Data.Char
import Data.List

--AST

data RMComm = Add_ing Ingr 
            | Add_rcp Recipe
            | Rm (String, Grams)
            | Rm_rcp String
            | CheckV
            | IEat String
            | WTE (Maybe [Cond]) --what to eat
            | RMHelp
            | RMSave
            | RMClose
            | Display
            | AddTable IngValues
            | RmTable String             
            | ImportTable String

data Comm = Load String         
          | Quit
          | Help
          | NewInv String

data Ingr = Ingr { iname :: String, 
                   nutritional_values :: Maybe NutritionalValues,
                   quantity :: Grams, 
                   expire :: Maybe ExpireDate
                 }


data IngValues = IV {tname   :: String,
                     portion :: Grams,
                     values  :: NutritionalValues}



data Recipe = Rcp { rname :: String,
                    ingrs :: [Ingr],
                    steps :: [Step],  
                    tags  :: Maybe [Tag]
                  }

type Tag = String --Desayuno / Fria / etc. 

data NutritionalValues = NV {carb  :: Grams,
                             prot  :: Grams,
                             fats  :: Grams}

data MacroNutrient = Carb Grams | Prot Grams | Fats Grams


type ExpireDate = DateTime 


type Step = String

type Grams = Double

type Calorie = Double

data Cond = LessThan MacroNutrient | MoreThan MacroNutrient | With Tag | Without Tag | LessThanC Calorie| MoreThanC Calorie

--The state that the program will carry

data Env = Env {file  :: String,
                inv   :: [Ingr],
                rcps  :: [Recipe],
                table :: [IngValues], 
                flag_saved :: Int}


--Eq

instance Eq Recipe where
    r1 == r2 = (rname r1 == rname r2)


-- Show

showExpireDate :: ExpireDate -> String
showExpireDate e = (show (day e)) ++ "/" ++ (show (month e)) ++ "/" ++ (show (year e))


instance Show IngValues where
    show = showIV

showIV :: IngValues -> String
showIV iv = (tname iv) ++ " " ++ show (portion iv) ++ " " ++ show (values iv) 

instance Show NutritionalValues where
    show = showNV

showNV :: NutritionalValues -> String
showNV nv = "Carbohidratos: " ++ show (myRound (carb nv) 2) ++ " - " ++
            "Proteinas: " ++ show (myRound (prot nv) 2) ++ " - " ++
            "Grasas: " ++ show (myRound (fats nv) 2)



instance Show Env where
    show = showEnv

showEnv :: Env -> String
showEnv s = "Mostrando inventario: " ++ (file s) ++
            "\n\nIngredientes:" ++
             concat (map (('\n':) . ('\n':) . show) (inv s)) ++
            "\n\nRecetas: " ++ "\n" ++
             concat (map (('\n':) . show ) (rcps s))            

instance Show Cond where
    show (With t) = "si"++show t
    show (Without t) = "no"++show t
    show (LessThan nv) = "menosq"++show nv
    show (MoreThan nv) = "masq"++show nv                

instance Show MacroNutrient where
    show (Carb g) = "carb "++show g
    show (Prot g) = "prot "++show g
    show (Fats g) = "fats "++show g
    

myRound :: Grams -> Integer -> Grams
myRound n digits = fromIntegral (round (n * (10 ^ digits))) / (10 ^ digits)

instance Show Ingr where
 show = showIngr 


showIngr :: Ingr -> String
showIngr i = (iname i) ++ " - Cantidad: " ++ (show (quantity i) ++ "\n" ++
             maybe "" showNV (nutritional_values i) ++ "\n" ++
             "Vencimiento: " ++ (maybe "" showExpireDate (expire i)))

showSimpleIngr :: Ingr -> String
showSimpleIngr i = (iname i) ++ " - Cantidad: " ++ (show (quantity i) ++ " - Vencimiento: " ++ (maybe "" showExpireDate (expire i)))

instance Show Recipe where
 show = showRcp 

{-
mapBut1 :: (a -> a) -> [a] -> [a]
mapBut1 f []  = []
mapBut1 f [x] = [x] 
mapBut1 f (x:xs) = f x : mapBut1 f xs
-}

showRcp :: Recipe -> String
showRcp r = "Nombre: " ++ (rname r) ++ "\n" ++ 
            "Ingredientes: " ++ intercalate ", " (map showIngrRcp (ingrs r)) ++ "\n" ++
            "Pasos: " ++ "\n" ++ (concat (map ((\s -> ("+) " ++ s ++ "\n")) . show) (steps r))) ++
            "Tags: " ++  (intercalate ", " ((maybe [] id (tags r)))) ++ "\n"



showIngrRcp :: Ingr -> String
showIngrRcp i = (iname i) ++ " " ++ (show (quantity i)) 
