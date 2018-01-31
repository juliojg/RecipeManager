module Types where

import Data.Dates
import Data.Char


--AST

data RMComm = Add_ing Ingr 
            | Add_rcp Recipe
            | Rm (String, Grams)
            | Rm_rcp String
            | CheckV
            | IEat String
            | WTE (Maybe Cond) --what to eat
            | WCDW [String] -- what can do with
            | RMHelp
            | RMSave
            | RMClose
            | Display
            | AddTable IngValues
            | RmTable String
            | AddTag String             


data Comm = Load String         
          | Quit
          | Help
          | NewInv String

data Ingr = Ingr { iname :: String, 
                   nutritional_values :: Maybe NutritionalValues,
                   quantity :: Grams, --either para mililitros?
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

data Calories = Carb Int | Prot Int | Fats Int


type ExpireDate = DateTime 


type Step = String

type Grams = Double

type KiloCalorie = Double

data Cond = Cond [NutritionalValues] 

--El estado que llevara el programa
data Env = Env {file  :: String,
                inv   :: [Ingr],
                rcps  :: [Recipe],
                table :: [IngValues],
                tag_list  :: [Tag], 
                flag_saved :: Int}


--Eq

instance Eq Recipe where
    r1 == r2 = (rname r1 == rname r2)


-- Show

instance Show Ingr where
 show = showIngr 

instance Show Recipe where
 show = showRcp 

showRcp :: Recipe -> String
showRcp r = "Nombre: " ++ (rname r) ++ "\n" ++ 
            "Ingredientes: " ++ (concat (map ((++ ", ") . show) (ingrs r))) ++ "\n" ++
            "Pasos: " ++ (concat (map ((++ ", ") . Prelude.show) (steps r)))

showIngr :: Ingr -> String
showIngr i = (iname i) ++ " " ++ (show (quantity i) ++ " " ++
             maybe "" showNV (nutritional_values i) ++ " " ++
             (maybe "" (\x -> show x) (expire i)))




{-
instance Show ExpireDate where
    show = showExpireDate

showExpireDate :: ExpireDate -> String
showExpireDate e = (day e) ++ "/" ++ (month e) ++ "/" ++ (year e)
-}

instance Show IngValues where
    show = showIV

showIV :: IngValues -> String
showIV iv = (tname iv) ++ " " ++ show (portion iv) ++ " " ++ show (values iv) 

instance Show NutritionalValues where
    show = showNV

showNV :: NutritionalValues -> String
showNV nv = show (carb nv) ++ " " ++
            show (prot nv) ++ " " ++
            show (fats nv) 



instance Show Env where
    show = showEnv

showEnv :: Env -> String
showEnv s = "Mostrando inventario: " ++ (file s) ++
            "\nIngredientes: " ++
             concat (map (('\n':) . show) (inv s)) ++
            "\nRecetas: " ++
             concat (map (('\n':) . show ) (rcps s))            
                

