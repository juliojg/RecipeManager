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
            | WTE (Maybe [Cond])
            | RMHelp
            | RMSave
            | RMClose
            | Display
            | AddTable IngValues
            | RmTable String             
            | ImportTable String
            | ShowT

data Comm = Load String         
          | Quit
          | Help
          | NewInv String

data Ingr = Ingr { iname :: String,
                   quantity :: Grams, 
                   expire :: Maybe ExpireDate
                 }


data Recipe = Rcp { rname :: String,
                    ingrs :: [Ingr],
                    steps :: [Step],  
                    tags  :: Maybe [Tag]
                  }


data IngValues = IV {tname   :: String,
                     portion :: Grams,
                     values  :: NutritionalValues}



data NutritionalValues = NV {carb  :: Grams,
                             prot  :: Grams,
                             fats  :: Grams}

data Entry = Entry {date :: DateTime, total :: Calorie}

data MacroNutrient = Carb Grams | Prot Grams | Fats Grams

data Cond = LessThan MacroNutrient | MoreThan MacroNutrient | With Tag | Without Tag | LessThanC Calorie| MoreThanC Calorie

type ExpireDate = DateTime 

type Step = String

type Grams = Double

type Calorie = Double

type Tag = String

--The state that the program will carry

data Env = Env {file  :: String,
                inv   :: [Ingr],
                rcps  :: [Recipe],
                table :: [IngValues],
                logC   :: [Entry], 
                flag_saved :: Int}

--Eq

instance Eq Recipe where
    r1 == r2 = (rname r1 == rname r2)

instance Eq IngValues where
    iv1 == iv2 = (tname iv1 == tname iv2)

--Show

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
showEnv s = "\n" ++
            "Mostrando inventario: " ++ (file s) ++
            "\n\nIngredientes:" ++
             concat (map (('\n':) . show) (inv s)) ++
            "\n\nRecetas: " ++ "\n" ++
             concat (map (('\n':) . show ) (rcps s)) ++            
            "\n\nLog: " ++ "\n" ++
             concat (map (('\n':) . show ) (logC s)) ++
            "\n------"            

instance Show Cond where
    show (With t) = "si"++show t
    show (Without t) = "no"++show t
    show (LessThan nv) = "menosq"++show nv
    show (MoreThan nv) = "masq"++show nv                

instance Show MacroNutrient where
    show (Carb g) = "carb "++show g
    show (Prot g) = "prot "++show g
    show (Fats g) = "fats "++show g
    

instance Show Ingr where
 show = showIngr 

showIngr :: Ingr -> String
showIngr i = (iname i) ++ " - Cantidad: " ++ (show (quantity i) ++ " - " ++
             "Vencimiento: " ++ (maybe "" showExpireDate (expire i)))

showSimpleIngr :: Ingr -> String
showSimpleIngr i = (iname i) ++ " - Cantidad: " ++ (show (quantity i) ++ " - Vencimiento: " ++ (maybe "" showExpireDate (expire i)))

instance Show Recipe where
 show = showRcp 

showRcp :: Recipe -> String
showRcp r = "Nombre: " ++ (rname r) ++ "\n" ++ 
            "Ingredientes: " ++ intercalate ", " (map showIngrRcp (ingrs r)) ++ "\n" ++
            "Pasos: " ++ "\n" ++ (concat (map ((\s -> ("+) " ++ s ++ "\n")) . show) (steps r))) ++
            "Tags: " ++  (intercalate ", " ((maybe [] id (tags r)))) ++ "\n"


showIngrRcp :: Ingr -> String
showIngrRcp i = (iname i) ++ " " ++ (show (quantity i)) 


instance Show Entry where
    show = showEntry

showEntry :: Entry -> String
showEntry e = "El " ++ showExpireDate (date e) ++ " se ingirieron " ++ show (total e) ++ " calorias"

--Aux

myRound :: Grams -> Integer -> Grams
myRound n digits = fromIntegral (round (n * (10 ^ digits))) / (10 ^ digits)

