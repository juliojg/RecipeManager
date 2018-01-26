module Types where

import Data.Dates
import Data.Char


--AST

data RMComm = Add_ing Ingr 
            | Add_rcp Receta
            | Rm (String, Cantidad)
            | Rm_rcp String
            | CheckV
            | IEat String
            | WhatToEat (Maybe Cond)
            | WhatCanDoWith [String]
            | RMHelp
            | RMSave
            | RMQuit
            | Display

data Comm = Load String         
          | Close
          | Help
          | NewInv String

data Ingr = Ingr { ing_name :: String, 
                   datos  :: Maybe Datos,
                   stock :: [(Maybe Vencimiento, Cantidad)]
                 }


type Paso = String

data Receta = Rcp { rcp_name :: String,
                    ingredientes :: [Ingr],
                    pasos        :: [Paso],  
                    caracteristicas :: Maybe [Tag]
                  }


type Tag = String -- Desayuno / Fria / etc. 

data Datos = Datos {calorias       :: Double,
                    carbohidratos  :: Gramos,
                    proteinas      :: Gramos,
                    lipidos        :: Gramos}

type Vencimiento = DateTime

type Cantidad = Int

type Gramos = Double

data Cond = Cond [Datos] 

--El estado que llevará el programa
data Env = Env { file :: String,
                 inv  :: [Ingr],
                 rcps :: [Receta],
                 flag_saved :: Int 
               }


--Eq

instance Eq Receta where
    r1 == r2 = (rcp_name r1 == rcp_name r2)


-- Show



instance Show Ingr where
 show = showIngr 

showStock :: (Maybe Vencimiento, Cantidad) -> String 
showStock (v, c) = case v of Just a  -> " Vence: " ++ (show a) ++ " Cantidad: " ++ Prelude.show c
                             Nothing -> [] ++ Prelude.show c





instance Show Receta where
 show = showRcp 


showRcp :: Receta -> String
showRcp r = "Nombre: " ++ (rcp_name r) ++ "\n" ++ 
            "Ingredientes: " ++ (concat (map ((++ ", ") . show) (ingredientes r))) ++ "\n" ++
            "Pasos: " ++ (concat (map ((++ ", ") . Prelude.show) (pasos r)))

 

showIngr :: Ingr -> String
showIngr i = (ing_name i) ++ (case (datos i) of Just a -> undefined 
                                                Nothing -> [] ) 
             ++ " " ++ concat (map showStock (stock i))



instance Show Env where
    show = showEnv


showEnv :: Env -> String
showEnv s = "Mostrando inventario: " ++ (file s) ++
            "\nIngredientes: " ++
             concat (map (('\n':) . show) (inv s)) ++
            "\nRecetas: " ++
             concat (map (('\n':) . show ) (rcps s))            
                

