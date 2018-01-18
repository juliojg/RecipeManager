module Types where

import Data.Dates
import Data.Char


--AST

data RMComm = Add_ing Ingr 
            | Add_rcp Receta
            | Rm (Ingr, Cantidad) -- ver como diferenciar ingredientes con vencimientos distintos             
            | Rm_rcp Receta
            | CheckV
            | IEat Receta --remove used ingredients
            | WhatToEat (Maybe Cond)
            | WhatCanDoWith [String]
            | NewInv String

data Comm = Load String         
          | Close
          | Help
          | Display
          | Save

data Ingr = Ingr { ing_name :: String, 
                   datos  :: Maybe [Datos],
                   stock :: [(Maybe Vencimiento, Cantidad)]
                 }


--ver como definir recetas y comidas ¿nombre?


type Paso = String

data Receta = Rcp { rcp_name :: String,
                    ingredientes :: [Ingr],
                    pasos        :: [Paso],  
                    caracteristicas :: Maybe [Tag]
                  }


type Tag = String -- Desayuno / Fria / etc. 

data Datos = Calorias Double | Carbohidratos Double | Proteinas Double | Lipidos Double

type Vencimiento = DateTime

type Cantidad = Int

data Cond = Cond [Datos] 

--el estado que llevará el programa
data Env = Env { inv  :: [Ingr],
                 rcps :: [Receta],
                 flag_saved :: Int 
               }



{-
"Queso, 2 kl, 01-01-2018 "

IdQueso = IdIngr ("Queso", Nothing)

IngrQueso = Ingr (IdQueso, "01-01-2018", 2)

-

"Pan, 1 kl, 01-01-2018, ch 200"

IdPan = IdIngr ("Queso", [Carbohidratos 200])

IngrPan = Ingr (IdPan, "01-01-2018", 2)


-}

-- Show



instance Show Ingr where
 show = showIngr 

showStock :: (Maybe Vencimiento, Cantidad) -> String 
showStock (v, c) = (case v of Just a -> (show a) ++ " "
                              Nothing -> []) ++ Prelude.show c ++ " "





instance Show Receta where
 show = showRcp 


showRcp :: Receta -> String
showRcp r = "Nombre: " ++ (rcp_name r) ++ "\n" ++ 
            "Ingredientes: " ++ (concat (map ((++ ", ") . show) (ingredientes r))) ++ "\n" ++
            "Pasos: " ++ (concat (map ((++ ", ") . Prelude.show) (pasos r)))

 

showIngr :: Ingr -> String
showIngr i = (ing_name i) ++ (case (datos i) of Just a -> undefined 
                                                Nothing -> [] ) 
             ++ concat (map showStock (stock i))


