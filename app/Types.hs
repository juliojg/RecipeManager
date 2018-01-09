module Types where

import Data.Dates



--AST

data RMComm = Add_ing Ingr 
            | Add_rcp Receta
            | Rm (Ingr, Cantidad) -- ver como diferenciar ingredientes con vencimientos distintos             
            | Rm_rcp Comida
            | CheckV
            | IEat Comida --remove used ingredients
            | WhatToEat (Maybe Cond)
            | WhatCanDoWith [String]
            | NewInv String

data Comm = Load String         
          | Close
          | Help
          | Display
          | Save




data IdIngr = IdIngr { ing_name :: String, 
                       datos  :: Maybe [Datos]
                     }


data Ingr = Ingr { id_ingr :: IdIngr,
                   ven    :: Maybe Vencimiento,
                   cant   :: Cantidad}



--ver como definir recetas y comidas ¿nombre?

type Paso = String

data Receta = Rcp { ingredientes :: [Ingr], 
                    pasos        :: [Paso]
                  }

data Comida = Comida { food_name          :: String, 
                       receta          :: Receta, 
                       caracteristicas :: [Tag]
                     }

--

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


