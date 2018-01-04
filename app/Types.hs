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
            | WhatCanDoWith [Ingr]
            | NewInv String

data Comm = Load String         
          | Close
          | Help
          | Display
          | Save




data IdIngr = IdIngr { ing_name :: String, 
                       datos  :: Maybe [Datos]
                     }

data Ingr = Ingr (IdIngr, Maybe Vencimiento, Cantidad) -- para las recetas vencimiento será Nothing




--

type Paso = String

data Receta = Rcp { ingredientes :: [Ingr], 
                    pasos        :: [Paso]
                  }

data Comida = Comida { food_name          :: String, 
                       receta          :: Receta, 
                       caracteristicas :: [Tipo]
                     }

--

data Tipo = Desayuno | Almuerzo | Merienda | Cena | Fria | Caliente 

data Datos = Calorias Double | Carbohidratos Double | Proteinas Double | Lipidos Double

data Vencimiento = Vencimiento (DateTime, DateTime, DateTime)

type Cantidad = Int

data Cond = Cond [Datos] 



--el estado que llevará el programa
data Env = Env { inv  :: [Ingr],
                 rcps :: [Receta]
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


