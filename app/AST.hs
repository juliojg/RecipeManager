module AST where

import Types

data Comm = Add_inv Ingr 
            | Add_rcp Receta

            | Rm (Ingr, Cantidad) -- ver como diferenciar ingredientes con vencimientos distintos             
            | Rm_rcp Comida

            | CheckV -- revisa vencimientos en la fecha actual

            | IEat Comida --quita los ingredientes usados de un comida del inventario
            | WhatToEat (Maybe Conditions)
            | WhatCanDo [Ingr]


data InterComm = Load String
                 | Save
                 | Close
                 | Help
                 | Display -- muestra inv


{-
"Queso, 2 kl, 01-01-2018 "

IdQueso = IdIngr ("Queso", Nothing)

IngrQueso = Ingr (IdQueso, "01-01-2018", 2)

-

"Pan, 1 kl, 01-01-2018, ch 200"

IdPan = IdIngr ("Queso", [Carbohidratos 200])

IngrPan = Ingr (IdPan, "01-01-2018", 2)


-}

