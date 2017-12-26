module Types where

import Data.Dates

data IdIngr = IdIngr (String, Maybe ([Datos]))

data Ingr = Ingr (IdIngr, Maybe Vencimiento, Cantidad) -- para las recetas vencimiento será Nothing 

data Inventario = Inv [Ingr]

--

data Paso = Paso (Int, String)

data Receta = Rcp ([Ingr], [Paso])

data Comida = Comida (String, Receta, Tipo)

--

data Tipo = Desayuno | Almuerzo | Merienda | Cena | Fria | Caliente 

data Datos = Calorias Double | Carbohidratos Double | Proteinas Double | Lipidos Double

type Vencimiento = DateTime

type Cantidad = Int

data Conditions = Have [Datos] 


