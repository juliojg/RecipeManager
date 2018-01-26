module Pretty where

import Text.PrettyPrint.HughesPJ
import Data.Dates
import Types

         
--inv1|Ingredientes:|Queso-20-11/12/2019#Salsa-20-11/12/2017#Harina-10-11/12/2018|Recetas:|MilanesaNapolitana...Queso-10;Salsa-7;Milanesa-1...Paso1;Paso2;Paso3;:f|Pizza...Queso-10;Salsa-7;Harina-15...Paso1;Paso2;Paso3;:f



printEnv :: Env -> Doc
printEnv s = text (file s) <>
             text "|" <>
             text "Ingredientes:|" <>
             printList "#" (inv s) printIngr <>
             text "|" <>
             text "Recetas:|" <>
             printList "|" (rcps s) printRcp
             



printIngr :: Ingr -> Doc
printIngr i = printIngr' i (stock i)

printIngr' :: Ingr -> [(Maybe Vencimiento, Cantidad)] -> Doc
printIngr' i []     = empty
printIngr' i [x]    = text (ing_name i) <>
                      text "-" <> 
                      int (snd x) <>
                      case fst x of
                        Nothing -> empty
                        Just v  -> text "-" <>
                                   printV (fst x)

printIngr' i (x:xs) = text (ing_name i) <>
                      text "-" <> 
                      int (snd x) <>
                      case fst x of
                        Nothing -> text ";" <> printIngr' i xs
                        Just v  -> text "-" <>
                                   printV (fst x) <>
                                   text "#" <> 
                                   printIngr' i xs

printV :: Maybe Vencimiento -> Doc
printV Nothing  = empty
printV (Just v) = int (day v) <>
                  text "/" <>
                  int (month v) <>
                  text "/" <>
                  int (year v)

printList :: String -> [a] -> (a -> Doc) -> Doc
printList sep []     _ = empty
printList sep [x]    p = p x
printList sep (x:xs) p = (p x) <> text sep <> printList sep xs p   


printRcp :: Receta -> Doc
printRcp r = text (rcp_name r) <>
             text "..." <>
             printList ";" (ingredientes r) printIngr <>
             text "..." <>
             printList ";" (pasos r) (\p -> text p) <>
             text ";" <>
             text ":f"
         
--inv1|Ingredientes:|Queso-20-11/12/2019#Salsa-20-11/12/2017#Harina-10-11/12/2018|Recetas:|MilanesaNapolitana...Queso-10;Salsa-7;Milanesa-1...Paso1;Paso2;Paso3;:f|Pizza...Queso-10;Salsa-7;Harina-15...Paso1;Paso2;Paso3;:f



{-

data Ingr = Ingr { ing_name :: String, 
                   datos  :: Maybe Datos,
                   stock :: [(Maybe Vencimiento, Cantidad)]
                 }


data Receta = Rcp { rcp_name :: String,
                    ingredientes :: [Ingr],
                    pasos        :: [Paso],  
                    caracteristicas :: Maybe [Tag]
                  }



-}
