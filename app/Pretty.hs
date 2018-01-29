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
printIngr i = text (iname i) <>
              text "-" <> 
              int (quantity i) <>
              maybe (empty) (\e -> printExpire e) (expire i)              
                        

{-
printIngr' i (x:xs) = text (ing_name i) <>
                      text "-" <> 
                      int (snd x) <>
                      case fst x of
                        Nothing -> text ";" <> printIngr' i xs
                        Just v  -> text "-" <>
                                   printV (fst x) <>
                                   text "#" <> 
                                   printIngr' i xs
-}
printExpire :: ExpireDate -> Doc
printExpire e = int (day e) <>
                text "/" <>
                int (month e) <>
                text "/" <>
                int (year e)


printList :: String -> [a] -> (a -> Doc) -> Doc
printList sep []     _ = empty
printList sep [x]    p = p x
printList sep (x:xs) p = (p x) <> text sep <> printList sep xs p   


printRcp :: Recipe -> Doc
printRcp r = text (rname r) <>
             text "..." <>
             printList ";" (ingrs r) printIngr <>
             text "..." <>
             printList ";" (steps r) (\p -> text p) <>
             text ";" <>
             maybe (empty) (\xs -> (text "..." <> (printList ";"  xs (\a-> text a)) <> text ";")) (tags r) <>
             text ":f"
         



--inv1|Ingredientes:|Queso-20-11/12/2019#Salsa-20-11/12/2017#Harina-10-11/12/2018|Recetas:|MilanesaNapolitana...Queso-10;Salsa-7;Milanesa-1...Paso1;Paso2;Paso3;:f|Pizza...Queso-10;Salsa-7;Harina-15...Paso1;Paso2;Paso3;:f



