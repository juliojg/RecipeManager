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
             printList "%" (rcps s) printRcp <>
             text "|Tabla:|" <>
             printList "|" (table s) printIV

{-
inv1|Ingredientes:|Queso-10.0-6.0 11.5 2.0-11/12/2019#Salsa-10.0-0.2 0.2 0.2-11/12/2019|Recetas:|MilanesaNap-iQueso-20.0;Salsa-10.0-ppaso1;paso2;-tCaliente;-f%Pizza-iQueso-20.0;Salsa-10.0-ppaso1;paso2;-tCaliente;-f|Tabla:|Queso-20.0-12.0 23.0 4.0|Salsa-100.0-2.0 2.0 2.0;
-}


printIV :: IngValues -> Doc
printIV iv = text (tname iv) <>
             text "-" <>
             double (portion iv) <>
             text "-" <>
             printNV (values iv)


printIngr :: Ingr -> Doc
printIngr i = text (iname i) <>
              text "-" <> 
              double (quantity i) <>
              maybe empty (\nv -> text "-" <> printNV nv) (nutritional_values i) <>     
              maybe empty (\e -> (text "-" <> printExpire e)) (expire i)              


printNV :: NutritionalValues -> Doc
printNV nv = double (carb nv) <>
             text " " <>
             double (prot nv) <>
             text " " <>
             double (fats nv)

printExpire :: ExpireDate -> Doc
printExpire e = int (day e) <>
                text "/" <>
                int (month e) <>
                text "/" <>
                int (year e)


printList :: String -> [a] -> (a -> Doc) -> Doc
printList sep []     _ = empty 
printList sep (x:xs) p = (p x) <> text sep <> printList sep xs p   

printList2 :: String -> [a] -> (a -> Doc) -> Doc
printList2 sep []     _ = empty
printList2 sep [x]    p = p x 
printList2 sep (x:xs) p = (p x) <> text sep <> printList2 sep xs p   



printRcp :: Recipe -> Doc
printRcp r = text (rname r) <>
             text "-i" <>
             printList2 ";" (ingrs r) printIngr <>
             text "-p" <>
             printList ";" (steps r) (\p -> text p) <>
             text "-t" <>
             maybe (text "-f") (\xs -> ((printList ";"  xs (\a-> text a)) <> text "-f")) (tags r) 
             
