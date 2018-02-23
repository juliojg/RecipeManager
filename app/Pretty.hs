module Pretty where

import Types

import Data.Dates

import Text.PrettyPrint.HughesPJ


printEnv :: Env -> Doc
printEnv s = text (file s) <>
             text "|" <>
             text "Ingredientes:|" <>
             printList "#" (inv s) printIngr <>
             text "|" <>
             text "Recetas:|" <>
             printList "%" (rcps s) printRcp <>
             text "|Tabla:|" <>
             printList "@" (table s) printIV <>
             text "|Log:|" <>
             printList "|" (logC s) printEntry


printEntry :: Entry -> Doc
printEntry e = text "-" <>
               printExpire (date e) <>
               text "-" <>
               double (total e)

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
