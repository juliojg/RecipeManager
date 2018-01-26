module Parser where

import Text.ParserCombinators.Parsec 
import Text.Parsec.Token
import Text.Parsec.Language (emptyDef)
import Text.Parsec.Char
import Text.PrettyPrint.HughesPJ (render)

import Pretty
import Types
import Data.Dates


-----------------------
-- Funcion para facilitar el testing del parser.
totParser :: Parser a -> Parser a
totParser p = do 
                  whiteSpace lis
                  t <- p
                  eof
                  return t

-- Analizador de Tokens
lis :: TokenParser u
lis = makeTokenParser (emptyDef   { commentStart    = "/*"
                                  , commentEnd      = "*/"
                                  , commentLine     = "//"
                                  , opLetter        = char '='
                                  , reservedOpNames = []
                                  , reservedNames   = [ "add_ing", "add_rcp", "rm_ing", "rm_rcp",
                                                        "check", "i_eat", "need_food", 
                                                        "new_inv", "save", "load", "close", "help", "display", "quit"]
                                  })

--add_ing Queso; 200; 20_12_2018



--Parser archivos guardados
parserEnv :: Parser Env
parserEnv = do name <- identifier lis
               string "|Ingredientes:|"
               ingrs <- sepEndBy (parserIng) (string "#")
               string "|Recetas:|"
               rcps  <- sepEndBy (parserRcp) (string "|")
               return (Env name (ingrs) (rcps) 1)


-- Parser de ingredientes
parserIng :: Parser Ingr
parserIng = do n <- identifier lis
               symbol lis "-" 
               w <- natural lis
               v <- optionMaybe parserV
               --parseDatos    -- idem parseV
               return (Ingr n Nothing [(v,(fromInteger w))])


--Parser de fechas
parserV :: Parser Vencimiento
parserV = do symbol lis "-"
             d <- natural lis --ver que pasa si consume algo de la entrada
             symbol lis "/"
             m <- natural lis
             symbol lis "/"
             y <- natural lis
             return (DateTime (fromIntegral y) 
                              (fromIntegral m) 
                              (fromIntegral d) 0 0 0) 

-- add_rcp Pizza ... Harina - 200; Salsa - 50; Queso - 150 ... Abrir la salsa; Abrir el queso :f

--Parser de recetas
parserRcp :: Parser Receta
parserRcp = do name <- identifier lis
               string "..."
               spaces
               ingrs <- manyTill (sepBy1 parserIngRcp (string ";")) (parserEnd "...")
               spaces
               pasos <- manyTill (parserPaso) (string ":f")
               return (Rcp name (foldr (++) [] ingrs) (pasos) Nothing) --agregar tags 
               
parserEnd :: String -> Parser String
parserEnd end = do {spaces ; string end}

-- "Pizza...Harina-200-11/09/1998;Salsa-50-20/11/2018;Queso-150-19/09/1990...Abrir la salsa;Abrir el queso;:f"

parserPaso :: Parser Paso
parserPaso = do {spaces ; manyTill anyChar (try (string ";"))} 


parserIngRcp :: Parser Ingr
parserIngRcp = do spaces
                  n <- identifier lis
                  symbol lis "-" 
                  w <- natural lis
                  return (Ingr n Nothing [(Nothing,(fromInteger w))])





--Parser comandos
parseRMComm :: Parser RMComm --quitar tantos try
parseRMComm =     try (do{ (reserved lis) "add_ing"; ing <- parserIng; return (Add_ing ing) })
              <|> (do{ (reserved lis) "add_rcp";  rcp <- parserRcp; return (Add_rcp rcp) })
              <|> (do{ (reserved lis) "rm_ing"; ing <- identifier lis; n <- natural lis; return (Rm (ing, fromInteger n)) })
              <|> (do{ (reserved lis) "rm_rcp"; rcp_name <- identifier lis; return (undefined) })
              <|> (do{ (reserved lis) "check"; return CheckV })
              <|> (do{ (reserved lis) "i_eat"; food_name <- identifier lis; return undefined })
              <|> (do{ (reserved lis) "need_food"; return (WhatToEat (Nothing)) })
              <|> (do{ (reserved lis) "what_with"; xs <- many1 (sepBy1 (identifier lis) (string ";")); return (WhatCanDoWith (foldr (++) [] xs)) })
              <|> (do{ (reserved lis) "help"; return RMHelp})
              <|> (do{ (reserved lis) "save"; return RMSave})
              <|> (do{ (reserved lis) "quit"; return RMQuit})
              <|> (do{ (reserved lis) "display"; return Display })

parseComm :: Parser Comm
parseComm =       try (do{ (reserved lis) "load"; name <- identifier lis; string ".txt"; return (Load (name ++ ".txt")) })
              <|> (do{ (reserved lis) "close"; return Close })
              <|> (do{ (reserved lis) "help"; return Help })
              <|> (do{ (reserved lis) "new_inv"; inv_name <- identifier lis; return (NewInv inv_name) })
               



{-
"Queso, 2 kl, 01-01-2018 "

IdQueso = IdIngr ("Queso", Nothing)

IngrQueso = Ingr (IdQueso, "01-01-2018", 2)
-}

