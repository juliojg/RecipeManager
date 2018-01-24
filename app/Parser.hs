module Parser where

import Text.ParserCombinators.Parsec 
import Text.Parsec.Token
import Text.Parsec.Language (emptyDef)
import Text.Parsec.Char

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
                                  , reservedNames   = [ "add_ingr", "add_rcp", "rm_ing", "rm_rcp",
                                                        "check", "i_eat", "need_food", 
                                                        "new_inv", "save", "load", "close", "help", "display"]
                                  })

--add_ing Queso; 200; 20_12_2018

-- Parser de ingredientes
parserIng :: Parser Ingr
parserIng = do spaces
               n <- identifier lis
               symbol lis "-" 
               w <- natural lis
               v <- optionMaybe ( do {skipMany (symbol lis "-") ; parserV})
               --parseDatos    -- idem parseV
               return (makeIngr n (fromInteger w) v)


{-
data Ingr = Ingr { ing_name :: String, 
                   datos  :: Maybe [Datos]
                   stock :: [(Maybe Vencimiento, Cantidad)]
                 }

-}

makeIngr :: String -> Cantidad -> Maybe Vencimiento -> Ingr
makeIngr n c v = Ingr { ing_name = n, 
                        datos = Nothing,
                        stock = [(v, c)]
                      } 
--Parser de fechas
parserV :: Parser Vencimiento
parserV = do d <- natural lis --ver que pasa si consume algo de la entrada
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
               ingrs <- manyTill (sepBy (try parserIng) (string ";")) (parserEnd "...")
               spaces
               pasos <- manyTill (parserPaso) (string ":f")
               return (Rcp name (foldr (++) [] ingrs) (pasos) Nothing) --agregar tags 
               
parserEnd :: String -> Parser String
parserEnd end = do {spaces ; string end}

-- "Pizza...Harina-200-11_09_1998;Salsa-50-20/11/2018;Queso-150-19/09/1990...Abrir la salsa_Abrir el queso_:f"

parserPaso :: Parser Paso
parserPaso = do {spaces ; manyTill anyChar (try (string ";"))} 




--Parser comandos
parseRMComm :: Parser RMComm --quitar tantos try
parseRMComm =     try (do{ (reserved lis) "add_ingr"; ing <- parserIng; return (Add_ing ing) })
              <|> try (do{ (reserved lis) "add_rcp";  rcp <- parserRcp; return (Add_rcp rcp) })
              <|> try (do{ (reserved lis) "rm_ing"; ing <- identifier lis; n <- natural lis; return (Rm (ing, fromInteger n)) })
              <|> try (do{ (reserved lis) "rm_rcp"; rcp_name <- identifier lis; return (undefined) })
              <|> try (do{ (reserved lis) "check"; return CheckV })
              <|> try (do{ (reserved lis) "i_eat"; food_name <- identifier lis; return undefined })
              <|> try (do{ (reserved lis) "need_food"; return (WhatToEat (Nothing)) })
              <|> try (do{ (reserved lis) "what_with"; xs <- many1 (sepBy1 (identifier lis) (string ";")); return (WhatCanDoWith (foldr (++) [] xs)) })
              <|> try (do{ (reserved lis) "help"; return RMHelp})

parseComm :: Parser Comm
parseComm =       try (do{ (reserved lis) "save"; return Save })
              <|> try (do{ (reserved lis) "load"; name <- identifier lis ; undefined })
              <|> try (do{ (reserved lis) "close"; return Close })
              <|> try (do{ (reserved lis) "help"; return Help })
              <|> try (do{ (reserved lis) "display"; return Display })
              <|> try (do{ (reserved lis) "new_inv"; inv_name <- identifier lis; return (NewInv inv_name) })
               



{-
"Queso, 2 kl, 01-01-2018 "

IdQueso = IdIngr ("Queso", Nothing)

IngrQueso = Ingr (IdQueso, "01-01-2018", 2)
-}

  
----------------------------------
-- Funciones auxiliares
----------------------------------

nsymbol :: String -> Parser String
nsymbol = symbol lis

nnatural :: Parser Integer
nnatural = natural lis

nidentifier :: Parser String
nidentifier = identifier lis

nparens :: Parser a -> Parser a
nparens = parens lis

nreserved :: String -> Parser ()
nreserved = reserved lis

nsemi :: Parser String
nsemi = semi lis

nreservedOp :: String -> Parser ()
nreservedOp = reservedOp lis

parseOp :: String -> (a -> a -> a) -> Parser (a -> a -> a)
parseOp s op = nreservedOp s >> return op
