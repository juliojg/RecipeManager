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
                                                        "new_inv", "save", "load", "close", "help", "display", "quit",
                                                        "add_t", "rm_t", "add_tag"]
                                  })

--Parser de archivos guardados
parserEnv :: Parser Env
parserEnv = do name <- identifier lis
               string "|Ingredientes:|"
               ingrs <- sepEndBy (parserAddedIng) (string "#")
               string "|Recetas:|"
               rcps  <- sepEndBy (parserRcp) (string "%")
               string "|Tabla:|"
               t     <- sepEndBy (parserIngValues) (string "|")
               return (Env name ingrs rcps t [] 1)


parserAddedIng :: Parser Ingr
parserAddedIng = do n  <- identifier lis
                    symbol lis "-" 
                    w  <- parserGrams
                    symbol lis "-"            
                    nv <- parserNV
                    e  <- parserExpireDate                    
                    return (Ingr n (Just nv) w (Just e) )


-- Parser de ingredientes
parserIng :: Parser Ingr
parserIng = do n <- identifier lis
               symbol lis "-" 
               w <- parserGrams
               e <- parserExpireDate
               return (Ingr n Nothing w (Just e) )

parserIngValues :: Parser IngValues
parserIngValues = do n <- identifier lis
                     string "-" 
                     p <- parserGrams
                     string "-"
                     v <- parserNV
                     return (IV n p v )

parserNV :: Parser NutritionalValues -- carb prot fats, en ese orden
parserNV = do c <- parserGrams
              spaces
              p <- parserGrams
              spaces
              f <- parserGrams
              return (NV c p f) 
 




--Parser de fechas
parserExpireDate :: Parser ExpireDate
parserExpireDate = do symbol lis "-"
                      d <- natural lis 
                      symbol lis "/"
                      m <- natural lis
                      symbol lis "/"
                      y <- natural lis
                      return (DateTime (fromIntegral y) 
                                       (fromIntegral m) 
                                       (fromIntegral d) 0 0 0) 


--Parser de recetas
parserRcp :: Parser Recipe
parserRcp = do name <- identifier lis
               string "..."
               spaces
               ingrs <- manyTill (sepBy1 parserIngRcp (string ";")) (parserEnd "...")
               spaces
               steps <- manyTill parserStep (string ":f")
               spaces
               ts    <- optionMaybe (manyTill parserTag (string ":ff"))
               return (Rcp name (foldr (++) [] ingrs) (steps) ts)  


parserEnd :: String -> Parser String
parserEnd end = do {spaces ; string end}


parserStep :: Parser Step
parserStep = do {spaces ; manyTill anyChar (try (string ";"))} 


parserTag :: Parser Tag
parserTag = do {spaces ; manyTill anyChar (try (string ";"))} 


parserIngRcp :: Parser Ingr
parserIngRcp = do spaces
                  n <- identifier lis
                  symbol lis "-" 
                  w <- parserGrams
                  return (Ingr n Nothing w Nothing)

parserGrams :: Parser Grams
parserGrams = try (float lis) <|> (do {x <- (natural lis); return (fromIntegral x)})


--Parser comandos
parseRMComm :: Parser RMComm
parseRMComm =     try ( do{ (reserved lis) "add_ing"; ing <- parserIng; return (Add_ing ing) })
              <|> (do{ (reserved lis) "add_rcp";  rcp <- parserRcp; return (Add_rcp rcp) })
              <|> (do{ (reserved lis) "rm_ing"; ing <- identifier lis; n <- parserGrams; return (Rm (ing, n)) })
              <|> (do{ (reserved lis) "rm_rcp"; rcp_name <- identifier lis; return (undefined) })
              <|> (do{ (reserved lis) "check"; return CheckV })
              <|> (do{ (reserved lis) "i_eat"; food_name <- identifier lis; return undefined })
              <|> (do{ (reserved lis) "need_food"; return (WTE (Nothing)) })
              <|> (do{ (reserved lis) "what_with"; xs <- many1 (sepBy1 (identifier lis) (string ";")); return (WCDW (foldr (++) [] xs)) })
              <|> (do{ (reserved lis) "help"; return RMHelp})
              <|> (do{ (reserved lis) "save"; return RMSave})
              <|> (do{ (reserved lis) "close"; return RMClose})
              <|> (do{ (reserved lis) "display"; return Display })
              <|> (do{ (reserved lis) "add_t"; spaces; iv <- parserIngValues; return (AddTable iv)})
              <|> (do{ (reserved lis) "rm_t"; n <- identifier lis; return (RmTable n)})
              <|> (do{ (reserved lis) "add_tag"; n <- identifier lis; return (AddTag n)})

parseComm :: Parser Comm
parseComm =       try (do{ (reserved lis) "load"; name <- identifier lis; string ".rcpm"; return (Load (name ++ ".rcpm")) })
              <|> (do{ (reserved lis) "quit"; return Quit })
              <|> (do{ (reserved lis) "help"; return Help })
              <|> (do{ (reserved lis) "new_inv"; inv_name <- identifier lis; return (NewInv inv_name) })
