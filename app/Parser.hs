module Parser where

import Pretty
import Types
import Data.Dates


import Text.ParserCombinators.Parsec 
import Text.Parsec.Token
import Text.Parsec.Language (emptyDef)
import Text.PrettyPrint.HughesPJ (render)




-- Tokens
lis :: TokenParser u
lis = makeTokenParser (emptyDef   { commentStart    = "/*"
                                  , commentEnd      = "*/"
                                  , commentLine     = "//"
                                  , opLetter        = char '='
                                  , reservedOpNames = ["<",">","{","}"]
                                  , reservedNames   = [ "add_ing", "add_rcp", "rm_ing", "rm_rcp",
                                                        "check", "i_eat", "need_food", 
                                                        "new_inv", "save", "load", "close", "help", "display", "quit",
                                                        "add_t", "rm_t"]
                                  })

--Saved files
parserEnv :: Parser Env
parserEnv = do name <- identifier lis
               string "|Ingredientes:|"
               ingrs <- sepEndBy (parserIng) (string "#")
               string "|Recetas:|"
               rcps  <- sepEndBy (parserRcp) (string "%")
               string "|Tabla:|"
               t     <- sepEndBy (parserIngValues) (string "@")
               string "|Log:|"
               l     <- sepEndBy (parserEntry) (string "|")
               return (Env name ingrs rcps t l 1)


parserEntry :: Parser Entry
parserEntry = do e <- parserExpireDate
                 symbol lis "-" 
                 w  <- parserGrams
                 return (Entry e w)


parserAddedIng :: Parser Ingr
parserAddedIng = do n  <- identifier lis
                    symbol lis "-" 
                    w  <- parserGrams
                    e  <- parserExpireDate                    
                    return (Ingr n {-(Just nv)-} w (Just e) )



--Table
parserTable :: Parser [IngValues]
parserTable = do identifier lis
                 string "|Ingredientes:|"
                 sepEndBy (parserAddedIng) (string "#")
                 string "|Recetas:|"
                 sepEndBy (parserRcp) (string "%")
                 string "|Tabla:|"
                 t     <- sepEndBy (parserIngValues) (string "@")
                 string "|Log:|"
                 sepEndBy (parserEntry) (string "|")
                 return t



-- Ingredients
parserIng :: Parser Ingr
parserIng = do n <- identifier lis
               symbol lis "-" 
               w <- parserGrams
               e <- parserExpireDate
               return (Ingr n {-Nothing-} w (Just e) )

parserIngValues :: Parser IngValues
parserIngValues = do n <- identifier lis
                     string "-" 
                     p <- parserGrams
                     string "-"
                     v <- parserNV
                     return (IV n p v )

-- Nutritional Values
parserNV :: Parser NutritionalValues
parserNV = do spaces
              c <- parserGrams
              spaces
              p <- parserGrams
              spaces
              f <- parserGrams
              return (NV c p f) 
 

--Dates
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


--Recipes
parserRcp :: Parser Recipe
parserRcp = do name <- identifier lis
               spaces
               string "-i"
               spaces
               ingrs <- manyTill (sepBy1 parserIngRcp (string ";")) (parserEnd "-p")
               spaces
               steps <- manyTill parserStep (parserEnd "-t")
               spaces
               ts    <- optionMaybe (manyTill parserTag (parserEnd "-f"))
               return (Rcp name (foldr (++) [] ingrs) (steps) ts)  


parserEnd :: String -> Parser String
parserEnd end = do {spaces ; string end}


parserStep :: Parser Step
parserStep = do {spaces ; manyTill anyChar (try (string ";"))} 


parserTag :: Parser Tag
parserTag = parserStep


parserIngRcp :: Parser Ingr
parserIngRcp = do spaces
                  n <- identifier lis
                  symbol lis "-" 
                  w <- parserGrams
                  return (Ingr n {-Nothing-} w Nothing)

parserGrams :: Parser Grams
parserGrams = try (float lis) <|> (do {x <- (natural lis); return (fromIntegral x)})

--Conditions

parserCond :: Parser Cond
parserCond =     try (do t <- identifier lis; return (With t)) 
             <|> (do {string "<";spaces; g <- parserGrams; try (do string "carb"; return (LessThan (Carb g)))
                                                               <|> (do string "prot"; return (LessThan (Prot g)))
                                                               <|> (do string "grasas"; return (LessThan (Fats g)))
                                                               <|> (do string "cal"; return (LessThanC g))})
             <|> (do {string ">";spaces; g <- parserGrams; try (do string "carb"; return (MoreThan (Carb g)))
                                                           <|> (do string "prot"; return (MoreThan (Prot g)))
                                                           <|> (do string "grasas"; return (MoreThan (Fats g)))
                                                           <|> (do string "cal"; return (MoreThanC g))}) 


parserConds :: Parser [Cond]
parserConds = try (do xs <- wantedConds
                      ys <- optionMaybe unwantedConds
                      return (maybe xs (\c -> (xs ++ c)) ys)
              <|> unwantedConds)



wantedConds :: Parser [Cond]
wantedConds = do string "+"
                 spaces
                 reservedOp lis "{"
                 xs <- manyTill (sepBy1 parserCond (string ", ")) (reservedOp lis "}")
                 return (foldr (++) [] xs)


unwantedConds :: Parser [Cond]
unwantedConds = do spaces
                   string "-"
                   spaces
                   reservedOp lis "{"
                   ys <- manyTill (sepBy1 ((do t <- identifier lis; return (Without t))) (string ",")) (reservedOp lis "}")
                   return (foldr (++) [] ys)


--Commands
parseRMComm :: Parser RMComm
parseRMComm =     try ( do{ (reserved lis) "add_ing"; ing <- parserIng; return (Add_ing ing) })
              <|> (do{ (reserved lis) "add_rcp";  rcp <- parserRcp; return (Add_rcp rcp) })
              <|> (do{ (reserved lis) "rm_ing"; ing <- identifier lis;string "-"; n <- parserGrams; return (Rm (ing, n)) })
              <|> (do{ (reserved lis) "rm_rcp"; rcp_name <- identifier lis; return (Rm_rcp rcp_name) })
              <|> (do{ (reserved lis) "check";eof; return CheckV })
              <|> (do{ (reserved lis) "i_eat"; rcp_name <- identifier lis; return (IEat rcp_name)})
              <|> (do{ (reserved lis) "need_food"; c <- optionMaybe (parserConds);eof; return (WTE c) })
              <|> (do{ (reserved lis) "help"; return RMHelp})
              <|> (do{ (reserved lis) "save"; return RMSave})
              <|> (do{ (reserved lis) "close"; return RMClose})
              <|> (do{ (reserved lis) "display"; return Display })
              <|> (do{ (reserved lis) "add_t"; spaces; iv <- parserIngValues; return (AddTable iv)})
              <|> (do{ (reserved lis) "rm_t"; n <- identifier lis;eof; return (RmTable n)})
              <|> (do{ (reserved lis) "import_table"; file <- identifier lis; string ".rcpm"; return (ImportTable (file ++ ".rcpm"))}
              <|> (do{ (reserved lis) "display_t";eof; return ShowT}))
parseComm :: Parser Comm
parseComm =       try (do{ (reserved lis) "load"; file <- identifier lis; string ".rcpm"; return (Load (file ++ ".rcpm")) })
              <|> (do{ (reserved lis) "quit"; return Quit })
              <|> (do{ (reserved lis) "help"; return Help })
              <|> (do{ (reserved lis) "new_inv"; inv_name <- identifier lis; return (NewInv inv_name) })
   
