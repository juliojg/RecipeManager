module Parser where

import Text.ParserCombinators.Parsec 
import Text.Parsec.Token
import Text.Parsec.Language (emptyDef)

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

-- Parser de ingredientes
parserIng :: Parser Ingr
parserIng = do n <- identifier lis
               symbol lis "," 
               w <- natural lis -- (try?)
               symbol lis ","
               v <- parseV     --(try?)
               --parseDatos
               return (makeIngr n (fromInteger w) v)


makeIngr :: String -> Cantidad -> Maybe Vencimiento -> Ingr
makeIngr n c v = Ingr { id_ingr = IdIngr { ing_name = n, datos = Nothing},
                               ven     = v,
                               cant    = c
                             } 
--Parser de fechas
parseV :: Parser (Maybe Vencimiento)
parseV = optionMaybe ( do d <- natural lis --ver que pasa si consume algo de la entrada
                          symbol lis "-"
                          m <- natural lis
                          symbol lis "-"
                          y <- natural lis
                          return (DateTime (fromIntegral y) 
                                           (fromIntegral m) 
                                           (fromIntegral d) 0 0 0)) 

--Parser de recetas
--parserPaso :: Parser Paso
--parserPaso = do x <- 




--Parser comandos
parseRMComm :: Parser RMComm
parseRMComm =     try (do{ (reserved lis) "add_ingr"; ing <- parserIng; return (Add_ing ing) })
              <|> try (do{ (reserved lis) "add_rcp"; undefined })
              <|> try (do{ (reserved lis) "rm_ing"; undefined })
              <|> try (do{ (reserved lis) "rm_rcp"; undefined })
              <|> try (do{ (reserved lis) "check"; return CheckV })
              <|> try (do{ (reserved lis) "i_eat"; undefined })
              <|> try (do{ (reserved lis) "need_food"; undefined })
              <|> try (do{ (reserved lis) "new_inv"; undefined })



parseComm :: Parser Comm
parseComm =       try (do{ (reserved lis) "save"; return Save })
              <|> try (do{ (reserved lis) "load"; undefined })
              <|> try (do{ (reserved lis) "close"; return Close })
              <|> try (do{ (reserved lis) "help"; return Help })
              <|> try (do{ (reserved lis) "display"; return Display })

               



{-
"Queso, 2 kl, 01-01-2018 "

IdQueso = IdIngr ("Queso", Nothing)

IngrQueso = Ingr (IdQueso, "01-01-2018", 2)
-}


{- data RMComm = Add_ing Ingr 
            | Add_rcp Receta
            | Rm (Ingr, Cantidad) -- ver como diferenciar ingredientes con vencimientos distintos             
            | Rm_rcp Comida
            | CheckV
            | IEat Comida --remove used ingredients
            | WhatToEat (Maybe Cond)
            | WhatCanDoWith [Ingr]
            | NewInv String
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
{-
-----------------------------------
-- Parser de expressiones enteras
-----------------------------------

intexp :: Parser IntExp
intexp  = chainl1 intops addop

intops :: Parser IntExp
intops = chainl1 iatom mulop

iatom :: Parser IntExp
iatom =     try (do{ x <- nnatural ; return $ Const x })
        <|> try (do{ v <- nidentifier ; return $ Var v })
        <|> try (do{ nreservedOp "-" ; i <- intexp ; return $ UMinus i })
        <|> try (nparens intexp)

addop, mulop :: Parser (IntExp -> IntExp -> IntExp)
addop =   (parseOp "+" Plus)  <|> (parseOp "-" Minus)
mulop =   (parseOp "*" Times) <|> (parseOp "/" Div)

-----------------------------------
-- Parser de expressiones booleanas
------------------------------------

boolexp :: Parser BoolExp
boolexp = chainl1 andexp bitor
 
andexp :: Parser BoolExp
andexp = chainl1 batom bitand 

batom :: Parser BoolExp
batom =     try (do{ nreserved "true" ; return BTrue })
        <|> try (do{ nreserved "false" ; return BFalse })
        <|> try (do{ nreservedOp "~" ; b <- boolexp ; 
                     return (Not b) })
        <|> try (nparens boolexp)
        <|> try (do{ i1 <- intexp ; nreservedOp "=" ;
                     i2 <- intexp ; return (Eq i1 i2) })
        <|> try (do{ i1 <- intexp ; nreservedOp "<" ;
                     i2 <- intexp ; return (Lt i1 i2) })
        <|> try (do{ i1 <- intexp ; nreservedOp ">" ;
                     i2 <- intexp ; return (Gt i1 i2) })

bitor, bitand :: Parser (BoolExp -> BoolExp -> BoolExp)
bitor = parseOp "|" Or
bitand = parseOp "&" And

-----------------------------------
-- Parser de comandos
-----------------------------------

comm :: Parser Comm
comm = chainl1 catom seqop

catom :: Parser Comm
catom =   do{ nreserved "skip"; return Skip }
      <|> try (do{ v <- nidentifier ; nreservedOp ":=" ;
                   i <- intexp ; return (Let v i) })
      <|> do{ nreserved "if"; b <- boolexp ;
              nreserved "then" ; c1 <- comm ;
              nreserved "else" ; c2 <- comm ;
              nreserved "end" ; return (Cond b c1 c2) }
      <|> do{ nreserved "repeat"; c <- comm ;
              nreserved "until" ; b <- boolexp ;
              return (Repeat c b) }

seqop :: Parser (Comm -> Comm -> Comm)
seqop = parseOp ";" Seq
------------------------------------
-- Función de parseo
------------------------------------
parseComm :: SourceName -> String -> Either ParseError Comm
parseComm = parse (totParser comm) 
-}
