{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}

module Lib where

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String
import Text.ParserCombinators.Parsec.Token
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import Data.Functor.Identity

spacex = do many (char ' ' <|> char '\t')
            return ()

data Comment = LineComment String
             | BlockComment String
               deriving (Show, Eq)

lineComment :: Parser Comment
lineComment = do spacex
                 string "//"
                 comment <- many $ noneOf "\n"
                 newline
                 return $ LineComment comment
                 
multilineComment :: Parser Comment
multilineComment = do spacex
                      string "/*"
                      comment <- many $ noneOf "*/"
                      string "*/"
                      return $ BlockComment comment

comment = many $ lineComment <|> multilineComment

keywordMacro :: Parser String
keywordMacro = do string ".macro"

------------------------------------------------------------------
data Ident = Ident String
           | CurInstruction
           | IdMacro
             deriving (Show, Eq)
                    
ident1 :: Parser Ident
ident1 = do part1 <- upper <|> lower
            part2 <- many $ alphaNum <|> oneOf "_"
            return $ Ident (part1 : part2)

ident2 = string ".macro" >> return IdMacro

            
ident3 :: Parser Ident
ident3 = do char '.'
            return $ CurInstruction


ident = ident1 <|> ident2 <|> ident3
------------------------------------------------------------------
data ArgList = ArgList [Ident] deriving (Show, Eq)

argList :: Parser ArgList
argList = do char '('             
             let spacedIdent = do {
                   ; spacex
                   ; x <- ident
                   ; spacex
                   ; return x
                   }
             args <- sepBy spacedIdent (char ',')
             char ')'
             return $ ArgList args

------------------------------------------------------------------
data Label = Label Ident deriving (Show, Eq)

label :: Parser Label
label = do spacex
           x <- ident
           spacex
           char ':'
           spaces
           return $ Label x

------------------------------------------------------------------
data Include = Include String deriving (Show, Eq)

include :: Parser Include
include = do string ".include"
             spacex
             char '"'
             filename <- many $ (alphaNum <|> oneOf "._" <?> "invalid file name")
             char '"'
             return $ Include filename

------------------------------------------------------------------
unaryMinus :: Parser Char
unaryMinus = char '-'

bitWiseComplement :: Parser Binop
bitWiseComplement = do string "~"
                       return BitwiseComplement
  
bitWiseAnd :: Parser Binop
bitWiseAnd = do string "&"
                return BitWiseAnd

bitWiseOr :: Parser Binop
bitWiseOr = do string "|"
               return BitWiseOr

addition :: Parser Binop
addition = do string "+"
              return Addition

subtraction :: Parser Binop
subtraction = do string "-"
                 return Subtraction

multiplication :: Parser Binop
multiplication = do string "*"
                    return Multiplication

division :: Parser Binop
division = do string "/"
              return Division

modulo :: Parser Binop
modulo = do string "%"
            return Modulo

rightShift :: Parser Binop
rightShift = do string ">>"
                return RightShift

leftShift :: Parser Binop
leftShift = do string "<<"
               return LeftShift

data Binop = BitwiseComplement
           | BitWiseAnd
           | BitWiseOr
           | Addition
           | Subtraction
           | Multiplication
           | Division
           | Modulo
           | RightShift
           | LeftShift
             deriving (Show, Eq)

binop :: Parser Binop
binop = do choice [ bitWiseComplement
                  , bitWiseAnd
                  , bitWiseOr
                  , addition
                  , subtraction
                  , multiplication
                  , division
                  , modulo
                  , rightShift
                  , leftShift
                  ]
                          


------------------------------------------------------------------
hexNum :: Parser Integer 
hexNum = do string "0x"
            h <- many1 hexDigit
            return (read ("0x" ++ h) :: Integer)

binNum :: Parser Integer 
binNum = do string "0b"
            b <- many1 $ char '0' <|> char '1'
            let x = sum [if n == '0' then 0 else 2^p |
                         (n, p) <- zip (reverse b) [0..]]
            return x

decNum :: Parser Integer        
decNum = do d <- many1 digit
            return (read d :: Integer)
            
------------------------------------------------------------------            
data LitChar = LitChar Char deriving (Show, Eq)

litChar :: Parser LitChar
litChar = do let quote = char '\''
             quote
             c <- noneOf "'"
             quote
             return $ LitChar c

------------------------------------------------------------------
data LitNum = LitNum Integer deriving (Show, Eq)

litNum :: Parser LitNum
litNum = do neg <- optionMaybe unaryMinus
            n <- choice [try binNum, try hexNum, try decNum]
            return $ LitNum $ case neg of
              Just _ -> -n
              Nothing -> n

------------------------------------------------------------------
data Expr = BinExpr Binop Expr Expr
          | NumExpr LitNum
          | IdentExpr Ident
          | CharExpr LitChar
          | Neg Expr
          | Call Ident [Expr]
            deriving (Show, Eq)

numExpr = return . NumExpr =<< litNum
identExpr = return . IdentExpr =<< ident
charExpr = return . CharExpr =<< litChar

------------------------------------------------------------------
  
languageDef =
  emptyDef { Token.commentStart    = "/*"
           , Token.commentEnd      = "*/"
           , Token.commentLine     = "//"
           , Token.identStart      = letter
           , Token.identLetter     = alphaNum
           , Token.reservedNames   = []
           , Token.reservedOpNames = [ "+", "-", "*", "/"
                                     , "~", "&", "|", "+"
                                     , "-", "*", "/", "%"
                                     , ">>", "<<" 
                                     ]
           }


lexer :: GenTokenParser String u Data.Functor.Identity.Identity
lexer = (Token.makeTokenParser languageDef){whiteSpace = spacex}

resOp = Token.reservedOp lexer

ops = [ [ Prefix (resOp "-" >> return (Neg))]
      , [ Infix (resOp "*" >> return (BinExpr Multiplication)) AssocLeft
        , Infix (resOp "/" >> return (BinExpr Division )) AssocLeft
        , Infix (resOp "+" >> return (BinExpr Addition )) AssocLeft
        , Infix (resOp "-" >> return (BinExpr Subtraction )) AssocLeft
        , Infix (resOp "~" >> return (BinExpr BitwiseComplement)) AssocLeft
        , Infix (resOp "&" >> return (BinExpr BitWiseAnd)) AssocLeft
        , Infix (resOp "|" >> return (BinExpr BitWiseOr)) AssocLeft
        , Infix (resOp "+" >> return (BinExpr Addition)) AssocLeft
        , Infix (resOp "-" >> return (BinExpr Subtraction)) AssocLeft
        , Infix (resOp "*" >> return (BinExpr Multiplication)) AssocLeft
        , Infix (resOp "/" >> return (BinExpr Division)) AssocLeft
        , Infix (resOp "%" >> return (BinExpr Modulo)) AssocLeft
        , Infix (resOp ">>" >> return (BinExpr RightShift)) AssocLeft
        , Infix (resOp "<<" >> return (BinExpr LeftShift)) AssocLeft
        ]]

term1 = parens lexer expr <|> numExpr <|> identExpr <|> charExpr
term2 = do spacex
           t <- term1
           spacex
           return t

expr :: Parser Expr
expr = buildExpressionParser ops term2

------------------------------------------------------------------
data Macro = MacroLine Ident ArgList [Expr]
           | MacroBlock Ident ArgList [Stmt]
           deriving (Show, Eq)

macroLine = do string ".macro"
               spacex
               macroName <- ident
               spacex
               args <- argList
               spacex
               es <- (many1 expr) 
               return $ MacroLine macroName args es


macroBlock = do string ".macro"
                spacex
                name <- ident
                spacex
                args <- argList
                spacex
                char '{'
                ss <- many (do spaces 
                               s <- stmt
                               spaces
                               return s )
                spaces
                char '}'
                return $ MacroBlock name args ss

macro = do try macroLine <|> macroBlock                  

------------------------------------------------------------------
data Stmt = AssignStmt Ident Expr
          | ExprStmt Expr
          | IdentStmt Ident
          deriving (Show, Eq)
                             
assn :: Parser Stmt
assn = do name <- ident
          spacex
          string "="
          spacex
          e <- expr          
          return $ AssignStmt name e

stmt :: Parser Stmt
stmt = try assn <|> try callStmt <|> identStmt

callStmt = callExpr >>= (return . ExprStmt)
identStmt = ident >>= (return . IdentStmt)

callExpr :: Parser Expr
callExpr = do
  name <- ident
  spacex
  char '('             
  let spacedExpr = do {
        ; spacex
        ; x <- expr
        ; spacex
        ; return x
        }
  args <- sepBy spacedExpr (char ',')
  char ')'
  return $ Call name args

------------------------------------------------------------------
data TopLevel = TopStmt Stmt
              | TopMacro Macro
              | TopLabel Label
                deriving (Show, Eq)

topLevel1 = return . TopStmt  =<< stmt
topLevel2 = return . TopMacro =<< macro
topLevel3 = return . TopLabel =<< Lib.label
topLevelN = try topLevel3 <|> try topLevel1 <|> try topLevel2

topLevels =
  do spaces
     top <- topLevelN
     spaces
     return top

topLevel = many topLevels


