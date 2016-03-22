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
  
lineComment :: Parser String
lineComment = do spaces
                 string "//"
                 comment <- many $ noneOf "\n"
                 newline
                 return comment
                 
multilineComment :: Parser String
multilineComment = do spaces
                      string "/*"
                      comment <- many $ noneOf "*/"
                      string "*/"
                      return comment

keywordMacro :: Parser String
keywordMacro = do string ".macro"

------------------------------------------------------------------
data Ident = Ident String deriving (Show, Eq)

ident :: Parser Ident
ident = do part1 <- upper <|> lower
           part2 <- many $ alphaNum <|> oneOf "_."
           return $ Ident (part1 : part2)

------------------------------------------------------------------
data ArgList = ArgList [Ident] deriving (Show, Eq)

argList :: Parser ArgList
argList = do char '('             
             let spacedIdent = do {
                   ; spaces
                   ; x <- ident
                   ; spaces
                   ; return x
                   }
                                  
             args <- sepBy1 spacedIdent (char ',')
             char ')'
             return $ ArgList args

------------------------------------------------------------------
data Label = Label Ident deriving (Show, Eq)

label :: Parser Label
label = do x <- ident
           spaces
           char ':'
           return $ Label x

------------------------------------------------------------------
data Include = Include String deriving (Show, Eq)

include :: Parser Include
include = do string ".include"
             spaces
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
            deriving (Show, Eq)

numExpr = do n <- litNum
             return $ NumExpr n

identExpr = do n <- ident
               return $ IdentExpr n

charExpr = do c <- litChar
              return $ CharExpr c
------------------------------------------------------------------
data Macro = MacroLine Ident ArgList [Expr]
  
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
lexer = Token.makeTokenParser languageDef

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
term2 = do spaces
           t <- term1
           spaces
           return t

expr :: Parser Expr
expr = buildExpressionParser ops term2
           
------------------------------------------------------------------
