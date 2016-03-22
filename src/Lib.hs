{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}

module Lib where

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Expr
import Text.Parsec.String
import Text.ParserCombinators.Parsec.Token

someFunc :: IO ()
someFunc = putStrLn "someFunc"
  
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

bitWiseComplement :: Parser String
bitWiseComplement = string "~"

bitWiseAnd :: Parser String
bitWiseAnd = string "&"

bitWiseOr :: Parser String
bitWiseOr = string "|"

addition :: Parser String
addition = string "+"

subtraction :: Parser String
subtraction = string "-"

multiplication :: Parser String
multiplication = string "*"

division :: Parser String
division = string "/"

modulo :: Parser String
modulo = string "%"

rightShift :: Parser String
rightShift = string ">>"

leftShift :: Parser String
leftShift = string "<<"

data Binop = Binop String deriving (Show, Eq)

binop :: Parser Binop
binop = do op <- choice [ bitWiseComplement
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
           return $ Binop op

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
            deriving (Show, Eq)

numExpr :: Parser Expr
numExpr = do n <- try litNum
             return $ NumExpr n

charExpr :: Parser Expr
charExpr = do c <- try litChar
              return $ CharExpr c
             
identExpr :: Parser Expr
identExpr = do c <- try ident
               return $ IdentExpr c

term = try numExpr <|> try identExpr <|> try charExpr

binExpr = do t <- term
             restExpr t <|> emptyExpr t
             
restExpr t = do b <- binop
                s <- binExpr <|> term
                return $ BinExpr b t s

emptyExpr t = return t                

expr = binExpr 
