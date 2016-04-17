{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}


module Lib where

import           Data.Functor.Identity
import           Debug.Trace
import           Text.Parsec
import           Text.Parsec.Char
import           Text.Parsec.String
import           Text.Parsec.Expr
import           Text.ParserCombinators.Parsec.Char
import qualified Text.ParserCombinators.Parsec.Expr as E
import           Text.ParserCombinators.Parsec.Language
import           Text.ParserCombinators.Parsec.Token
import qualified Text.ParserCombinators.Parsec.Token as Token

spacex = do many (char ' ' <|> char '\t')
            return ()

keywordMacro :: Parser String
keywordMacro = do string ".macro"

debug = False
dbg s = if debug
        then traceShowM s
        else return ()

------------------------------------------------------------------
data Ident = Ident String
           | CurInstruction
             deriving (Show, Eq)
                    
ident1 :: Parser Ident
ident1 = do
  dbg "ident1"
  part1 <- upper <|> lower <|> char '_'
  part2 <- many $ alphaNum <|> oneOf "_"
  return $ Ident (part1 : part2)

ident3 = do
  dbg "ident3"
  s <- char '.'
  dbg s
  return CurInstruction

ident = do
  dbg "ident"
  ident1 <|> ident3

identSepComma :: Parser [Ident]
identSepComma = do
  dbg "identSepComma"
  firstEl <- ident
  restEls <- many $ do spacex
                       string ","
                       spacex
                       x <- ident
                       return x
  return $ firstEl:restEls

parened rule = do string "("
                  spacex
                  x <- rule
                  spacex
                  string ")"
                  return x


identList = parened identSepComma

------------------------------------------------------------------
hexNum = do
  dbg "hexNum"
  string "0x"
  h <- many1 hexDigit
  return (read ("0x" ++ h) :: Integer)

binNum :: Parser Integer 
binNum = do
  dbg "binNum"
  string "0b"
  b <- many1 $ char '0' <|> char '1'
  let x = sum [if n == '0' then 0 else 2^p |
               (n, p) <- zip (reverse b) [0..]]
  return x

decNum :: Parser Integer        
decNum = do
  dbg "decNum"
  d <- many1 digit
  return (read d :: Integer)

------------------------------------------------------------------
data LitNum = LitNum Integer deriving (Show, Eq)

litNum :: Parser LitNum
litNum = do
  dbg "litNum"
  n <- choice [try binNum, try hexNum, try decNum]
  return $ LitNum n

data Binop = BitwiseComplement
           | BitWiseAnd
           | BitWiseOr
           | Addition
           | Subtract
           | Multiply
           | Division
           | Modulo
           | RightShift
           | LeftShift
             deriving (Show, Eq)

data Expr = ExprNeg Expr
          | ExprTerm Term
          | ExprTermExpr Term [Expr]
          | ExprBinTail Binop Term
            deriving (Show, Eq)
                     
data Term = TermIdent Ident
          | TermLitNum LitNum
          | TermNeg Term
          | TermExpr Expr
            deriving (Show, Eq)
                     
expr2 =
  try expr21 <|>
  try expr22 <|>
  try expr23

expr21 = do string "-"
            spacex
            x <- expr2
            return $ ExprNeg x

expr22 = do spacex
            t <- term
            spacex
            x <- expr1
            return $ ExprTermExpr t x

expr23 = do string "("
            spacex
            t <- term
            spacex
            x <- expr1
            spacex
            string ")"
            return $ ExprTermExpr t x

op :: String -> Binop -> Parser Binop
op s c = string s >> return c

binop = choice [ try (op "*" Multiply)
               , try (op "-" Subtract)
               , try (op "+" Addition)
               , try (op "/" Division)
               , try (op ">>" RightShift)
               , try (op "<<" LeftShift)
               , try (op "%" Modulo)
               ]

expr1 = many $ do spacex
                  b <- binop
                  spacex
                  t <- term
                  return (ExprBinTail b t)
            
term = try term1 <|> try term2 <|> try term3
term1 = ident >>= return . TermIdent
term2 = litNum >>= return . TermLitNum
term3 = do e <- parened expr2
           return $ TermExpr e


data Proc = DotInclude String
          | DotAlign Expr
          | DotAscii String
          | DotText String
          | DotBreakPoint
          | DotProtect
          | DotUnProtect
          | DotOptions
          | Label String
            deriving (Show, Eq)
------------------------------------------------------------------

proc = do choice [ try $ do string ".include"
                            qs <- quotedString
                            return $ DotInclude qs
                 , try $ do string ".align"
                            x <- expr2
                            return $ DotAlign x
                 ]
            
------------------------------------------------------------------
-- this quote parse shameless swiped from stackoverflow post:
-- https://stackoverflow.com/questions/24106314/parser-for-quoted-string-using-parsec

escape :: Parser String
escape = do
  d <- char '\\'
  c <- oneOf "\\\"0nrvtbf" -- all the characters which can be escaped
  return [d, c]

nonEscape :: Parser Char
nonEscape = noneOf "\\\"\0\n\r\v\t\b\f"

character :: Parser String
character = fmap return nonEscape <|> escape

quotedString :: Parser String
quotedString = do
    char '"'
    strings <- many character
    char '"'
    return $ concat strings
------------------------------------------------------------------
