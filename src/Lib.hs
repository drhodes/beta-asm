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

debug = True
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
  choice [ try ident1
         , try ident3]


ruleSepComma rule = do
  dbg "ruleSepComma'"
  spacex
  xs <- sepBy rule (do spacex
                       char ','
                       spacex)
  spacex
  return xs


identSepComma = ruleSepComma ident

parened rule = do spacex
                  string "("
                  spacex
                  x <- rule
                  spacex
                  string ")"
                  spacex
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
                     
expr2 = do
  dbg "expr2"
  choice [ try expr21
         , try expr22 
         , try expr23
         ]

expr21 = do
  dbg "expr21"
  spacex
  string "-"
  spacex
  x <- expr2
  spacex
  return $ ExprNeg x

expr22 = do spacex
            t <- term
            spacex
            x <- expr1
            spacex
            return $ ExprTermExpr t x

expr23 = do spacex
            string "("
            spacex
            t <- term
            spacex
            x <- expr1
            spacex
            string ")"
            spacex
            return $ ExprTermExpr t x

op :: String -> Binop -> Parser Binop
op s c = do spacex
            string s
            spacex
            return c

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
                  spacex
                  return (ExprBinTail b t)

exprSepComma :: Parser [Expr]
exprSepComma = ruleSepComma expr2

exprList :: Parser [Expr]
exprList = do
  dbg "exprList"
  spacex
  string "("
  spacex
  xs <- sepBy expr2 (do spacex
                        char ','
                        spacex)
  spacex
  string ")"
  spacex
  return xs
                  
term = do
  dbg "term"
  choice [ try term1
         , try term2
         , try term3
         , try term4
         ]
term1 = dbg "term1" >> ident >>= return . TermIdent
term2 = dbg "term2" >> litNum >>= return . TermLitNum
term3 = do
  dbg "term3"
  spacex
  string "-"
  spacex
  t <- term
  spacex
  return $ TermNeg t
  
term4 = do
  dbg "term4"
  spacex
  e <- parened expr2
  spacex
  return $ TermExpr e

data Proc = DotInclude String
          | DotAlign Expr
          | DotAscii String
          | DotText String
          | DotBreakPoint
          | DotProtect
          | DotUnprotect
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
                 , try $ do string ".ascii"
                            qs <- quotedString
                            return $ DotAscii qs
                 , try $ do string ".text"
                            qs <- quotedString
                            return $ DotText qs
                 , try $ string ".breakpoint" >> return DotBreakPoint
                 , try $ string ".protect" >> return DotProtect
                 , try $ string ".unprotect" >> return DotUnprotect
                   -- still need to do OPTIONS
                   -- still need to do label.
                 ]

------------------------------------------------------------------
-- this code parsing quoted string shameless swiped from stackoverflow
-- post: https://stackoverflow.com/questions/24106314

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

data Stmt = StmtProc Proc
          | StmtCall Call
          | StmtAssn Assn
          | StmtExpr Expr
            deriving (Show, Eq)

stmt :: Parser Stmt
stmt = choice [ try stmt1 
              , try stmt2
              , try stmt3 
              , try stmt4
              ]

stmt1 = proc >>= return . StmtProc
stmt2 = call >>= return . StmtCall
stmt3 = assn >>= return . StmtAssn
stmt4 = expr2 >>= return . StmtExpr


------------------------------------------------------------------
data Assn = Assn Ident Expr deriving (Show, Eq)

assn :: Parser Assn
assn = do
  dbg "assn"
  lhs <- ident
  spacex
  string "="
  spacex
  x <- expr2
  return $ Assn lhs x

------------------------------------------------------------------
data Call = Call Ident [Expr] deriving (Show, Eq)

call :: Parser Call
call = do name <- ident
          spacex
          list <- exprList
          return $ Call name list

data Macro = Macro Ident [Ident] [Stmt] deriving (Show, Eq)

macro = do
  dbg "macro"
  string ".macro"
  spacex
  name <- ident
  spacex
  args <- identList
  spacex
  body <- macroBody
  return $ Macro name args body
          
macroBody = do
  dbg "macroBody"
  spacex
  choice [ try macroBody2
         , try macroBody1
         ]

macroBody1 :: Parser [Stmt]  
macroBody1 = do
  dbg "macroBody"
  spacex
  stmts <- many (do spacex
                    s <- stmt
                    spacex
                    return s)
  spacex
  endOfLine
  return stmts
  
macroBody2 = do
  dbg "macroBody"
  spaces
  string "{"
  spaces
  stmts <- many (do s <- stmt
                    spaces
                    return s)
  spaces
  string "}"
  return stmts

------------------------------------------------------------------
data TopLevel = TopStmt Stmt
              | TopMacro Macro
                deriving (Show, Eq)

topLevel = try topLevel2 <|> try topLevel1

topLevel1 = do spaces
               s <- stmt
               spaces
               return (TopStmt s)
               
topLevel2 = do spaces
               m <- macro
               spaces
               return (TopMacro m)

sourceFile = many topLevel
