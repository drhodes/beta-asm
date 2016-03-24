{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}


module Lib where

import           Data.Functor.Identity
import           Debug.Trace
import           Text.Parsec
import           Text.Parsec.Char
import           Text.Parsec.String
import           Text.ParserCombinators.Parsec.Char
import           Text.ParserCombinators.Parsec.Expr
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
  part1 <- upper <|> lower
  part2 <- many $ alphaNum <|> oneOf "_"
  return $ Ident (part1 : part2)

ident3 = do
  dbg "ident3"
  s <- char '.'
  dbg s
  return CurInstruction

ident = do
  dbg "ident"
  choice [ try ident3
         , try ident1]

------------------------------------------------------------------
data ArgList = ArgList [Ident] deriving (Show, Eq)

argListEmpty :: Parser ArgList
argListEmpty = do
  dbg "argListEmpty"
  char '('
  spacex
  char ')'
  return $ ArgList []

argListSome :: Parser ArgList
argListSome = do
  dbg "argListSome"
  char '('             
  let spacedIdent = do {
        ; spacex
        ; x <- ident
        ; spacex
        ; return x
        }
  args <- sepBy spacedIdent (char ',')
  char ')'
  return $ ArgList args

argList = argListSome <|> argListEmpty


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
unaryMinus :: Parser Char
unaryMinus = char '-'

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

opform op cons = do
  dbg $ ": (" ++ op ++ ")"
  string op
  return cons

                      
binop :: Parser Binop
binop = choice [ opform "~" BitwiseComplement
               , opform "&" BitWiseAnd
               , opform "|" BitWiseOr
               , opform "+" Addition
               , opform  "-" Subtraction
               , opform "*" Multiplication
               , opform "/" Division
               , opform "%" Modulo
               , opform ">>" RightShift
               , opform "<<" LeftShift
               ]


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
data LitChar = LitChar Char deriving (Show, Eq)

litChar = do
  dbg "litChar"
  let quote = char '\''
  quote
  c <- noneOf "'"
  quote
  return $ LitChar c

------------------------------------------------------------------
data LitNum = LitNum Integer deriving (Show, Eq)

litNum :: Parser LitNum
litNum = do
  dbg "litNum"
  neg <- optionMaybe unaryMinus
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
          | CurInst
            deriving (Show, Eq)

curInstExpr :: Parser Expr
curInstExpr = do
  dbg "curInstExpr"
  n <- try $ string "."
  return $ CurInst

numExpr = do
  dbg "numExpr"
  n <- try litNum
  return $ NumExpr n
  
identExpr = do
  dbg "identExpr"
  n <- try ident
  return $ IdentExpr n
  
charExpr = do
  dbg "charExpr"
  c <- try litChar
  return $ CharExpr c

------------------------------------------------------------------
{-


 expr    = buildExpressionParser table term
         <?> "expression"

 term    =  parens expr
         <|> natural
         <?> "simple expression"

 table   = [ [prefix "-" negate, prefix "+" id ]
           , [postfix "++" (+1)]
           , [binary "*" (*) AssocLeft, binary "/" (div) AssocLeft ]
           , [binary "+" (+) AssocLeft, binary "-" (-)   AssocLeft ]
           ]

 binary  name fun assoc = Infix (do{ reservedOp name; return fun }) assoc
 prefix  name fun       = Prefix (do{ reservedOp name; return fun })
 postfix name fun       = Postfix (do{ reservedOp name; return fun })

-}

  
languageDef =
  emptyDef { Token.reservedOpNames = [ "+", "-", "*", "/"
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

term1 = do
  dbg "term1"
  choice [ try $ parens lexer expr
         , try curInstExpr
         , try callExpr
         , try identExpr
         , try numExpr
         , try charExpr
         ]
    
term2 = do
  dbg "term2"
  spacex
  t <- term1
  spacex
  return t

expr :: Parser Expr
expr = do
  dbg "expr"
  buildExpressionParser ops term2

------------------------------------------------------------------
data Macro = MacroLine Ident ArgList [Stmt]
           | MacroBlock Ident ArgList [Stmt]
           deriving (Show, Eq)

macroLine = do
  dbg "macroLine"
  string ".macro"
  spacex
  macroName <- ident
  spacex
  args <- argList
  spacex
  es <- many1 (do spacex
                  s <- stmt
                  spacex
                  return s)
  return $ MacroLine macroName args es

macroBlock = do dbg "Entering macro block"
                string ".macro"
                spacex
                name <- ident
                dbg $ (show name)
                spacex
                args <- argList
                dbg $ (show args)
                spacex
                char '{'
                spaces
                ss <- many stmts
                dbg (show ss)
                spaces
                dbg $ "Done with macroblock ss"
                char '}'
                return $ MacroBlock name args ss

stmts = do dbg "stmts"
           spaces
           dbg "Entering stmts"
           s <- try stmt
           spaces
           return s 

macro = do try macroLine <|> try macroBlock                  

------------------------------------------------------------------
data Stmt = AssignStmt Ident Expr
          | ExprStmt Expr
          | IdentStmt Ident
          | ProcStmt Proc
          deriving (Show, Eq)
                             
assn :: Parser Stmt
assn = do dbg "assn"
          name <- ident
          spacex
          string "="
          spacex
          e <- expr          
          return $ AssignStmt name e

exprStmt = do
  dbg "exprStmt"
  expr >>= (return . ExprStmt)

procStmt = do
  dbg "procStmt"
  proc >>= (return . ProcStmt)
       
stmt :: Parser Stmt
stmt = do
  dbg "stmt"
  s <- choice [ try assn
              , try procStmt
              , try exprStmt
              , try callStmt
              , try identStmt
              ]
  return s

callStmt = do
  dbg "callStmt"
  callExpr >>= (return . ExprStmt)
  
identStmt = do
  dbg "identStmt"
  ident >>= (return . IdentStmt)

exprListEmpty :: Parser [Expr]
exprListEmpty = do
  dbg "exprListEmpty"
  char '('
  spacex
  char ')'
  return []

exprList :: Parser [Expr]
exprList = do
  dbg "exprList"
  char '('
  spacex
  fst <- expr
  rest <- many $ do spacex
                    char ','
                    spacex
                    expr
  spacex
  char ')'
  return (fst:rest)
             


callExpr :: Parser Expr
callExpr = do
  dbg "In: callExpr"
  name <- ident <?> "Failed to get identifier of call expression"
  spacex
  elist <- try exprList <|> try exprListEmpty
  return $ Call name elist

------------------------------------------------------------------
data Proc = Include String
          | Align (Maybe Expr)
          | Ascii String
          | Text String
          | Breakpoint
          | Protect
          | Unprotect
            deriving (Show, Eq)

str = do spacex
         char '"'
         txt <- many $ noneOf "\""
         char '"'
         spacex
         return txt

align = do string ".align"           
           return . Align =<< optionMaybe expr

text = do string ".text"
          s <- str
          return $ Text s

ascii = do string ".ascii"
           s <- str
           return $ Ascii s

breakpoint :: Parser Proc
breakpoint = string ".breakpoint" >> return Breakpoint

protect :: Parser Proc  
protect = string ".protect" >> return Protect

include :: Parser Proc
include = do string ".include"
             spacex
             char '"'
             filename <- many $ (alphaNum <|> oneOf "._" <?> "invalid file name")
             char '"'
             return $ Include filename

proc = choice [ try include
              , try protect
              , try breakpoint
              , try ascii
              , try text
              , try align
              ]

data TopLevel = TopStmt Stmt
              | TopMacro Macro
              | TopLabel Label
              | TopProc Proc
                deriving (Show, Eq)

topStmt = return . TopStmt  =<< stmt
topMac = return . TopMacro =<< macro
topProc = return . TopProc =<< proc
topLbl = return . TopLabel =<< Lib.label
topX = try topLbl <|> try topProc <|> try topMac <|> try topStmt

topLevels = do
  dbg "topLevels"
  spaces
  top <- topX
  spaces     
  return top

topLevel = many $ topLevels

-- ".macro extract_field1 (RA, M, N, RB) { a = 10 \n b = 30 \n SHLC(RA, 31-M, RB) \n }"
