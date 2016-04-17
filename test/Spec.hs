{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
import Lib

import qualified Text.Parsec as TP
import Text.Parsec.Error
import Text.Parsec.String
import Control.Monad

main :: IO ()
main = do putStrLn "--------------------------------------------"
          putStrLn "Testing parser..."
          testLitNums
          testIdent
          testIdentSepComma
          testExpr
          
assertParse :: (Eq a, Show a) => TP.Parsec [Char] () a -> [Char] -> a -> IO ()
assertParse rule str val = do
  let result = TP.parse rule "" str
  case result of
    (Right v) -> if v == val
                 then return ()
                 else error (show v)
    (Left msg) -> error (show msg)

testLitNums = do
  assertParse litNum "42" (LitNum 42)
  assertParse litNum "0x0" (LitNum 0) 
  assertParse litNum "0b0" (LitNum 0)
  assertParse hexNum "0x0" 0 
  assertParse binNum "0b11" 3
  assertParse hexNum "0xF" 15

testIdent = do
  let f = assertParse ident
  f "asdf" (Ident "asdf")
  f "." CurInstruction
  f "__asdf234" (Ident "__asdf234")
  
testIdentSepComma = do
  let f = assertParse identSepComma
  f "a, ." [Ident "a", CurInstruction]
  f "a, b, c" $ map (Ident . (:[])) "abc"
  f "a,b,  c" $ map (Ident . (:[])) "abc"

testIdentList = do
  let x = Ident "x"
      f = assertParse identList
  f "()" []
  f "(x)" [x]
  f "(x,x,x)" [x,x,x]
  f "(x , x,x  , x)" [x,x,x,x]

testExpr = do
  let f = assertParse expr2
  f ("1 + 2") (ExprTermExpr (TermLitNum (LitNum 1))
               [ExprBinTail Addition (TermLitNum (LitNum 2))])

  let abc = (ExprTermExpr (TermIdent (Ident "a"))
             [ExprBinTail Addition (TermIdent (Ident "b")),
              ExprBinTail Addition (TermIdent (Ident "c"))])

  f ("a+b+c") abc
  f ("-a") (ExprNeg (ExprTermExpr (TermIdent (Ident "a")) []))


testQuotedString = do
  let f = assertParse quotedString
  f "\"asdf\"" "asdf"

  -- f ("((a+b+c))") abc
  -- parseTest("ExprList", "(1, 1, 1)")    

{-
    
------------------------------------------------------------------    
def testAstBinopStar():
    parseTest("Binop", "*")
def testAstBinopPlus():
    parseTest("Binop", "+")
def testAst_binop_fslash():
    parseTest("Binop", "/")   
def testAst_binop_minux():
    parseTest("Binop", "-")    
def testAstBinopLeftShift():
    parseTest("Binop", "<<")    
def testAstBinopRightShift():
    parseTest("Binop", ">>")
    
def testStmt1():
    parseTest("Stmt", "a = 23")
def testStmt2():
    parseTest("Stmt", ". = 23")
def testStmt3():
    parseTest("Stmt", ". = 23 << 1") 
def testStmt4():
    parseTest("Stmt", ". = 1+(-0x23 << --0b01)")
def testStmt5():
    parseTest("Stmt", "betaopc(0x1B,RA,0,RC)")   
def testStmt6():
    parseTest("Stmts", "a=10 b=20 a b c")
def testStmt7():
    parseTest("Stmts", "a=10 \n b=20 \n c=30 \n")
def testStmt8():
    parseTest("Stmts", "A")
def testStmt9():
    parseTest("Stmts", ". . . .")

    
def testMacro0():
    parseTest("Macro", ".macro CALL(label) BR(label, LP) //asdf \n")
def testMacro1():
    parseTest("Macro", ".macro RTN() JMP(LP)\n")
def testMacro2():
    parseTest("Macro", ".macro XRTN() JMP(XP)\n")
def testMacro3():
    parseTest("Macro", ".macro GETFRAME(OFFSET, REG) LD(bp, OFFSET, REG) \n")
def testMacro4():
    parseTest("Macro", ".macro PUTFRAME(REG, OFFSET) ST(REG, OFFSET, bp) \n")
def testMacro_5():
    parseTest("Macro", ".macro CALL(S,N) BR(S,lp) SUBC(sp, 4*N, sp) \n")
def testMacro_6():
    parseTest("Macro", ".macro ALLOCATE(N) ADDC(sp, N*4, sp) \n")
def testMacro_7():
    parseTest("Macro", ".macro DEALLOCATE(N) SUBC(sp, N*4, sp) \n")
def testMacro_8():
    parseTest("Macro", ".macro save_all_regs(WHERE) save_all_regs(WHERE, r31)\n")
def testMacro_9():
    parseTest("Macro", ".macro A(a) {\nA\n}")
def testMacro_10():
    parseTest("Macro", ".macro A(a) {A}")
def testMacro_11():
    parseTest("Macro", ".macro extract_field1 (RA, M, N, RB) {\na = 10\n }")
def testMacro_12():
    parseTest("Macro", ".macro A(a) {\nA\n}")
def testMacro_13():
    parseTest("Macro", ".macro A(a) {A\n A\n A}\n\n")
def testMacro_14():
    parseTest("Macro", ".macro JMP(RA, RC) betaopc(0x1B,RA,0,RC)\n ")
def testMacro_15():
    parseTest("Macro", ".macro LD(RA, CC, RC) betaopc(0x18,RA,CC,RC)\n ")
def testMacro_16():
    parseTest("Macro", ".macro LD(CC, RC) betaopc(0x18,R31,CC,RC)\n ")
def testMacro_17():
    parseTest("Macro", ".macro ST(RC, CC, RA) betaopc(0x19,RA,CC,RC)\n ")
def testMacro_18():
    parseTest("Macro", ".macro ST(RC, CC) betaopc(0x19,R31,CC,RC)\n ")
def testMacro_19():
    parseTest("Macro", ".macro LDR(CC, RC) BETABR(0x1F, R31, RC, CC)\n")
def testMacro_20():
    parseTest("Macro", ".macro PUSH(RA) ADDC(SP,4,SP)  ST(RA,-4,SP)\n ")
def testMacro_21():
    parseTest("Macro", ".macro POP(RA) LD(SP,-4,RA)   ADDC(SP,-4,SP)\n ")

def testTopLevel_0():
    parseTest("Macro", ".macro A (RA, M, N, RB) {a = 10}") 
def testTop_level_1():
    parseTest("Macro", ".macro PUTFRAME(REG, OFFSET)  ST(REG, OFFSET, bp)\n") 
def testTop_level_2():
    parseTest("Macro", ".macro PUTFRAME(REG, OFFSET)  ST(REG, OFFSET, bp)\n") 
def testTop_level_3():
    parseTest("TopLevel", ".macro PUTFRAME(REG, OFFSET)  ST(REG, OFFSET, bp)\n")
def testTop_level_4():
    parseTest("TopLevel", ".macro LONG(x) WORD(x) WORD(x >> 16) // asdfasdf \n")
def testTop_level_5():
    fileTest("macroblock.uasm")
   
def fileTest(f):
    txt = open("./tests/"+f).read()
    parseTest("TopLevel", txt)
    
def testFiles1():
    fileTest("macroblock.uasm")    
def testFiles2():
    fileTest("beta1.uasm")
def testFiles3():
    fileTest("beta.uasm")

def testProcessor0():
    parseTest("Proc", '.text "asdf"') 
def testProcessor_1():
    parseTest("Proc", '.options mul')
    
def testParseTest_assn_1():    
    parseTest("Assn", "a = 23")
def testParse_test_assn_2():    
    parseTest("Assn", ". = 23")
def testParse_test_assn_3():    
    parseTest("Assn", ". = 23 << 1") 
def testParse_test_assn_4():    
    parseTest("Assn", ". = 1+(-0x23 << --0b01)")
def testParse_test_assn_5():    
    parseTest("Assn", "VEC_RESET = 0")
def testParse_test_assn_6():    
    parseTest("Assn", "VEC_II = 4")
    
def testParseTest_call_1():
    parseTest("Call", "Hello(asdf,123)") 
def testParse_test_call_2():
    parseTest("Call", "A_(1,2,3,4,-0x123)")
def testParse_test_call_3():
    parseTest("Call", "Asdf()")

def testParseTest_expr_1():    
    parseTest("Expr2", "1 + 2 + (3)")
def testParse_test_expr_2():    
    parseTest("Expr2", "(1) + 2 + (3)")
def testParse_test_expr_3():
    parseTest("Expr2", "(1 + 2)")
def testParse_test_expr_4():
    parseTest("Expr2", "(1+2+3)")
def testParse_test_expr_5():
    parseTest("Expr2", "((1)+2+3)")
def testParse_test_expr_6():
    parseTest("Expr2", "(((1)))")
def testParse_test_expr_7():
    parseTest("Expr2", "((1+1))")
def testParse_test_expr_8():
    parseTest("Expr2", "((1+(1+1)))")
def testParse_test_expr_9():
    parseTest("Expr2", "(1+1+(1+(1+1)))")
def testParse_test_expr_10():
    parseTest("Expr2", "(((1+((1))+(1+(1+1)))))")
def testParse_test_expr_11():
    parseTest("Expr2", "1+(2+3)+4")
def testParse_test_expr_12():
    parseTest("Expr2", "1020 + 0xDeadBeef + 0b101 + (3)")
def testParse_test_expr_13():
    parseTest("Expr2", "1020 << 0xDeadBeef - (0b101 / (3))")
def testParse_test_expr_14():
    parseTest("Expr2", "(-1020 + -0x200)") 
def testParse_test_expr_15():
    parseTest("Expr2", "-(1 + -1)")
def testParse_test_expr_16():
    parseTest("Expr2", "--(1 + -1)")
def testParse_test_expr_17():
    parseTest("Expr2", "--(---1 + --(--1))")
def testParse_test_expr_18():
    parseTest("Expr2", "--(--1%--(2>>1--1))")
def testParse_test_expr_19():
    parseTest("Expr2", "(.+_-_+.)")
def testParse_test_expr_20():
    parseTest("Expr2", "-(.)+.+-.")



-}


{-
go1 x y = setupTest x y >>= processResults
  
processResults (file, result) = do
  case result of
    Left err -> putStrLn $ "Fail: " ++ file ++ "\n" ++ (show err)
    Right msg -> putStrLn $ "Pass: " ++ file ++ ": " ++ (show msg)
  
setupTest parser file = do
  txt <- readFile $ "./test/uasm/" ++ file
  let t = eraseComments txt
  let r = TP.parse parser file t
  return (file, r)

eraseLineComment [] _ = []
eraseLineComment src@(c:str) inComment =
  if take 2 src == "//"
  then "  " ++ (eraseLineComment (drop 2 src) True)
  else if c == '\n'
       then c:(eraseLineComment str False)
       else if inComment
            then ' ':(eraseLineComment str True)
            else c:(eraseLineComment str False)

eraseBlockComment [] _ = []
eraseBlockComment src@(c:str) inComment =
  if take 2 src == "/*" && not inComment
  then "  " ++ (eraseBlockComment (drop 2 src) True)       
  else if take 2 src == "*/" && inComment
       then "  " ++ (eraseBlockComment (drop 2 src) False)
       else if inComment               
            then ' ':(eraseBlockComment str inComment)
            else c:(eraseBlockComment str inComment)

eraseComments src =
  (eraseLineComment
   (eraseBlockComment src False) False)
-}
