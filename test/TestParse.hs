
import           Uasm.Parser
import           Uasm.Types
import qualified Text.Parsec as TP
import qualified TestExpand as TE
import qualified TestLabelPass as TLP
import qualified TestFinalPass as TFP

import           Test.Tasty
import           Test.Tasty.HUnit

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [ parseTests
                          , TE.testAll
                          , TLP.testAll
                          , TLP.testProc
                          , TFP.testAll
                          ]

parseTests = testGroup "Parse tests"
  [ testCase "Idents parse" $ testIdent
  , testCase "testFile" $ testFile "beta.uasm"
  , testCase "testLitNums" $ testLitNums 
  , testCase "testIdent" $ testIdent 
  , testCase "testIdentSepComma" $ testIdentSepComma 
  , testCase "testExpr" $ testExpr 
  , testCase "testQuotedString" $ testQuotedString 
  , testCase "testCall" $ testCall 
  , testCase "testAssn" $ testAssn 
  , testCase "testStmt" $ testStmt 
  , testCase "testMacro" $ testMacro 
  ] 

assertParse :: (Eq a, Show a) => TP.Parsec [Char] () a -> [Char] -> a -> IO ()
assertParse rule str val = do
  let result = TP.parse (rule <* TP.eof) "" str
  case result of
    (Right v) -> if v == val
                 then return ()
                 else do print ("Expected: ", val)
                         print ("Parsed  : ", v)
                         error (show v)
    (Left msg) -> error (show msg)

justParse :: (Eq a, Show a) => TP.Parsec [Char] () a -> [Char] -> IO ()
justParse rule str = do  
  let result = TP.parse (rule <* TP.eof) "" str
  case result of
    (Right _) -> return ()
    (Left msg) -> error $ (show msg) ++ "\n\n" ++ (show str)

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
      abc = [Ident "a", Ident "b", Ident "c"]
  
  f "a, ." [Ident "a", CurInstruction]
  f "a, b, c" $ abc
  f "a,b,  c" $ abc

testIdentList = do
  let x = Ident "x"
      f = assertParse identList
  f "()" []
  f "(x)" [x]
  f "(x,x,x)" [x,x,x]
  f "(x , x,x  , x)" [x,x,x,x]

testExpr = do
  let f = assertParse expr2
  let j = justParse expr2
  
  f ("1 + 2") (ExprTermExpr (TermLitNum (LitNum 1))
               [ExprBinTail Addition (TermLitNum (LitNum 2))])

  let abc = (ExprTermExpr (TermIdent (Ident "a"))
             [ ExprBinTail Addition (TermIdent (Ident "b")),
               ExprBinTail Addition (TermIdent (Ident "c"))])

  f ("a+b+c") abc
  f ("-a") (ExprNeg (ExprTermExpr (TermIdent (Ident "a")) []))

  case TP.parse expr2 "" "((a+b+c))" of
    (Right val1) -> do f "((a+b+c))" val1
                       f "( (a+b+c))" val1 
                       f "(( a+b+c))" val1
                       f "( (a+b+  c) )" val1 
                       f "(( a+ b+c))" val1
                       f "(   (   a + b + c )  )" val1
                       f "((a + b + c)  )" val1
                       f "(  ( a+b + c)  )" val1
    (Left msg) -> error (show msg)
    
  j "-----a+ --1"
  j "-(-(--a+-1))"

quote s = "\"" ++ s ++ "\""
  
testQuotedString = do
  let f = assertParse quotedString
  let j = justParse quotedString
  f (quote "asdf") "asdf"
  f (quote " hello ") " hello "
  j (quote "   asdf asdf asdf asdf \\t ")

testAssn = do
  let j = justParse assn
      f = assertParse assn
      result1 = (Assn
                 (Ident "a")
                 (ExprTermExpr (TermLitNum (LitNum 23)) []))

  f "a = 23" result1
  f "a=23" result1
  f "a= 23" result1
  f "a =23" result1
  j ". = 23"
  j ". = 23 << 1"
  j ". = 1+(-0x23 << --0b01)"

testCall = do
  let f = assertParse call
  let j = justParse call
  f "foo(a,b)" $ (Call (Ident "foo")
                  [ ExprTermExpr (TermIdent (Ident "a")) []
                  , ExprTermExpr (TermIdent (Ident "b")) []])

  f "foo()" $ (Call (Ident "foo") [])
  f "foo(1,2)" $ (Call (Ident "foo")
                  [ ExprTermExpr (TermLitNum (LitNum 1)) []
                  , ExprTermExpr (TermLitNum (LitNum 2)) []])

  j "Hello(asdf,  123)"
  j "A_(1,2,3,4,-0x123)"
  j "Asdf()"

testMacro = do
  let f = assertParse macro
      mac1 = (Macro (Ident "Add")
              [Ident "A",Ident "B"]
              [StmtExpr
               (ExprTermExpr
                (TermIdent (Ident "A"))
                [ExprBinTail Addition (TermIdent (Ident "B"))])])
  
  f (".macro Add(A,B) A+B\n") mac1
  f (".macro Add(A,B) A+B \n") mac1
  f (".macro Add(A,B) { A + B }") mac1
  f (".macro Add(A,B) {\n A + B\n }") mac1
  f (".macro Add(A,B) { A + B\n }") mac1
  
testStmt = do
  let f = assertParse stmt
      j = justParse stmt
      betaopc = Ident "betaopc"
      num = LitNum 0x1B
      ra = Ident "RA"
      result1 = (StmtCall
                 (Call (Ident "betaopc")
                  [ ExprTermExpr (TermLitNum (LitNum 27)) []
                  , ExprTermExpr (TermIdent (Ident "RA")) []
                  , ExprTermExpr (TermLitNum (LitNum 0)) []
                  , ExprTermExpr (TermIdent (Ident "RC")) []]))
  
  -- f "betaopc(0x1B,RA)" (Call betaopc [Expr )
  -- j "a=10 b=20 a b c"
  -- j "a=10 \n b=20 \n c=30 \n"
  j "A"
  -- j ". . . ."
  f "betaopc(0x1B,RA,0,RC)" result1
  -- j "a=10 \n b=20 \n c=30 \n"
  j "A"
  j "a+b"
  f "." (StmtExpr (ExprTermExpr (TermIdent CurInstruction) []))
  j "a = 23"
  j ". = 23"
  j ". = 23 << 1"
  j ". = 1+(-0x23 << --0b01)"

testFile file = setupTest (TP.many topLevel) file

setupTest parser file = do
  txt <- readFile $ "./test/uasm/" ++ file
  let t = eraseComments txt
  justParse parser t
  

{-
------------------------------------------------------------------    
    
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


  
