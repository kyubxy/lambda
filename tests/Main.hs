module Main where

import Test.HUnit
import Lambda (toNormal)
import Absyn (Expression(..), Absynable(..))
import Parser (parse, lexer)
import qualified System.Exit as Exit

-- TODO: if the parsing fails, the tests will pass
readexp = genAbsyn . parse . lexer

testSingleVar = "test one var" ~: "x" ~: exp ~=? (toNormal exp) 
    where 
        exp = Var "x"

testSingleAbs = "test one abstraction" ~: "/x.x" ~: exp ~=? (toNormal exp)
    where exp = Abstract "x" (Var "x")

testSingleApp = "test one application" ~: "xy" ~: exp ~=? (toNormal exp)
    where exp = Apply (Var "x") (Var "x")

tests = test [
    testSingleVar, testSingleAbs, testSingleApp,

    "test beta reduction with no free variables" ~: "(/y.yyy)x -> /x.xxx" 
        ~: (Apply (Var "x")(Apply (Var "x") (Var "x"))) 
        ~=? (toNormal (Apply (Abstract "y" (Apply (Var "y")(Apply (Var "y") (Var "y"))))(Var "x"))),

    "test beta reduction with free variables" ~: "(/y.yzy)x -> /x.xzx" 
        ~: (Apply (Var "x")(Apply (Var "z") (Var "x"))) 
        ~=? (toNormal (Apply (Abstract "y" (Apply (Var "y")(Apply (Var "z") (Var "y"))))(Var "x"))),

    "test beta reduction with free variables" ~: "(/y.yzy)x -> /x.xzx" 
        ~: (Apply (Var "x")(Apply (Var "z") (Var "x"))) 
        ~=? (toNormal (Apply (Abstract "y" (Apply (Var "y")(Apply (Var "z") (Var "y"))))(Var "x"))),

    -- tests taken from questions in https://www.cs.umd.edu/class/fall2017/cmsc330/tests/prac8-soln-fall13.pdf

    "test beta reduction 1" ~: "(/z.z)(/y.yy)(/x.xa) -> a a" 
        ~: readexp "aa"
        ~=? (toNormal . readexp) "(/z.z)(/y.yy)(/x.xa)",

    "test beta reduction 2" ~: "(/z.z)(/y.zz)(/z.zy) -> a a" 
        ~: readexp "yy"
        ~=? (toNormal . readexp) "(/z.z)(/y.zz)(/z.zy)",
        
    -- haskell requires ] to be on the same line as a list elem for some reason
    "dummy" ~: "dummy" ~: 5 ~=? 5 ] 

main = do 
    result <- runTestTT tests
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess
