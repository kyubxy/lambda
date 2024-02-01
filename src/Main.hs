module Main where

import Parser (parse, lexer)
import Absyn (genAbsyn)
import Lambda (betaReduce)

main = do
    print "kyubey's lambda calculus calculator"
    print "[INTERACTIVE MODE]"
    print "Enter lambda expression below"
    input <- getLine
    (print . betaReduce . genAbsyn . parse . lexer) input
