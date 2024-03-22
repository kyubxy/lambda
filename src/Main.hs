module Main where

import System.Exit

import Parser (parse, lexer)
import Absyn (genAbsyn, Expression)
import Lambda (toNormal)

main = do
    print "kyubey's lambda calculus calculator"
    print "[INTERACTIVE MODE]"
    interactive

interactive = do
    print "Enter lambda expression below"
    input <- getLine
    if input == "q" then
        exitSuccess
    else
        (print . toNormal . genAbsyn . parse . lexer) input
    interactive
