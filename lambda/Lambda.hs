module Lambda 
( alphaConvert
, betaReduce
, toNormal
) where

import Absyn (Expression(Nul, Var, Abstract, Apply))

-- | applies alpha conversion on the given term by replacing the bound
--   variable with the given one. Only works on abstractions.
alphaConvert :: Expression -> String -> Expression
alphaConvert (Abstract var term) replace = Abstract replace (convert term)
    where
        convert (Var x)       = Var (if x == var then replace else x)
        convert (Apply t1 t2) = Apply (convert t1) (convert t2)
        convert apply         = apply
alphaConvert _ _ 
    = error ("can only apply alpha conversion on abstractions, " 
        ++ "ie lambda terms of the form (\\x.M[x])")

-- | applies a lazy beta reduction using deterministic sematics to a term once
betaReduce :: Expression -> Expression
betaReduce (Apply (Abstract x m) e) = substitute m e x
betaReduce e = e

-- | uses deterministic semantics to reduce an expression to normal form
toNormal :: Expression -> Expression
toNormal = toNormal' Nul

-- helper function
toNormal' :: Expression -> Expression -> Expression
toNormal' prevExp currentExp 
    | prevExp == currentExp = betaReduce currentExp 
    | otherwise = toNormal' currentExp (betaReduce currentExp)

-- | analogous to exp[m\x], m -> x in x, (replace all x with m in exp, m into x) 
--   main expression -> expression to replace with -> variable to replace -> output expression
substitute :: Expression -> Expression -> String -> Expression
substitute t@(Var v) m x 
    | v == x    = m
    | otherwise = t
substitute t@(Abstract b e) m x 
    -- x is already bound, nothing to do here..
    | b == x    = t 
    -- x is unbound
    | otherwise = Abstract b (substitute e m x)
substitute (Apply e1 e2) m x = Apply (substitute e1 m x) (substitute e2 m x)
substitute Nul _ _ = error "cannot substitute into the null expression"
