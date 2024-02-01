-- module Parser
-- ( Variable 
-- , Expression (..)
-- ) where

import Data.Char

-- lexing

data Token
    = Lambda
    | Vars String
    | Dot
    | LParen
    | RParen
    deriving Show

lexer :: String -> [Token]
lexer [] = []
lexer (x:xs)
    | isSpace x = lexer xs
    | isAlpha x = lexVar (x:xs)
lexer ('/':xs) = Lambda : lexer xs
lexer ('.':xs)  = Dot : lexer xs
lexer ('(':xs)  = LParen : lexer xs
lexer (')':xs)  = RParen : lexer xs

-- all identifiers are 1 character long until we 
-- figure out how to have longer identifiers
lexVar :: String -> [Token]
lexVar (x:xs) = Vars [x] : lexer xs

-- parsing

data Expression 
    = Nul
    | Var String 
    | Abstract String Expression 
    | Apply Expression Expression 
    deriving Eq

parse :: [Token] -> Expression
parse [] = Nul
parse xs = parseExpr xs

-- expecting x or /

-- \x.(xy)z                     -- default
-- (Apply x Apply y) Apply z 

-- \x.x(yz) --
-- Apply x (Apply y Apply z)

parseExpr (LParen:xs) = 
parseExpr (Vars v:xs) = Apply (parseExpr xs) (Var v) 
parseExpr (Lambda:xs) = parseBind xs
parseExpr _ = error "parse error"

-- expecting x.
parseBind (Vars v:xs) = Abstract v (parseBind xs)
parseBind (Dot:xs) = parseExpr xs
parseBind _ = error "expected variable in bind"

semant :: Expression -> Expression
semant (Abstract _ Nul) = error "cannot have Nul body in abstraction"
semant (Apply a b) = case (a,b) of
    (expr, Nul) -> expr
    (Nul, _) -> error ""
