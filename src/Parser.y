{
module Parser where

import Data.Char (isAlpha, isSpace)
}

%name parse
%tokentype { Token }
%error { parseError }

%token
    vars    { TokenVars $$ }
    '/'     { TokenLambda }
    '.'     { TokenDot }
    '('     { TokenLParen }
    ')'     { TokenRParen }

%%

Expr : '/' vars '.' Expr { PtAbstract $2 $4 }
     | App               { PtApp $1 }

App : Expr Simple        { PtApply $1 $2 }
    | Simple             { PtSimple $1 }

Simple : vars            { PtVar $1 }
       | '(' Expr ')'    { PtBrack $2 }

{
parseError _ = error "parse error"

data PtExpr 
    = PtAbstract String PtExpr
    | PtApp PtApp
    deriving Show

data PtApp 
    = PtApply PtExpr PtSimple
    | PtSimple PtSimple
    deriving Show

data PtSimple
    = PtVar String
    | PtBrack PtExpr
    deriving Show

data Token
    = TokenLambda
    | TokenVars String
    | TokenDot
    | TokenLParen
    | TokenRParen
    deriving Show


lexer :: String -> [Token]
lexer [] = []
lexer (x:xs)
    | isSpace x = lexer xs
    | isAlpha x = TokenVars [x] : lexer xs
lexer ('/':xs)  = TokenLambda : lexer xs
lexer ('.':xs)  = TokenDot : lexer xs
lexer ('(':xs)  = TokenLParen : lexer xs
lexer (')':xs)  = TokenRParen : lexer xs

}
