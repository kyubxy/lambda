module Absyn 
(
    Expression (..),
    Absynable (..),
) where

--import Parser (PtExpr(PtAbstract, PtApp), PtApp(PtApply, PtSimple), PtSimple(PtVar, PtBrack))
import Parser (PtExpr(..), PtApp(..), PtSimple(..))

data Expression 
    = Nul
    | Var String 
    | Abstract String Expression 
    | Apply Expression Expression 
    deriving (Eq, Read)

instance Show Expression where
    show (Var v) = show v
    show (Abstract var term) = "\\" ++ show var ++ "." ++ show term
    -- TODO: only show brackets when contents have more than 1 term
    show (Apply t1 t2) = "(" ++ show t1 ++ ")(" ++ show t2 ++ ")" 
    show Nul = "nul"

class Absynable a where
    genAbsyn :: a -> Expression

instance Absynable PtExpr where
    genAbsyn (PtAbstract var expr) = Abstract var (genAbsyn expr)
    genAbsyn (PtApp app) = genAbsyn app

instance Absynable PtApp where
    genAbsyn (PtApply expr simp) = Apply (genAbsyn expr) (genAbsyn simp)
    genAbsyn (PtSimple simp) = genAbsyn simp

instance Absynable PtSimple where
    genAbsyn (PtVar str) = Var str
    genAbsyn (PtBrack expr) = genAbsyn expr
