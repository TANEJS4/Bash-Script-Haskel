module ExprPretty where

import ExprType

{-
  module ExprPretty:
  Making code Prittier
  Description : Contains a type class and instances for
  parsing the expressions.

  License : WTFPL
  Maintainer : Tanejs4@mcmaster.ca
  Stability : experimental

-}

parens :: String -> String
parens ss = "(" ++ ss ++ ")"

instance Show a => Show (Expr a) where
  show (Add e1 e2) = parens (show e1) ++ " !+ " ++ parens (show e2)
  show (Mult e1 e2) = parens (show e1) ++ " !* " ++ parens (show e2)
  show (Cos x)  = "Cos" ++ parens (show x)
  show (Sin x) = "Sin" ++ parens (show x)
  show (Var ss)     = parens $ "Var " ++ "\"" ++ ss ++ "\""
  show (Const x)    = parens $ "Val. " ++ show x
  show (NatExp x) = "e^" ++ parens (show x)
  show (Ln x) = "ln" ++ parens (show x)
  show (Exp e1 e2) = parens (show e1) ++ " !^ " ++ parens (show e2)
