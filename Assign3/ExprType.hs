{-|
Module      : ExprDiff
Description : Contains a data type and all fields
Copyright   : (c) Shivam Taneja @ 2018
License     : WTFPL
Maintainer  : github.com/TanejS
Stability   : experimental
Portability : POSIX
-}
module ExprType where

import Data.List

{-



  Expression Datatype
  -------------------
  Wraps different operations in a expression tree
  Ops:
    Add - standard binary addition
    Mult - standard binary multiplication
    Const - wrapper for a simple values
    Var - string identifier for variables
-}

-- * Data Type: Expr
-- | makes a data type that contains all operators needed in assignment

data Expr a = Add (Expr a) (Expr a) -- ^ addition operator of type Expr
            | Mult (Expr a) (Expr a) -- ^ multiply operator of type Expr
            | Cos (Expr a) -- ^ This constructor represents the cosine of an expression of type 'Expr a'
            | Sin (Expr a) -- ^ This constructor represents the sin of an expression of type 'Expr a'
            | Exp (Expr a) (Expr a) -- ^ This constructor represents the exponent of two expressions of type 'Expr a'. First expression to the power of second expression.
            | NatExp (Expr a) -- ^ This constructor represents the Natural exponent of an expression of type 'Expr a'
            | Ln (Expr a) -- ^  This constructor represents the Natural Logarithm of an expression of type 'Expr a'
            | Const a -- ^ wraps a constant
            | Var String   -- ^ wraps a variable
  deriving Eq

  -- ** getVars
  -- | Encoding the expression types into a list of strings that can be evaluated


getVars :: Expr a -> [String]
getVars (Add e1 e2)  = getVars e1 `union` getVars e2
getVars (Mult e1 e2) = getVars e1 `union` getVars e2
getVars (Cos e1) = getVars e1
getVars (Sin e1) = getVars e1
getVars (Ln e1) = getVars e1
getVars (NatExp e1) = getVars e1
getVars (Exp e1 e2) = getVars e1 ++ getVars e2
getVars (Const _)    = []
getVars (Var ident)  = [ident]
