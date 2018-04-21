module ExprTest where

import           ExprDiff
import           ExprParser
import           ExprPretty
import           ExprType

import qualified Data.Map.Strict as Map
import           Test.QuickCheck

-- | tests Add
sampleExpr1 :: Expr Double
sampleExpr1 = ((var "x") !+ (var "y"))


-- | tests ability to take a list and convert into an Expr
listToExpr1 :: [Double] -> Expr Double
listToExpr1 [x]    = Const x
listToExpr1 (x:xs) = Add (Const x) (listToExpr1 xs)
listToExpr1 []     = error "Not list to expression for empty"


-- | tests validity of Add
evalProp1 :: Double -> Double -> Bool
evalProp1 n n' = eval (Map.fromList [("y",n),("z",n')]) (Add (Var "y") (Var "z")) == n + n'
testEvalProp1 = quickCheck evalProp1

-- | tests validity of Mult
evalProp2 :: Double -> Double -> Bool
evalProp2 n n' = eval (Map.fromList [("y",n),("z",n')]) (Mult (Var "y") (Var "z")) == n * n'
testEvalProp2 = quickCheck evalProp2

-- | tests validity of Cos
evalProp6 :: Double -> Bool
evalProp6 n = eval (Map.fromList [("x",n)]) (Cos (Var "x")) == cos(n)
testEvalProp6 = quickCheck evalProp6

-- | tests validity of Sin
evalProp7 :: Double -> Bool
evalProp7 n = eval (Map.fromList [("x",n)]) (Sin (Var "x")) == sin(n)
testEvalProp7 = quickCheck evalProp7
