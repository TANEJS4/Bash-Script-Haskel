{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
module ExprDiff where

import ExprType

import qualified Data.Map as Map
{-
  Class diffExpr:
    Differentiable Expressions
  Description : Contains a type class and instances for
  differentiable expressions

  License : WTFPL
  Maintainer : Tanejs4@mcmaster.ca
  Stability : experimental

  ----------------------------------------------

  The DiffExpr class cointains methods over the
  Expr datatype tht helps with making and
  evaluating diferentiable expressions.

  ----------------------------------------------

  Methods:
    eval: Takes a dictionary of variable-identifiers
          and values, and uses it to compute the Expr.

    simplify: Takes a dictionary (complete or incomplete)
              and uses it to reduce Expr as much as
              possible.

              e1 =a + b
              e2 = b +a
              e1 == e2

              Add (Add (Var "x") (Const 1) (Add (Const 2) (Var "b")))
              => Add (Const 3) (Add (Var "x") (Var "b"))
    partDiff: Given a var identifier, differentiate
          in terms of that identifier
    Default Methods:
      !+,!*,var,val: are function wrappers for Expr
                     constructors that perform
                     additional simplification
-}

class DiffExpr a where
  eval :: Map.Map String a -> Expr a -> a
  simplify :: Map.Map String a -> Expr a -> Expr a
  partDiff :: String -> Expr a -> Expr a


  {- Default Methods -}

  (!+) :: Expr a -> Expr a -> Expr a
  e1 !+ e2 = simplify (Map.fromList []) $ Add e1 e2
  (!*) :: Expr a -> Expr a -> Expr a
  e1 !* e2 = simplify (Map.fromList []) $ Mult e1 e2
  (!^) :: Expr a -- ^ First Expression given
          -> Expr a -- ^ Second Expression given
          -> Expr a -- ^ Resulting simplified Exp Expression
  e1 !^ e2 = simplify (Map.fromList []) $ Exp e1 e2
  -- | corresponds to the Cos type wrappers to optimize Cos type. tries to simplify the Expr
  mycos :: Expr a -- ^ Expression given
          -> Expr a -- ^ Resulting simplified Cos Expression
  mycos e1 = simplify (Map.fromList []) $ Cos e1
  -- | corresponds to the Sin type wrappers to optimize Sin type. tries to simplify the Expr
  mysin :: Expr a -- ^ Expression given
          -> Expr a -- ^ Resulting simplified Sin Expression
  mysin e1 = simplify (Map.fromList []) $ Sin e1

  -- | corresponds to the NatExp type wrappers to optimize Natexp type. tries to simplify the Expr
  natexp :: Expr a -- ^ Expression given
            -> Expr a -- ^ Resulting simplified NatExp Expression
  natexp e1 = simplify (Map.fromList []) $ NatExp e1
  -- | Takes a List of Lists and wraps it in a Matrix constructor

  val :: a -> Expr a
  val a = Const a
  var :: String -> Expr a
  var a = Var a



instance (Eq a, Floating a) => DiffExpr a where
  eval vrs (Add e1 e2)  = eval vrs e1 + eval vrs e2
  eval vrs (Mult e1 e2) = eval vrs e1 * eval vrs e2
  eval vrs (Exp e1 e2) = eval vrs e1 ** eval vrs e2
  eval vrs (Cos a) =  cos (eval vrs a)
  eval vrs (Sin a) = sin (eval vrs a)
  eval vrs (NatExp a) = exp (eval vrs a)

  eval vrs (Const a) =a
  eval vrs (Var a) = case Map.lookup a vrs of
                       Just v  -> v
                       Nothing -> error "failed"




  partDiff t (Var a) |a == t = Const 1
                     | otherwise = (Const 0)
  partDiff _ (Const _) = Const 0
  partDiff t (Add e1 e2) =(Add (partDiff t e1) (partDiff t e2))
  partDiff t (Mult e1 e2) = (Add (Mult (partDiff t e1) e2) (Mult e1 (partDiff t e2)))
  partDiff t (Cos a) =(Mult (Const (-1) ) (Mult (partDiff t a) (Sin a)))
  partDiff t (Sin a) = (Mult (partDiff t a) (Cos a))
  partDiff t (NatExp a) = Mult (NatExp a) (partDiff t a)


  simplify vrs (Const a) = Const a
  simplify vrs (Mult (Const 0) e1) = Const 0
  simplify vrs (Mult e1 (Const 0)) = Const 0
  simplify vrs (Mult e1 (Const 1)) = simplify vrs e1
  simplify vrs (Mult (Const 1) e1) =simplify vrs e1
  simplify vrs (Add e1 (Const 0)) = simplify vrs e1
  simplify vrs (Add (Const 0) e1) = simplify vrs e1
  simplify vrs (Exp (e1) (Const 0)) = Const 1
  simplify vrs (Exp (Const 0) e1) = Const 0


  simplify vrs (Var a) = case Map.lookup a vrs of
                          Just v -> Const v
                          Nothing -> Var a

  simplify vrs (Add e1 (Var a)) = case Map.lookup a vrs of
                                    Just v ->  simplify vrs (Add (Const v) (simplify vrs e1))
                                    Nothing -> Add (simplify vrs e1) (Var a)

  simplify vrs (Add (Var a) e1) = case Map.lookup a vrs of
                                    Just v ->  simplify vrs (Add (simplify vrs e1) (Const v) )
                                    Nothing -> Add (Var a) (simplify vrs e1)


  simplify vrs (Mult (Var a) e1) = case Map.lookup a vrs of
                                    Just v ->  simplify vrs (Mult (simplify vrs e1) (Const v) )
                                    Nothing -> Mult (Var a) (simplify vrs e1)

  simplify vrs (Mult e1 (Var a)) = case Map.lookup a vrs of
                                    Just v ->  simplify vrs (Mult (Const v) (simplify vrs e1) )
                                    Nothing -> Mult (simplify vrs e1) (Var a)


  simplify vrs (Add e1 e2) = case ((simplify vrs e1),(simplify vrs e2)) of
                             (Const a , Const b) -> Const (a+ b)
                             _ -> Add (simplify vrs e1)  (simplify vrs e2)



  simplify vrs (Mult e1 e2) = case ((simplify vrs e1) ,(simplify vrs e2)) of
                             (Const a,Const b)->(Const (a*b) )
                             _ -> Mult (simplify vrs e1)  (simplify vrs e2)



  simplify vrs (Exp (Var a) e1) = case Map.lookup a vrs of
                                    Just v ->  simplify vrs (Exp (Const v) (simplify vrs e1) )
                                    Nothing -> Exp (Var a) (simplify vrs e1)

  simplify vrs (Exp e1 (Var a)) = case Map.lookup a vrs of
                                    Just v ->  simplify vrs (Exp (simplify vrs e1)  (Const v) )
                                    Nothing -> Exp (simplify vrs e1) (Var a)
  simplify vrs (Cos (Var a)) = case Map.lookup a vrs of
                                    Just v ->  simplify vrs (Cos (Const v) )
                                    Nothing -> Cos (Var a)
  simplify vrs (Sin (Var a)) = case Map.lookup a vrs of
                                    Just v ->  simplify vrs (Sin (Const v) )
                                    Nothing -> Sin (Var a)

  simplify vrs (Exp e1 e2) = case ((simplify vrs e1) ,(simplify vrs e2)) of
                             (Const a,Const b)->(Const (a**b) )
                             _ -> Exp (simplify vrs e1)  (simplify vrs e2)
  simplify vrs (Cos e1) = case (simplify vrs e1) of
                             Const a -> Const (cos a)
                             _-> Cos (simplify vrs e1)
  simplify vrs (Sin e1) = case (simplify vrs e1) of
                             Const a -> Const (sin a)
                             _-> Sin (simplify vrs e1)

  simplify vrs (NatExp (Var a)) = case Map.lookup a vrs of
                                    Just v ->  simplify vrs (NatExp (Const v) )
                                    Nothing -> NatExp (Var a)
