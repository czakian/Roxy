{-# LANGUAGE OverloadedStrings, GADTs #-}

module Roxy.HTTPInspector where 

import Data.ByteString.Char8
import Prelude hiding(lookup)

-- | Define that an environment must provide a lookup function for variables. 
-- The actual data structure of the environment must be provided by the user. 
class Env env where
  lookup ::  ByteString -> env -> SealedPrim
  extend :: ByteString -> SealedPrim -> env -> env

--TODO: this doesn't work right with the env typeclass above.
data Prim a where
    I   :: Int        -> Prim Int
    B   :: Bool       -> Prim Bool
    S   :: ByteString -> Prim ByteString
    IP  :: ByteString -> Prim ByteString
    Var :: ByteString -> Prim ByteString

data SealedPrim where
  SealedPrim :: Prim a -> SealedPrim

data Expr a where
    Not :: Expr Bool  -> Expr Bool
    Add :: Expr Int   -> Expr Int -> Expr Int
    Mul :: Expr Int   -> Expr Int -> Expr Int
    And :: Expr Bool  -> Expr Bool -> Expr Bool
    Or  :: Expr Bool  -> Expr Bool -> Expr Bool
    Eq  :: (Eq a)  => Expr a -> Expr a -> Expr Bool
    Lt  :: (Ord a) => Expr a -> Expr a -> Expr Bool
    Gt  :: (Ord a) => Expr a -> Expr a -> Expr Bool
    Leq :: (Ord a) => Expr a -> Expr a -> Expr Bool
    Geq :: (Ord a) => Expr a -> Expr a -> Expr Bool
    S2I :: ByteString -> Expr (Prim Int)
    S2B :: ByteString -> Expr Bool
    S2IP:: ByteString -> Expr ByteString 
    Pfx :: Expr ByteString -> Expr ByteString -> Expr Bool -- Prefix

eval :: (Env env) => env -> Expr a -> a
--eval env (I n)   = n
--eval env (B b)   = b
--eval env (S s)   = s
--eval env (IP ip) = ip 
--eval env (Var v) = lookup v env
eval env (Not e) = not  (eval env e)
eval env (Add  e1 e2) = (eval env e1) +  (eval env e2)
eval env (Mul  e1 e2) = (eval env e1) *  (eval env e2)
eval env (Eq   e1 e2) = (eval env e1) == (eval env e2)
eval env (Lt   e1 e2) = (eval env e1) <  (eval env e2)
eval env (Gt   e1 e2) = (eval env e1) >  (eval env e2) 
eval env (Leq  e1 e2) = (eval env e1) <= (eval env e2)
eval env (Geq  e1 e2) = (eval env e1) >= (eval env e2)
eval env (Pfx  e1 e2) = isPrefixOf (eval env e1) (eval env e2)

