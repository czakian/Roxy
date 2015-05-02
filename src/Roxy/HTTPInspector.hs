{-# LANGUAGE OverloadedStrings, GADTs #-}

module Roxy.HTTPInspector where 

import Data.ByteString.Char8
import Network.HTTP.Types (Status)
import Prelude hiding(lookup)

data Expr a where
    I   :: Int        -> Expr Int
    B   :: Bool       -> Expr Bool
    S   :: ByteString -> Expr ByteString
    IP  :: ByteString -> Expr ByteString
    Var :: ByteString -> Expr ByteString
    Not :: Expr Bool -> Expr Bool
    Add :: Expr Int -> Expr Int -> Expr Int
    Mul :: Expr Int -> Expr Int -> Expr Int
    And :: Expr Bool -> Expr Bool -> Expr Bool
    Or  :: Expr Bool -> Expr Bool -> Expr Bool
    Pfx :: Expr ByteString -> Expr ByteString -> Expr Bool -- Prefix
    Eq  :: (Eq a)  => Expr a -> Expr a -> Expr Bool
    Lt  :: (Ord a) => Expr a -> Expr a -> Expr Bool
    Gt  :: (Ord a) => Expr a -> Expr a -> Expr Bool
    Leq :: (Ord a) => Expr a -> Expr a -> Expr Bool
    Geq :: (Ord a) => Expr a -> Expr a -> Expr Bool
    S2I :: ByteString -> Expr Int
    S2B :: ByteString -> Expr Bool
    S2IP:: ByteString -> Expr ByteString 

-- | Evaluate an 'Expr' and return a 'Classification' if the 'Expr' evalutes to true. Otherwise additive identity is returned.
data ClassificationRule a = ClassificationRule (Expr a) Classification

-- | An expression and a route to be returned if the expression evaluates to true.
data RoutingRule a = RoutingRule (Expr a) Route

-- | Routing decision of a request.
data Route =
    Proxy ByteString 
  | TemporaryRedirect ByteString -- issue a 302 redirect
  | PermanentRedirect ByteString -- issue a 301 redirect
  | LocalFile         Status ByteString -- serve a static local file

-- | Classification of a request by a single rule. 
data Classification = Human Int | Robot Int | Malicious Int

-- | Define that an environment must provide a lookup function for variables. 
-- The actual data structure of the environment must be provided by the user. 
class Env y where
  lookup :: ByteString -> y -> ByteString

eval :: (Env env) => env -> Expr a -> a
eval env (I n)   = n
eval env (B b)   = b
eval env (S s)   = s
eval env (IP ip) = ip 
eval env (Var v) = lookup v env
eval env (Not e) = not  (eval env e)
eval env (Add e1 e2)  = (eval env e1) +  (eval env e2)
eval env (Mul e1 e2)  = (eval env e1) *  (eval env e2)
eval env (Eq  e1 e2)  = (eval env e1) == (eval env e2)
eval env (Pfx e1 e2)  = isPrefixOf (eval env e1) (eval env e2)
eval env (Lt  e1 e2)  = (eval env e1) <  (eval env e2)
eval env (Gt  e1 e2)  = (eval env e1) >  (eval env e2) 
eval env (Leq  e1 e2) = (eval env e1) <= (eval env e2)
eval env (Geq  e1 e2) = (eval env e1) >= (eval env e2)

