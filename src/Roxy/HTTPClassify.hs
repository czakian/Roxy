{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Roxy.HTTPClassify where 

import Conduit
import Control.Exception (evaluate)
import qualified Data.ByteString.Char8  as BS
import qualified Data.Conduit.Binary    as CB
import qualified Data.Conduit.List      as CL
import qualified Data.Conduit.Network   as CN
import qualified Data.Map               as MP
import Data.Streaming.Network (AppData, appCloseConnection)
import System.Timeout (timeout)

import Roxy.Types
import Roxy.Util


--- Logical Expression Language For Classifying HTTP Requests ---

-- The strategy for classifying HTTPS requests is to create a small DSL
-- inspecting HTTP request headers which
-- can dynamically evaluate an expression tree in context of an HTTP request 
-- and provide classification of the request in context of the rules. We want
-- to classify a request based on several parameters, such as header values,
-- client IP, bit rate, request rate, etc.
-- Given a classification, the HTTPHandler can define a QOS for the request

data Rule =  Rule Expr Bool Classification

data Expr =
    Binop Quantifier Expr Expr
  | UnOp  Quantifier Expr 
  | LookUp BS.ByteString Env Type
  | Null

-- | Classification of a request by a single rule. 
--
data Classification = Human Int | Robot Int | Malicious Int
 
-- | Specialized types for the DSL which allow optimized implementations
-- of comparisons on commonly checked types, such as the client IP.
data Type = I | F | S  | IP -- Int, Float, String, IP

-- | Logical quantifiers supported. Each Quantifier must have an implementation
-- for every type or explicitely exclude that type from evaluation at AST creation.
data Quantifier = AND | OR | NOT | EQ | RANGE | ALL | BEGINSWITH

type Env = MP.Map BS.ByteString BS.ByteString

-- Classification Score Card ---

-- | The score card represents an aggregated view returned to the httpHandler
-- which keeps track of the confidence we have if the request was sent by
-- a human, a robot, or was malicious in nature.
data ScoreCard = ScoreCard {
    humanScore     :: Int -- confidence a human made this request
  , robotScore     :: Int -- confidence a robot made this request
  , maliciousScore :: Int -- confidence that the request is meant to do harm
  }

--- Rule Time Bounding ---

-- | Constant defining the maximum run time for any given rule in microseconds.
maxRuleRuntimeMicroseconds :: Int
maxRuleRuntimeMicroseconds = 100

-- | Time bound the running of a classification rule. At a certain point, 
-- we should just serve the request instead of 
timeBoundFn :: (a -> b) -> a -> IO (Maybe b)
timeBoundFn f x = timeout maxRuleRuntimeMicroseconds $ evaluate (f x)

---- Rule Evaluation Logic ---
--
---- | Evalute the classification rules in context of an HTTP request
---- and generate a score card with confidence values classifying the
---- request as human, robot, or malicious.
--classifyRequest :: [Rule] -> RequestLine -> Headers -> ScoreCard
--classifyRequest rules requestLine headers = mkScoreCard $! runRules rules env
--  where 
--    env = mkEnv requestLine headers
--
--mkEnv :: RequestLine -> Headers -> Env
--mkEnv requestLine headers = -- TODO
--
--mkScoreCard :: [Classification] -> ScoreCard
--mkScoreCard classifications = -- TODO






