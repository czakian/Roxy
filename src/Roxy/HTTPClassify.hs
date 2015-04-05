{-# LANGUAGE OverloadedStrings, RecordWildCards, ScopedTypeVariables #-}

module Roxy.HTTPClassify where 

import Conduit
import Control.Exception (evaluate)
import qualified Data.ByteString.Char8  as BS
import qualified Data.Map               as MP
import qualified Data.List              as LS
import System.Timeout (timeout)
import Network.HTTP.Types (Status)
import Data.Semiring
import Roxy.Types

--- Logical Expression Language For Classifying HTTP Requests ---

-- The strategy for classifying HTTPS requests is to create a small DSL
-- inspecting HTTP request headers which
-- can dynamically evaluate an expression tree in context of an HTTP request 
-- and provide classification of the request in context of the rules. We want
-- to classify a request based on several parameters, such as header values,
-- client IP, bit rate, request rate, etc.
-- Given a classification, the HTTPHandler can define a QOS for the request

-- | Routing decision on a request.
data Route =
    Proxy BS.ByteString 
  | TemporaryRedirect BS.ByteString -- issue a 302 redirect
  | PermanentRedirect BS.ByteString -- issue a 301 redirect
  | LocalFile         Status BS.ByteString -- serve a static local file

-- | Classification of a request by a single rule. 
data Classification = Human Int | Robot Int | Malicious Int

-- | Expression tree which organizes logical quantifiers in context of an environment
data Expr =
    Binop Quantifier Expr Expr
  | UnOp  Quantifier Expr 
  | LookUp BS.ByteString Env Type
  | Null

type Rule = Either ClassificationRule RoutingRule 

-- | Evaluate an 'Expr' and return a 'Classification' if the 'Expr' evalutes to true. Otherwise additive identity is returned.
data ClassificationRule = ClassificationRule Expr Classification

-- | Evaluate
data RoutingRule = RoutingRule Expr Route

-- | Result of evaluating an expression.
data RuleResult = Route | Classification
 
-- | Specialized types for the DSL which allow optimized implementations
-- of comparisons on commonly checked types, such as the client IP.
data Type = I | F | S  | IP -- Int, Float, String, IP

-- | Logical quantifiers supported. Each Quantifier must have an implementation
-- for every type or explicitely exclude that type from evaluation at AST creation.
data Quantifier = AND | OR | NOT | EQ | RANGE | ALL | BEGINSWITH

-- | Holds a mapping of variable names to values. 
-- TODO: decide where we want to infer variable types. Is it at environment creation time, or at run time?
type Env = (RequestLine, Headers, MP.Map BS.ByteString BS.ByteString)

-- Classification Score Card ---

-- | Holds an aggregate view of the sum of the confidence on each classification dimension.
data ScoreCard = ScoreCard {
    humanScore     :: Int -- confidence a human made this request
  , robotScore     :: Int -- confidence a robot made this request
  , maliciousScore :: Int -- confidence that the request is meant to do harm
  }

instance Semiring ScoreCard where
  zero  = ScoreCard 0 0 0
  one   = ScoreCard 1 1 1
  (ScoreCard hw1 rw1 mw1) .+. (ScoreCard hw2 rw2 mw2) = ScoreCard (hw1+hw2) (rw1+rw2) (mw1+mw2)
  (ScoreCard hw1 rw1 mw1) .*. (ScoreCard hw2 rw2 mw2) = ScoreCard (hw1*hw2) (rw1*rw2) (mw1*mw2)

--- Rule Time Bounding ---

-- | Constant defining the maximum run time for any given rule in microseconds.
maxRuleRuntimeMicroseconds :: Int
maxRuleRuntimeMicroseconds = 100

-- | Time bound the running of a classification rule. At a certain point, 
-- we should just serve the request instead of 
timeBoundFn :: (a -> b) -> a -> IO (Maybe b)
timeBoundFn f x = timeout maxRuleRuntimeMicroseconds $ evaluate (f x)

--- Rule Evaluation Logic ---

runRules :: [ClassificationRule] -> [RoutingRule] -> RequestLine -> Headers -> Route
runRules crules rrules reqLine headers = routeRequest env rrules scoreCard 
  where
    env = mkEnv reqLine headers
    scoreCard = classifyRequest () env crules

-- | Build a runtime environment from the request line and headers
mkEnv :: RequestLine -> Headers -> Env
mkEnv requestLine headers = (requestLine, headers, MP.empty)

-- | Evalute the classification rules in context of an HTTP request
-- and generate a 2-tuple of a score card with confidence values classifying the
-- request as human, robot, or malicious and the list of classification results.
classifyRequest :: (Semiring s) => s -> Env -> [ClassificationRule] -> s
classifyRequest scoreCard env [] = scoreCard
classifyRequest scoreCard env (r:rules) = classifyRequest (scoreCard .+. execExpr env r) env rules 

-- | Make a routing decision about a requests
routeRequest :: (Semiring s) => Env -> [RoutingRule] ->  s -> Route
routeRequest env rules scoreCard = Proxy "foo"

execExpr :: (Semiring s) => Env -> ClassificationRule -> s
execExpr env rule = zero --TODO






