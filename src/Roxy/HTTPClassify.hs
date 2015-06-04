{-# LANGUAGE OverloadedStrings, RecordWildCards, ScopedTypeVariables #-}

module Roxy.HTTPClassify where 

import Conduit
import Control.Exception (evaluate)
import qualified Data.Map as MP
import System.Timeout (timeout)
import Network.HTTP.Types (Status, status404)
import Data.Semiring
import Roxy.Types
import Roxy.HTTPInspector
import Data.ByteString.Char8 (pack)

--- Logical Expression Language For Classifying HTTP Requests ---

-- | An expression and a route to be returned if the expression evaluates to true.
data RoutingRule a = RoutingRule (Expr a) Route

-- | Routing decision for a request.
data Route =
    Proxy ByteString 
  | TemporaryRedirect ByteString -- issue a 302 redirect
  | PermanentRedirect ByteString -- issue a 301 redirect
  | LocalFile         Status ByteString -- serve a static local file

--TODO: the types don't quite work out here yet.
-- requestline, headers, custom variables. 
data HTTPEnv = HTTPEnv RequestLine Headers (MP.Map ByteString ByteString)

instance Env HTTPEnv where
  lookup var env = I 5
  extend var val env = env


--- Rule Evaluation Logic ---

-- | Build a runtime environment from the request line and headers
mkEnv :: RequestLine -> Headers -> HTTPEnv
mkEnv requestLine headers = HTTPEnv requestLine headers MP.empty

-- | Make a routing decision about a requests
routeRequest :: (Env env) => env -> [RoutingRule a] -> Route
routeRequest env [] = LocalFile status404 "errors/404.html"
--routeRequest env x:xs = 
--  case (eval env expr) of 
--    (B False) -> routeRequest env xs
--    _ -> route

--- Error Pages ---

