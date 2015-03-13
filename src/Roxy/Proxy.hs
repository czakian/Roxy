{-# LANGUAGE OverloadedStrings, RecordWildCards, LambdaCase, DoAndIfThenElse #-}

module Roxy.Proxy
( runRoxy
) where 

import Conduit
import qualified Data.ByteString.Char8  as BS
import qualified Data.Conduit.Binary    as CB
import qualified Data.Conduit.List      as CL
import Data.Conduit.Network (serverSettings, appSource, runTCPServer, ServerSettings)
import Data.Streaming.Network (AppData)
import Roxy.HTTPProxy

-- | Handle a tcp connection. Right now the handler assumes all incoming connections come in
-- as HTTP requests which may then be upgraded to websockets. 
tcpHandler :: AppData -> IO () 
tcpHandler appData = do
  let source = appSource appData
  (source',  requestLine) <- source  $$+  readRequestLine
  (source'', headers)     <- source' $$++ readHeaders
  httpHandler appData source'' requestLine headers

-- | Runs a proxy listening over a TCP socket and forks a lightweight thread for each connection.
runTCPProxy :: ServerSettings -> IO ()
runTCPProxy serverSettings = runTCPServer serverSettings tcpHandler

-- | Kicks off roxy to proxy. 
-- TODO: Add different types sockets to support here (udp, unix domain)
runRoxy :: IO ()
runRoxy = runTCPProxy $ serverSettings 3333 "*" 
