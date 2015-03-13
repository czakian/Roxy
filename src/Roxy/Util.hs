{-# LANGUAGE OverloadedStrings, RecordWildCards, LambdaCase, DoAndIfThenElse #-}

module Roxy.Util where 

import           Conduit
import qualified Data.ByteString.Char8  as BS
import qualified Data.Conduit.Binary    as CB
import qualified Data.Conduit.List      as CL
import Data.Streaming.Network (AppData, appSockAddr)
import Network.Socket (SockAddr)


-- | Take a single line from a Conduit stream
-- Strips the trailing newline any any trailing carriage return characters.
takeLine :: Sink BS.ByteString IO BS.ByteString
takeLine = do
    let newline = 10
    line <- CB.takeWhile (/= newline) =$ CL.consume
    CB.drop 1 -- drop the newline
    return $ BS.takeWhile (/= '\r') $ BS.concat line


-- | Get socket address from AppData
getSockAddr :: AppData -> SockAddr
getSockAddr appData = appSockAddr appData
