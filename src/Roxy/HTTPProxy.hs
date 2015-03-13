{-# LANGUAGE OverloadedStrings, RecordWildCards, LambdaCase, DoAndIfThenElse #-}

module Roxy.HTTPProxy where 

import Conduit
import qualified Data.ByteString.Char8  as BS
import qualified Data.Conduit.Binary    as CB
import qualified Data.Conduit.List      as CL
import qualified Data.Conduit.Network   as CN
import qualified Data.Map               as MP
import Data.Streaming.Network (AppData, appCloseConnection)

import Roxy.Types
import Roxy.Util


-- | Handle an http connection
httpHandler :: AppData -> ResumableStream -> RequestLine -> Headers -> IO ()
httpHandler appData stream requestLine headers = do 
 appCloseConnection appData
  --return ()
-- TODO use some resumable resource again here: see http://www.yesodweb.com/blog/2012/06/conduit-0-5 (look for $$++ since we are resuming)
--  yield statusLine
--  return ()


-- | Reads an HTTP request line as part of a 'Data.Conduit.Source' pipeline.
readRequestLine :: Sink ByteString IO RequestLine
readRequestLine = takeLine


-- | Reads HTTP headers as part of a 'Data.Conduit.Source' pipeline.
readHeaders :: Sink ByteString IO Headers
readHeaders = do
      line <- takeLine
      let empty = BS.empty
      case line of
        ""     -> return MP.empty
        header -> do
          headers <- readHeaders
          return $ insertHeader header headers
  where
    errorString = "Encountered malformed header when attempting to parse the string: "
    -- Strip beginning colon and any whitespace on either end of the string.
    sanitizeHeader val = fst $ BS.spanEnd (==' ') $ BS.dropWhile (==' ') $ BS.dropWhile (==':') val
    -- Insert the parsed header into the headers map. Duplicate values are overwritten. 
    insertHeader header headers = 
      case BS.breakSubstring ":" header of
        (key, val) | BS.null val  -> error $ errorString ++ (show header)
                   | otherwise    -> MP.insert key (sanitizeHeader val) headers
