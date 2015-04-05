{-# LANGUAGE OverloadedStrings #-}

module Roxy.Types where 

import           Conduit
import qualified Data.ByteString.Char8  as BS
import qualified Data.Map               as MP

type Headers         = MP.Map BS.ByteString BS.ByteString
type RequestLine     = MP.Map BS.ByteString BS.ByteString
type ByteString      = BS.ByteString
type ResumableStream = ResumableSource IO BS.ByteString

