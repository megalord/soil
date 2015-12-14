{-# LANGUAGE OverloadedStrings #-}
module Network.Soil.Request
  ( jsonHeaders
  , withBody
  , withHeader
  , withHeaders
  , withMethod
  , Request(..)
  ) where

import qualified Data.ByteString.Char8 as BC
import Data.Char (toUpper)
import Network.HTTP.Conduit (Request(..), RequestBody(..))
import Network.HTTP.Types.Header

withMethod :: String -> Request -> Request
withMethod m r = r { method = BC.pack . map toUpper $ m }

withHeader :: Header -> Request -> Request
withHeader h req = req { requestHeaders = h:headers }
  where headers = requestHeaders req

withHeaders :: [Header] -> Request -> Request
withHeaders = foldr (.) id . map withHeader

jsonHeaders :: [Header]
jsonHeaders = [ ("Accept", "application/json")
              , ("Content-Type", "application/json")
              ]

withBody :: String -> Request -> Request
withBody "" r = r
withBody b r = r { requestBody = RequestBodyBS . BC.pack $ b }
