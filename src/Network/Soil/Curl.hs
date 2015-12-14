module Network.Soil.Curl
  ( curl
  ) where

import Data.ByteString.Char8 (unpack)
import Data.CaseInsensitive (original)
import Network.HTTP.Conduit (RequestBody(..))
import Network.Soil.Request
import Text.Printf (printf)

headers :: Request -> String
headers = foldr ((++) . header) "" . requestHeaders 
  where header (k, v) = printf "-H \"%s: %s\"" (unpack . original $ k) (unpack v)

prefix :: String -> String -> String
prefix pfx rest = if null rest then "" else pfx ++ " " ++ rest

method' :: Request -> String
method' = prefix "-X" . unpack . method

mapReq :: [Request -> String] -> Request -> [String]
mapReq fs req = map ($ req) fs

url :: Request -> String
url = foldr (++) "" . mapReq fs
  where protocol req = if secure req then "https://" else "http://"
        fs = [protocol, unpack . host, \_ -> ":", show . port, unpack . path, unpack . queryString]

extract :: RequestBody -> String
extract (RequestBodyBS b) = unpack b
extract _ = ""

wrap :: String -> String -> String
wrap w = (w ++) . (++ w)

wrap' :: String -> String -> String
wrap' w s = if null s then "" else wrap w s

body :: Request -> String
body = prefix "--data" . wrap' "'" . extract . requestBody

curl :: Request -> String
curl =  unwords . filter (not . null) . mapReq [headers, url, method', body]
