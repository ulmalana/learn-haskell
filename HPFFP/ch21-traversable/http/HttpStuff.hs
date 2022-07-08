module HttpStuff where

import Data.ByteString.Lazy
import Network.Wreq

urls :: [String]
urls = [ "http://httpbin.com/ip",
         "http://httpbin.com/bytes/5"
       ]

mappingGet :: [IO (Response ByteString)]
mappingGet = map get urls

traversedUrls :: IO [Response ByteString]
traversedUrls = traverse get urls
