{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Servant
import Network.Wai.Handler.Warp
import qualified Network.Wai.Middleware.RequestLogger as Log
import Network.Wai
import Prelude
import qualified Serve as N

main :: IO ()
main = run 8081 . Log.logStdout $ app

server :: Server N.ServiceApis
server = N.server

api :: Proxy N.ServiceApis
api = Proxy

app :: Application
app = serve api server
