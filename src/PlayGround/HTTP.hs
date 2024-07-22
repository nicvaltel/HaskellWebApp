module PlayGround.HTTP where


import Reexport
import Network.HTTP.Client (ManagerSettings, Manager, newManager, parseRequest, Request (..), RequestBody (..))
import Network.HTTP.Client.TLS (tlsManagerSettings)




runTest :: IO ()
runTest = do
  manager <- newManager tlsManagerSettings
  initReq <- parseRequest "http://example.com/path/to/resource"
  let req = initReq {
      method = "POST"
    , requestHeaders = [
            ("x-header-1","something")
          , ("x-header-2","hello")
          ]
    , requestBody = RequestBodyLBS "{\"hello\":1}"
    , queryString = "param1=hello&param2=world"
    , cookieJar = Nothing
  }
  pure ()
