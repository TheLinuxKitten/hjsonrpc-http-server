{-# LANGUAGE OverloadedStrings #-}

module Network.JsonRpcSrvHttp
  ( FromJsonRpcRequest(..)
  , jsonRpcHttpService
  , runJsonRpcHttpServer
  , runJsonRpcHttpsServer
  , runJsonRpcFastCGIServer
  ) where

import Control.Concurrent (forkOS)
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson.JsonRpc
import Data.Aeson.Types
import Data.ByteString.Lazy (ByteString)
import Data.Monoid ((<>))
import Data.Text (Text)
import Happstack.Server.Monads (setHeaderM)
import Happstack.Server.SimpleHTTP
import Happstack.Server.SimpleHTTPS
    ( TLSConf(..)
    , nullTLSConf
    , simpleHTTPS
    )
import Network.FastCGI (runFastCGIConcurrent')
import Network.CGI
  ( getBodyFPS
  , handleErrors
  , outputError
  , outputFPS
  , requestMethod
  )
import Network.CGI.Monad
import Network.CGI.Protocol (HeaderName(..))
import Network.Constants

class FromJsonRpcRequest a where
  fromJsonRpcRequest' :: JsonRpcMethod -> [Value] -> JsonRpcResp a
  fromJsonRpcRequest' m = fromJsonRpcRequest m . toArrayValue

  fromJsonRpcRequest :: JsonRpcMethod -> Value -> JsonRpcResp a
  fromJsonRpcRequest m = fromJsonRpcRequest' m . fromArrayValue

jsonRpcProcBody :: (FromJsonRpcRequest r)
                => (r -> IO (JsonRpcResp Value))
                -> ByteString
                -> IO ByteString
jsonRpcProcBody fSrv body = case jsonRpcDecodeRequest body of
  Left e -> retResp $ JsonRpcOrphanError JsonRpcV2 (errObj 1 e)
  Right jreq -> case jreq of
    (JsonRpcRequest v m p i) -> case fromJsonRpcRequest m p of
      Left e1 -> retResp $ JsonRpcResponseError v (errObj 2 e1) i
      Right req -> do
        evresp <- fSrv req
        case evresp of
          Left e2 -> retResp $ JsonRpcResponseError v (errObj 3 e2) i
          Right vresp -> retResp $ JsonRpcResponseResult v vresp i
    (JsonRpcNotification v m p) -> error "No respondo notificaciones!!!"
  where
    retResp = return . jsonRpcEncodeResponse
    errObj c e = JsonRpcErrorObject c e Null

-- | Crea servicio HTTP Happstack a partir de un callback que atiende peticiones
jsonRpcHttpService :: (FromJsonRpcRequest r)
                   => (r -> IO (JsonRpcResp Value))
                   -> ServerPartT IO ByteString
jsonRpcHttpService fSrv = msum
  [ do
      method POST
      decodeBody $ defaultBodyPolicy "/tmp" 10000000 1000000 1000000
      uncurry setHeaderM hContentTypeValue
      uncurry setHeaderM hServerValue
      askRq >>= liftIO . readMVar . rqBody
            >>= liftIO . jsonRpcProcBody fSrv . unBody
            >>= ok
  , badRequest "Bad Request"
  ]

-- | Ejecuta servidor HTTP JsonRpc
runJsonRpcHttpServer :: (FromJsonRpcRequest r)
                     => Int     -- ^ Puerto
                     -> (r -> IO (JsonRpcResp Value))   -- ^ Callback para atender las peticiones
                     -> IO ()
runJsonRpcHttpServer portSrv = simpleHTTP nullConf{port=portSrv}
                             . jsonRpcHttpService

-- | Ejecuta servidor HTTPS JsonRpc
runJsonRpcHttpsServer :: (FromJsonRpcRequest r)
                      => Int     -- ^ Puerto
                      -> FilePath   -- ^ Certificado
                      -> FilePath   -- ^ Clave privada
                      -> (r -> IO (JsonRpcResp Value))   -- ^ Callback para atender las peticiones
                      -> IO ()
runJsonRpcHttpsServer port cert key = simpleHTTPS nullTLSConf
                                        { tlsPort = port
                                        , tlsCert = cert
                                        , tlsKey = key
                                        }
                                    . jsonRpcHttpService

-- | Ejecuta aplicacion FastCGI que procesa peticiones JsonRpc de servidor HTTP
runJsonRpcFastCGIServer :: (FromJsonRpcRequest r)
                        => Int      -- ^ Nro de threads iniciales
                        -> (r -> IO (JsonRpcResp Value))   -- ^ Callback para atender las peticiones
                        -> IO ()
runJsonRpcFastCGIServer nTh fSrv =
  runFastCGIConcurrent' forkOS nTh $ handleErrors $ do
    m <- requestMethod
    if m == "POST"
      then do
        uncurry addH hContentTypeValue
        uncurry addH hServerValue
        getBodyFPS >>= liftIO . jsonRpcProcBody fSrv >>= outputFPS
      else outputError 400 "Bad Request" []
  where
    addH k = cgiAddHeader (HeaderName k)

