{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import System.IO
import qualified Network.HTTP.Client as Client
import Network.HTTP.Types.URI (urlEncode)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Header (hContentType)
-- import Network.HTTP.Req
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, decode, parseJSON, withObject, (.:))
import qualified Data.ByteString.Lazy as LBS
import Network.Socket
import Network.Socket.ByteString (sendAll)
import Data.Function ((&))
import Network.HTTP.Req (http, (/:), req, GET, NoReqBody, lbsResponse, responseBody, defaultHttpConfig, runReq)
-- import qualified Network.HTTP.Req as Req
import Data.ByteString.Char8 (pack)
import qualified Data.ByteString.Lazy.Char8 as LBSC
import GHC.Generics (Generic)
import Data.Aeson
import GHC.Generics

-- data FormParam = FormParam
--     { user :: String
--     , ip :: String
--     , port :: String
--     } deriving (Show, Generic)

main :: IO ()
main = do
    print "Hello!"
    print "Welcome to the "
    print "Please enter your username and press Enter"
    username <- getLine
    login username
    print "If you want to wait for incoming connections, enter 'listen' and press Enter"
    print "If you want to establish a connection, enter 'connect' and press Enter"
    input <- liftIO getLine
    case input of
        "listen" -> listenForConnections
        "connect" -> establishConnection
        _ -> print "Invalid input"


login :: String -> IO ()
login username = runReq defaultHttpConfig $ do
    manager <- liftIO $ Client.newManager tlsManagerSettings
    initialRequest <- liftIO $ Client.parseRequest "http://0.0.0.0/login"
    let info = [(pack "user", pack username), (pack "ip", pack "0.0.0.0"), (pack "port", pack "4500")]
    let request = Client.urlEncodedBody info $ initialRequest {
                                    Client.host = "0.0.0.0",
                                    Client.port = 3500,
                                    Client.path = "/login",
                                    Client.method = "POST",
                                    Client.requestHeaders
                                    = [(hContentType, "application/x-www-form-urlencoded")]
                                }
    response <- liftIO $ Client.httpLbs request manager
    liftIO $ print "Logged in"

establishConnection :: IO ()
establishConnection = runReq defaultHttpConfig $ do
    liftIO $ print "Please enter the username of the person you want to connect to and press Enter"
    username <- liftIO getLine
    manager <- liftIO $ Client.newManager tlsManagerSettings
    initialRequest <- liftIO $ Client.parseRequest "http://0.0.0.0/login"
    let info = [(pack "user", pack username)]
    let request = Client.urlEncodedBody info $ initialRequest {
                                    Client.host = "0.0.0.0",
                                    Client.port = 3500,
                                    Client.path = "/connect",
                                    Client.method = "POST",
                                    Client.requestHeaders
                                    = [(hContentType, "application/x-www-form-urlencoded")]
                                }
    response <- liftIO $ Client.httpLbs request manager
    -- liftIO $ print $ Client.responseBody response
    liftIO $ handleResponse (Client.responseBody response)

handleResponse :: LBS.ByteString -> IO ()
handleResponse responseBody = do
    case decode responseBody of
        Just socketInfo -> do
            let host = (ip socketInfo)
            let portNum = (port socketInfo)
            putStrLn $ "Connecting to " ++ host ++ ":" ++ portNum
            addrInfos <- getAddrInfo Nothing (Just host) (Just portNum)
            let serverAddr = head addrInfos
            sock <- socket (addrFamily serverAddr) Stream defaultProtocol
            connect sock (addrAddress serverAddr)
            putStrLn "Connected!"
            handle <- socketToHandle sock ReadWriteMode
            hSetBuffering handle NoBuffering
            runConn handle False
        Nothing -> putStrLn "Failed to parse JSON response"

data SocketInfo = SocketInfo
  { ip :: String
  , port :: String
  , user :: String
  , status :: String
  } deriving (Show, Generic)

instance FromJSON SocketInfo
    -- parseJSON = withObject "SocketInfo" $ \obj -> do
    --     host <- obj .: "host"
    --     port <- obj .: "port"
    --     return $ SocketInfo host port


listenForConnections :: IO ()
listenForConnections = withSocketsDo $ do
    addrInfos <- getAddrInfo 
                 (Just (defaultHints {addrFlags = [AI_PASSIVE]})) 
                 Nothing 
                 (Just "4500")
    let serverAddr = head addrInfos
    sock <- socket (addrFamily serverAddr) Stream defaultProtocol
    bind sock (addrAddress serverAddr)
    listen sock 1
    putStrLn "Listening on port 4500"
    mainLoop sock

mainLoop :: Socket -> IO ()
mainLoop sock = do
    conn <- accept sock
    handle <- socketToHandle (fst conn) ReadWriteMode
    hSetBuffering handle NoBuffering
    runConn handle True
    mainLoop sock

-- runConn :: handle -> Bool -> IO ()
runConn handle sendFirstMsg = do
    -- hPutStrLn handle "Hello, Client!"
    -- print "Sent message to Client"
    if sendFirstMsg
        then do
            -- read from terminal
            input <- getLine
            -- send to client
            hPutStrLn handle input
            -- read from client
            line <- hGetLine handle
            -- write to terminal
            putStrLn line
        else do
            -- read from client
            line <- hGetLine handle
            -- write to terminal
            putStrLn line
            -- read from terminal
            input <- getLine
            -- send to client
            hPutStrLn handle input
    -- repeat
    runConn handle sendFirstMsg
    -- -- close the handle
    -- hClose handle