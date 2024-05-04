{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent ( forkIO, newEmptyMVar, putMVar, takeMVar , threadDelay)
import Control.Monad ( replicateM_ , unless, void, when)
import Control.Exception (try, IOException)
import Control.Monad.IO.Class (liftIO)
import System.IO ( Handle, hSetBuffering, stdout, BufferMode(NoBuffering) , IOMode(ReadWriteMode), IOMode(AppendMode), hSeek, SeekMode(SeekFromEnd) , hPutStrLn , hGetLine , hClose, openFile, hPutStr, hIsEOF, hGetContents, hFlush)

import qualified Network.HTTP.Client as Client
import Network.HTTP.Types.URI (urlEncode)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Header (hContentType)
import Network.Socket
import Network.Socket.ByteString (sendAll)
import Data.Function ((&))
import Network.HTTP.Req (http, (/:), req, GET, NoReqBody, lbsResponse, responseBody, defaultHttpConfig, runReq, Req)


import Data.Aeson (object, ToJSON, (.=), toJSON, FromJSON, eitherDecode, decode, parseJSON, withObject, (.:), Options (fieldLabelModifier), encode)
import qualified Data.Aeson.Types as AesonTypes (defaultOptions)
import Data.Aeson.Types (genericParseJSON, toJSON, genericToJSON)
import Data.IORef
import qualified Data.ByteString.Lazy as B
import GHC.Generics (Generic)

import Data.ByteString.Char8 (pack)
import qualified Data.ByteString.Lazy.Char8 as LBSC
import Safe (atMay)

import Web.Scotty
import GHC.IO.IOMode (IOMode(ReadWriteMode))
import GHC.IO.Handle (hSeek)
-- import Data.ByteString (hGetLine, writeFile)

import System.Environment   
import Data.List 

data Config = Config
    { userid :: String
    , port :: Int
    , apiport :: Int
    , ip :: String
    } deriving (Show, Generic)

data Request = Request
    { method :: String
    , user :: String
    , status1 :: Int
    } deriving (Show, Generic)

data SocketInfo = SocketInfo
  { ip2 :: String
  , port2 :: String
  , user2 :: String
  , status2 :: String
  } deriving (Show, Generic)

instance FromJSON Config
instance FromJSON Request where
    parseJSON = genericParseJSON AesonTypes.defaultOptions
                    { 
                        fieldLabelModifier = \field -> case field of
                            "ip1" -> "ip"
                            "port1" -> "port"
                            "status1" -> "status"
                            _ -> field
                    }

instance ToJSON Request where
    toJSON = genericToJSON AesonTypes.defaultOptions
                    { 
                        fieldLabelModifier = \field -> case field of
                            "ip1" -> "ip"
                            "port1" -> "port"
                            "status1" -> "status"
                            _ -> field
                    }

instance FromJSON SocketInfo where
    parseJSON = genericParseJSON AesonTypes.defaultOptions
                    { 
                        fieldLabelModifier = \field -> case field of
                            "ip2" -> "ip"
                            "port2" -> "port"
                            "user2" -> "user"
                            "status2" -> "status"
                            _ -> field
                    }

main :: IO ()
main = do
    args <- getArgs
    -- configpath <- getLine
    let configpath = ("../" ++ head args )
    content <- B.readFile configpath
    let config = eitherDecode content :: Either String Config
    case config of
        Left err -> putStrLn err
        Right config -> do
            login config
            hSetBuffering stdout NoBuffering

            -------  empty the requests file
            let requestsFileName = "../tmp_" ++ userid config ++ "/requests.txt"
            writeFile requestsFileName ""
            -------

            done <- newEmptyMVar
            -- forkIO (handleInterface config >> putMVar done ())
            forkIO (listenForConnections (userid config) (show (port config)))
            forkIO ( apihandler (userid config) (apiport config) >> putMVar done () )
            takeMVar done
    putStrLn "\nMain thread finished."

login :: Config -> IO ()
login config = runReq defaultHttpConfig $ do
    let message = "The id is: " ++ show (userid config) ++ " and the port is: " ++ show (port config)
    liftIO $ putStrLn message
    let username = userid config

    manager <- liftIO $ Client.newManager tlsManagerSettings
    initialRequest <- liftIO $ Client.parseRequest "http://0.0.0.0/login"
    let info = [(pack "user", pack username), (pack "ip", pack (ip config)), (pack "port", pack (show (port config)))]
    let request = Client.urlEncodedBody info $ initialRequest {
                                    Client.host = "0.0.0.0",
                                    Client.port = 3500,
                                    Client.path = "/login",
                                    Client.method = "POST",
                                    Client.requestHeaders
                                    = [(hContentType, "application/x-www-form-urlencoded")]
                                }
    response <- liftIO $ Client.httpLbs request manager
    liftIO $ print $ Client.responseBody response
    liftIO $ print "Logged in"


-- =========================== ESTABLISHING CONNECTIONS ================================
establishConnection :: String -> String -> String -> String -> IO ()
establishConnection uid username game choice = runReq defaultHttpConfig $ do
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
    liftIO $ handleResponse uid username game choice (Client.responseBody response)

handleResponse :: String -> String -> String -> String -> B.ByteString -> IO ()
handleResponse me him game choice responseBody = do
    case decode responseBody of
        Just socketInfo -> do
            let host = ip2 socketInfo
            let portNum = port2 socketInfo
            putStrLn $ "Connecting to " ++ host ++ ":" ++ portNum
            addrInfos <- getAddrInfo Nothing (Just host) (Just portNum)
            let serverAddr = head addrInfos
            sock <- socket (addrFamily serverAddr) Stream defaultProtocol
            connect sock (addrAddress serverAddr)
            putStrLn "Connected!"

            handle <- socketToHandle sock ReadWriteMode
            hSetBuffering handle NoBuffering

            -- send the user id to the server
            hPutStrLn handle (if game == "1" then "PLAY" ++ choice ++ me else me)

            let fileName = "../tmp_" ++ me ++ "/" ++ (if game == "1" then "game_" else "" ) ++ him ++ ".txt"

            -- make the file empty
            writeFile fileName (if game == "1" then "PLAY" ++ (if choice == "1" then "0\n" else "1\n") else "")

            let reqfileName = "../tmp_" ++ me ++ "/" ++ (if game == "1" then "play.txt" else "requests.txt")
            appendFile reqfileName (him ++ "\n")

            handle_get <- openFile fileName ReadWriteMode

            void $ forkIO $ connGet me him handle handle_get
            void $ forkIO $ connSend handle handle_get

        Nothing -> putStrLn "Failed to parse JSON response"



-- =========================== LISTENING FOR CONNECTIONS ================================


listenForConnections :: String -> String -> IO ()
listenForConnections uid port = withSocketsDo $ do
    addrInfos <- getAddrInfo 
                 (Just (defaultHints {addrFlags = [AI_PASSIVE]})) 
                 Nothing 
                 (Just port )
    let serverAddr = head addrInfos
    sock <- socket (addrFamily serverAddr) Stream defaultProtocol
    bind sock (addrAddress serverAddr)
    listen sock 1
    putStrLn ("Listening on port " ++ port)
    mainLoop uid sock


mainLoop :: String -> Socket -> IO ()
mainLoop uid sock = do
    conn <- accept sock
    handle <- socketToHandle (fst conn) ReadWriteMode
    hSetBuffering handle NoBuffering

    -- obtaining the user id of the requester
    -- create new file <user>.txt
    line <- hGetLine handle

    if take 4 line == "PLAY" then do
        let user = drop 5 line
        let fileName = "../tmp_" ++ uid ++ "/game_" ++ user ++ ".txt"
        writeFile fileName ( (take 5 line) ++ "\n" )
        let reqfileName = "../tmp_" ++ uid ++ "/play.txt"
        appendFile reqfileName (user ++ "\n")

        handle_get <- openFile fileName ReadWriteMode
        forkIO $ connGet uid user handle handle_get
        forkIO $ connSend handle handle_get
    else do
        let user = takeWhile (/= ' ') line
        putStrLn $ "User " ++ user ++ " connected"
        let fileName = "../tmp_" ++ uid ++ "/" ++ user ++ ".txt"
        writeFile fileName ""
        let reqfileName = "../tmp_" ++ uid ++ "/requests.txt"
        appendFile reqfileName (user ++ "\n")

        handle_get <- openFile fileName ReadWriteMode
        forkIO $ connGet uid user handle handle_get
        forkIO $ connSend handle handle_get

    mainLoop uid sock

connGet :: String -> String -> Handle -> Handle -> IO ()
connGet myid uid handle fileHandle = do
    line <- hGetLine handle
    print ("get " ++ show line)

    when (take 4 line == "FILE") $ do
        filename <- hGetLine handle
        print ("get file: " ++ show filename)

        let loop = do
                chunk <- hGetLine handle
                if chunk == "EOF"
                    then return ""
                    else do
                        rest <- loop
                        return ((chunk ++ "\n") ++ rest)
        contents <- loop
        writeFile ("../tmp_" ++ myid ++ "/" ++ filename) contents

        hSeek fileHandle SeekFromEnd 0
        hPutStrLn fileHandle ("FILEGET_" ++ filename)
        hFlush fileHandle
        threadDelay 500000 -- sleep for 0.5 second
        connGet myid uid handle fileHandle

    hSeek fileHandle SeekFromEnd 0
    hPutStrLn fileHandle ("GET_" ++ line)
    hFlush fileHandle
    threadDelay 500000 -- sleep for 0.5 second
    connGet myid uid handle fileHandle

connSend :: Handle -> Handle -> IO ()
connSend handle fileHandle = do
    result <- try $ hGetLine fileHandle :: IO (Either IOException String)
    case result of
        Left _ -> do
            -- If an error occurred (e.g., EOF), sleep for 0.5 second and retry
            -- print the error message
            threadDelay 500000 -- sleep for 0.5 second
        Right line -> do
            -- If a line was read successfully, send it to the server after removing "SEND_"
            -- If it was a GET message , ignore it
            print ("tmp send " ++ show line)
            when (take 5 line == "SEND_") $ do
                print ("send " ++ show line)
                hPutStrLn handle (drop 5 line)
                hFlush handle
            when (take 5 line == "FILE_") $ do
                print ("file " ++ show line)
                let filepath = drop 5 line
                hPutStrLn handle "FILE"
                hFlush handle

                -- need just the filename ignoring the path, i.e trim all the"/" in the filename 
                let filename = reverse $ takeWhile (/= '/') $ reverse filepath
                hPutStrLn handle filename
                hFlush handle

                contents <- readFile filepath
                hPutStrLn handle contents
                hFlush handle

                hPutStrLn handle "EOF"
                hFlush handle

    threadDelay 500000 -- sleep for 0.5 second
    connSend handle fileHandle


-- ================================================ THE API HANDLING THREAD ==========================

getlines :: String -> IO [String]
getlines filename = do
    contents <- readFile filename
    return $ lines contents



apihandler :: String -> Int -> IO ()
apihandler uid apiport = do
    scotty apiport $ do
        post "/connect" $ do
            userid <- formParam "user" :: ActionM String
            liftIO $ forkIO (establishConnection uid userid "0" "0")
            json $ object ["status" .= ("ok" :: String)]
        get "/requests" $ do
            result <- liftIO $ getlines ("../tmp_" ++ uid ++ "/requests.txt")
            case result of
                [] -> json $ object ["status" .= ("error" :: String)]
                x -> json $ object ["status" .= ("ok" :: String), "data" .= x]

        post "/append" $ do 
            message <- formParam "data" :: ActionM String
            userid <- formParam "user" :: ActionM String
            liftIO $ appendFile ("../tmp_" ++ uid ++ "/" ++ userid ++ ".txt") ("SEND_" ++ message ++ "\n")
        
        -- at this point, just return all lines of the file...
        -- future : keep track of offsets and return ret new lines etc etc
        post "/read" $ do
            userid <- formParam "user" :: ActionM String
            result <- liftIO $ getlines ("../tmp_" ++ uid ++ "/" ++ userid ++ ".txt")
            case result of
                [] -> json $ object ["status" .= ("error" :: String)]
                x -> json $ object ["status" .= ("ok" :: String), "data" .= x]
        post "/play" $ do
            userid <- formParam "user" :: ActionM String
            choice <- formParam "choice" :: ActionM String
            liftIO $ forkIO (establishConnection uid userid "1" choice)
            json $ object ["status" .= ("ok" :: String)]

        notFound $ do
            json $ object ["status" .= ("error" :: String)]
