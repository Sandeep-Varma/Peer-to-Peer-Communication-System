-- Main.hs
{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Data.Aeson (object, (.=), ToJSON, toJSON)
import Data.IORef
import Control.Monad.IO.Class (liftIO)

users = newIORef []

main :: IO ()
main = do
    usersRef <- users
    scotty 3500 $ do
        get "/" $ do
            html "Welcome to Chat Lambda!\n"

        post "/login" $ do
            userid <- formParam "user" :: ActionM String
            ip <- formParam "ip" :: ActionM String
            port <- formParam "port" :: ActionM String

            let user = (userid, ip, port)
            liftIO $ modifyIORef usersRef (++ [user])

            -- print users variable
            -- users <- liftIO $ readIORef usersRef
            -- liftIO $ print users

            -- return a json response {'status': 'ok'}
            json $ object ["status" .= ("ok" :: String)]

        post "/connect" $ do
            userid <- formParam "user" :: ActionM String

            -- return a json response {'status' : 'ok', 'user': 'userid', 'ip': 'ip', 'port': 'port'} for the target user if he is in the list
            -- else return { 'status' : 'error' }

            users <- liftIO $ readIORef usersRef
            let user = filter (\(u, _, _) -> u == userid) users
            case user of
                [] -> json $ object ["status" .= ("error" :: String)]
                [(u, ip, port)] -> json $ object ["status" .= ("ok" :: String), "user" .= u, "ip" .= ip, "port" .= port]
            
        notFound $ do
            redirect "/"
