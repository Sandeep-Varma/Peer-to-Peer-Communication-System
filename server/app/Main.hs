-- Main.hs
{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Data.Aeson (object, (.=), ToJSON, toJSON)
-- import Data.Char(digitToInt)
import Data.IORef
import Control.Monad.IO.Class (liftIO)
-- import qualified Data.Type.Bool as scores
import GHC.Real (Integral(div))
-- import Data.Aeson.KeyMap (difference)
-- import Data.Aeson
-- import qualified Data.ByteString.Lazy as B
-- import Data.Char (ord)
-- import Foreign (new)
-- import Data.ByteString.Builder (lazyByteString)

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
                _ -> do
                    let (u, ip, port) = last user
                    json $ object ["status" .= ("ok" :: String), "user" .= u, "ip" .= ip, "port" .= port]
        post "/game" $ do
            userid <- formParam "user" :: ActionM String

            -- select a random user from the rest of the list
            users <- liftIO $ readIORef usersRef
            let user = filter (\(u, _, _) -> u /= userid) users
            case user of
                [] -> json $ object ["status" .= ("error" :: String)]
                _ -> do
                    let (u, ip, port) = last user
                    json $ object ["status" .= ("ok" :: String), "user" .= u, "ip" .= ip, "port" .= port]

        post "/info" $ do
            user1 <- formParam "user1" :: ActionM String
            user2 <- formParam "user2" :: ActionM String
            score1 <- formParam "score1" :: ActionM String
            score2 <- formParam "score2" :: ActionM String

            -- STORE THE INFO TO DB.txt
            let info1 = user1 ++ " " ++ score1 ++ "\n"
            let info2 = user2 ++ " " ++ score2 ++ "\n"

            liftIO $ appendFile "DB.txt" info1
            liftIO $ appendFile "DB.txt" info2

        get "/getinfo" $ do
            user <- formParam "user" :: ActionM String

            -- read the DB.txt file and return the score of the user by adding all the scores
            content <- liftIO $ readFile "DB.txt"
            let scores = filter (\x -> head x == user) $ map words $ lines content
            let score = sum $ map (\x -> read (last x) :: Int) scores
            json $ object ["score" .= score]

        notFound $ do
            redirect "/"


-- game position dice = do
--     -- find new position
--     let new_position = digitToInt position + digitToInt dice
--     -- check if new position is in the snakes_n_ladders
--     -- if it is, return the new position
--     let snakes_n_ladders = [[1,2],[3,4]] ++ [[new_position,new_position]]
--     -- return the position corresponding to the new position
--     let l = filter (\x -> head x == new_position) snakes_n_ladders
--     return $ last (head l)
