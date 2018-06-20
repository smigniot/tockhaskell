{-# LANGUAGE OverloadedStrings #-}
module Storage where


import Game
import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.Time
import Database.SQLite.Simple.FromRow
import qualified Data.ByteString.Lazy.Char8 as LBS
import Blaze.ByteString.Builder
import Data.Time (UTCTime, formatTime)


-- | Connect to a database, creating the table if needed
connectTo :: String -> IO Connection
connectTo path = open path

-- | Connect to a database, creating the table if needed
connect :: IO Connection
connect = do
    conn <- open "data/tock.db"
    execute_ conn "CREATE TABLE IF NOT EXISTS tockgame (name TEXT, creation TEXT, west TEXT, north TEXT, east TEXT, south TEXT, pc TEXT, data TEXT)"
    return conn

utcToColumn :: UTCTime -> String
utcToColumn u = LBS.unpack $ toLazyByteString $ utcTimeToBuilder u

playerColumns :: [Player] -> [String]
playerColumns players = let
    nameat [] _ = ""
    nameat (pl:ps) pos = if (playerPosition pl) == pos
        then (playerName pl)
        else nameat ps pos
    in
        map (nameat players) [West,North,East,South]

-- | Store a game for the first time
createGame :: Connection -> Game -> IO ()
createGame conn game@(Game nm cr p d h) = let
    -- probably useless and constant array of empty strings
    [w,n,e,s] = playerColumns p
    t = show game 
    pc = "New"
    c = utcToColumn cr in
    execute conn "INSERT INTO tockgame (name,creation,west,north,east,south,pc,data) VALUES(?,?,?,?,?,?,?,?)" [nm,c,w,n,e,s,pc,t]
    

-- | Update a game
updateGame :: Connection -> Game -> Game -> IO ()
updateGame conn before after = let
    [w,n,e,s] = playerColumns (gamePlayers after)
    h = gameHistory after
    nm1 = gameName before
    nm2 = gameName after 
    t = show after 
    pc = if null h
        then "Registering"
        else (show . head) h
    c1 = utcToColumn (gameCreation before)
    c2 = utcToColumn (gameCreation after) in
    execute conn "UPDATE tockgame SET name=?, creation=?, west=?, north=?, east=?, south=?, pc=?, data=? WHERE name=? AND creation=?" [nm2,c2,w,n,e,s,pc,t,nm1,c1]


    
-- | List games, each one being name,creation,west,north,east,south
listGames :: Connection -> IO [[String]]
listGames conn =
    query_ conn "SELECT name,creation,west,north,east,south,pc FROM tockgame"

-- | Read a game by creation and name
readGame :: Connection -> String -> String -> IO Game
readGame conn creation name = do
    datas <- query conn "SELECT data FROM tockgame WHERE creation=? AND name=?" [creation,name]
    return (read (head (head datas)) :: Game)

