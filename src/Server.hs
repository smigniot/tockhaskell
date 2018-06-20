{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Server where


import Network.Wai
import Network.Wai.Parse
import Network.HTTP.Types.Status
import Game
import System.Directory
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Time (UTCTime, formatTime)
import System.Locale (defaultTimeLocale)
import Control.Concurrent.STM
import GHC.Generics
import Data.Aeson
import Requests
import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.Time
import Database.SQLite.Simple.FromRow

type Storage    = TVar [Game]

-- Needs:
-- ======
-- Store a list of game snapshots
--      in files <GAME_TAG>.properties
-- Store a game initial, current states and event history
--      in file <GAME_TAG>.dat
--	data History = History {
--	    -- Looks like a real-life path of choices can not escape this type
--	    historyInitial :: Game,
--	    historyEvents :: GameEvent,
--	    historyCurrent :: Game  -- Computed at load time, maintained after
--	}
-- Render a game list response
--    from files *.properties
--    let <GAME_TAG>=<GAMEDATE>_<GAMENAME>
type GameList = [GameMini];
data GameMini = GameMini {
    miniName :: String,
    miniCreation :: String,
    miniWest :: String,
    miniNorth :: String,
    miniEast :: String,
    miniSouth :: String,
    miniStatus :: String
}

-- Register a game
--	data GameEvent = Registration {
--	    registrationPosition :: Position,
--	    registrationName :: String
--	} | Exchange {
--	    exchangePosition :: Position,
--	    exchangeCard :: Card
--	} | MoveEvent {
--	    eventPosition :: Position,
--	    eventMove :: Move
--	}

-- Render a game forms response
-- Render a game state response for a position
-- Receive game state updates for a position
--      through websocket ?
-- Exchange a card on a game for a position
-- Play a move for a card for a position


-- | A tock application, curried with the Storage
tockApplication :: Storage -> Request -> Responder -> Answer
tockApplication storage request respond =
    let verb = BS.unpack $ requestMethod request
        rawpath = BS.unpack $ rawPathInfo request in do
            Prelude.putStrLn (verb ++ " " ++ rawpath)
            handleTockRequest storage request respond

-- | Route requests
handleTockRequest :: Storage -> Request -> Responder -> Answer
handleTockRequest storage request respond =
    case (requestMethod request) of
        "POST" -> case (pathInfo request) of
            "games":[]                -> 
                creategame request respond storage
            "game":g:p:[]             -> 
                registeras request g p respond storage
            "game":g:p:"exchange":[]  -> 
                exchange request g p respond storage
            _                         -> notfound respond
        "GET" -> case (pathInfo request) of
            []                        -> index respond
            "index.html":[]           -> index respond
            "games":"latest.json":[]  -> gamelist respond storage
            "js":subpath              -> js respond subpath
            "img":subpath             -> img respond subpath
            "game":gamename:"register.html":[] ->
                joingame gamename respond storage
            "game":gamename:"players.json":[] ->
                playersof gamename respond storage
            "game":gamename:position:"play.html":[] ->
                mainpage gamename position respond storage
            "game":gamename:position:"state.json":[] ->
               gamestate request gamename position respond storage
            _                         -> notfound respond
        _ -> notfound respond

-- | Create a game
creategame :: Request -> Responder -> Storage -> Answer
creategame request respond storage = do
    params <- fmap fst (parseRequestBody lbsBackEnd request)
    case lookup "gamename" params of
        Nothing -> badrequest respond
        Just gamename -> do
            found <- lookupGame (BS.unpack gamename) storage
            case found of
                (game:_) -> badrequest respond
                _ -> do
                    fresh <- newGame (BS.unpack gamename)
                    atomically $ appendGame fresh storage
                    writeGame fresh
                    seeother respond "."

-- | Find a game by name in storage
lookupGame :: String -> Storage -> IO [Game]
lookupGame name storage = do
    games <- atomically $ readTVar storage
    return $ filter (((==) name) . gameName) games

-- | Add a game to storage
appendGame :: Game -> Storage -> STM ()
appendGame game storage = do
    games <- readTVar storage
    writeTVar storage (game:games)

-- | Write a game to disk
writeGame :: Game -> IO ()
writeGame game =
    let name = gameName game
        file = "data/" ++ name ++ ".dat"
        body = show game in
    writeFile file body

-- | List all games, in a "snapshot list" summary
gamelist :: Responder -> Storage -> Answer
gamelist respond games = do
    snapshots <- sumUpGames games
    respond (responseLBS status200 [
        ("Content-Type","application/json;charset=utf-8")
        ] (encode snapshots))

-- | The snapshot type
data GameShot = GameShot {
    shotDate :: UTCTime,
    shotTitle :: String,
    shotPlayers :: [(String,Int)],
    shotPc :: Int
} deriving (Show,Generic)
instance ToJSON GameShot
instance FromJSON GameShot

-- | Build summaries
sumUpGames :: Storage -> IO [GameShot]
sumUpGames storage = do
    games <- atomically $ readTVar storage
    return $ map asShot games
    where
        asShot :: Game -> GameShot
        asShot game = (GameShot
            (gameCreation game)
            (gameName game)
            (buildShotPlayers game)
            (length (gameHistory game)))
        buildShotPlayers game =
                map asShotPlayer (asOrderedPlayers game)
        asOrderedPlayers game = 
            let players = gamePlayers game
                positions = [South,West,North,East]
                at p = (((==) p) . playerPosition) in
                [filter (at p) players | p <- positions]
        asShotPlayer [] = ("",0)
        asShotPlayer (player:ps) = ((playerName player),
            length (filter isParked (playerPawns player)))

-- | Serve the join game page
joingame :: T.Text -> Responder -> Storage -> Answer
joingame gamename respond storage = do
    found <- lookupGame (T.unpack gamename) storage
    case found of
        (game:_) -> 
            respond (responseFile status200 [
                ("Content-Type","text/html;charset=utf-8")
                ] "www/game/name/join.html" Nothing)
        _ -> notfound respond

-- | Serve the current registered player names
playersof :: T.Text -> Responder -> Storage -> Answer
playersof gamename respond storage = do
    found <- lookupGame (T.unpack gamename) storage
    case found of
        (game:_) -> let 
                players = gamePlayers game
                positions = [South,West,North,East]
                at p = (((==) p) . playerPosition)
                ordered = [(p,(filter (at p) players)) | p <- positions]
                pnames = map posAndName ordered
                posAndName (pos,[]) = (pos,"")
                posAndName (pos,(player:ps)) = (pos,(playerName player)) in
            respond (responseLBS status200 [
                ("Content-Type","application/json;charset=utf-8")
                ] (encode pnames))
        _ -> notfound respond


-- | Join the game !
registeras :: Request -> T.Text -> T.Text -> Responder -> Storage -> Answer
registeras request gamename positionStr respond storage = do
    found <- lookupGame (T.unpack gamename) storage
    case found of
        (before:_) -> do
            params <- fmap fst (parseRequestBody lbsBackEnd request)
            case lookup (BS.pack (T.unpack positionStr)) params of
                Nothing -> badrequest respond
                Just playerName -> let 
                    n = BS.unpack playerName
                    p = (read (T.unpack positionStr)) :: Position
                    after = registerPlayer n p before 
                    in do
                        dealt <- if ((not (gameComplete before)) 
                                    && (gameComplete after) )
                                 then dealHands after
                                 else return after
                        atomically $ replaceGame before dealt storage
                        writeGame dealt
                        seeother respond ((show p) ++ "/play.html")
        _ -> notfound respond

-- | Replace the game state in storage
replaceGame :: Game -> Game -> Storage -> STM ()
replaceGame before after storage = do
    games <- readTVar storage
    writeTVar storage (replaceGame' before after games)
    where
        replaceGame' before after [] = []
        replaceGame' before after (g:gs) =
            if g == before
            then after:gs
            else g:(replaceGame' before after gs)

-- | Serve the main page
mainpage :: T.Text -> T.Text -> Responder -> Storage -> Answer
mainpage gamename position respond storage = do
    found <- lookupGame (T.unpack gamename) storage
    case found of
        (game:_) -> respond (responseFile status200 [
            ("Content-Type","text/html;charset=utf-8")
            ] "www/game/name/position/play.html" Nothing)
        _ -> notfound respond

-- | Serve the state for a particular player
gamestate :: Request -> T.Text -> T.Text -> Responder -> Storage -> Answer
gamestate request gamename positionStr respond storage = do
    found <- lookupGame (T.unpack gamename) storage
    case found of
        (game:_) -> 
            let position = read (T.unpack positionStr) :: Position
                state = visibleState position game in
            respond (responseLBS status200 [
                ("Content-Type","application/json;charset=utf-8")
                ] (encode state))
        _ -> notfound respond

data GameSnapshot = GameSnapshot {
    gsPosition :: Position,
    gsGame :: Game
} deriving (Show,Generic)
instance ToJSON GameSnapshot
instance FromJSON GameSnapshot

visibleState :: Position -> Game -> GameSnapshot
visibleState position (Game n c p d h) = 
    let onlyMe position pl@(Player pn ppo ph pe ppa) = 
            if (position == (playerPosition pl))
            then pl
            else (Player pn ppo [] Nothing ppa)
        filtered = map (onlyMe position) p in
        (GameSnapshot position (Game n c filtered [] h))


exchange :: Request -> T.Text -> T.Text -> Responder -> Storage -> Answer
exchange request gamerepr posrepr respond storage = let
    gamename = T.unpack gamerepr
    position = (read (T.unpack posrepr)) :: Position in do
    found <- lookupGame gamename storage
    case found of
        (before:_) -> do
            params <- fmap fst (parseRequestBody lbsBackEnd request)
            let cardValue = lookup "cardValue" params
                cardColor = lookup "cardColor" params in
                case (cardValue,cardColor) of
                    (Nothing,_) -> badrequest respond
                    (_,Nothing) -> badrequest respond
                    (Just cv,Just cc) -> (exchange' before 
                        (Card 
                            (((read . BS.unpack) cv) :: CardValue) 
                            (((read . BS.unpack) cc) :: CardColor) 
                        )
                        position respond storage)
        _ -> notfound respond

exchange' :: Game -> Card -> Position -> Responder -> Storage -> Answer
exchange' before card position respond storage = let 
    player = playerAt before position
    exchanged = setExchanged before player card
    after = if allExchanged exchanged
            then endExchange exchanged
            else exchanged
    state = visibleState position after in do
        atomically $ replaceGame before after storage
        writeGame after
        respond (responseLBS status200 [
            ("Content-Type","application/json;charset=utf-8")
            ] (encode state))


