{-# LANGUAGE OverloadedStrings #-}
module Main where


import System.Environment (getEnv)
import Game
import Requests
import Storage
import Robots
import Network.Wai
import Network.Wai.Parse
import Network.Wai.Handler.Warp
import Network.HTTP.Types.Status
import Control.Applicative
import qualified Database.SQLite.Simple as S
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Key as K
import Data.List (intercalate, sort)
import Database.SQLite.Simple.FromRow
import Control.Monad (forM_)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Pretty
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Char8 as SBS
import qualified Data.ByteString.Lazy.Char8 as LBS

-- | Tock server application
main = do
    port <- read <$> getEnv "PORT"
    connection <- connect
    putStrLn ("Running Tock server on port " ++ (show port))
    runSettings (tockSettings port) (tracedTockApplication connection)

tockSettings :: Port -> Settings
tockSettings port = setFdCacheDuration 0 $ setPort port defaultSettings 


-- | A tock application, curried with the Connection.
--   This method logs the incoming request before delegating to
--   tockApplication
tracedTockApplication :: S.Connection -> Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
tracedTockApplication connection request respond = let
    conv = T.unpack . TE.decodeUtf8
    verb = conv $ requestMethod request
    rawpath = conv $ rawPathInfo request in do
        Prelude.putStrLn (verb ++ " " ++ rawpath)
        tockApplication connection request respond

-- | A tock application, curried with the Connection.
--   This method is a route list.
tockApplication :: S.Connection -> Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
tockApplication connection request respond =
    case (requestMethod request) of
        "POST" -> case (pathInfo request) of
            "games":[]       -> creategame connection request respond
            "game":date:name:"join":[] -> do
                                before <- gread date name
                                joingame request before respond updateEta
            "game":date:name:position:"exchange":[]
                             -> do
                                before <- gread date name
                                exchange request before position respond updateEta
            "game":date:name:position:"play":[]
                             -> do
                                before <- gread date name
                                playmove request before position respond updateEta
            _                -> notfound respond
        "GET" -> case (pathInfo request) of
            []                -> statichtml respond "index.html"
            "index.html":[]   -> statichtml respond "index.html"
            "js":resource:[]  -> js respond [resource]
            "css":resource:[] -> css respond [resource]
            "games.csv":[]    -> serveGameList connection respond
            "game":date:name:position:"play.html":[] 
                              -> statichtml respond "game/date/name/position/play.html" 
            "game":date:name:position:"status.json":[]
                              -> do
                                before <- gread date name
                                statusJson before position respond
            "game":date:name:position:"history":startpos:[]
                              -> do
                                before <- gread date name
                                historyJson before (read (T.unpack startpos)) position respond
            "img":resource:[] -> svg respond [resource]
            _                 -> notfound respond
        _ -> notfound respond
    where
        gread :: T.Text -> T.Text -> IO Game
        gread date name = readGame connection (T.unpack date) (T.unpack name)
        updateEta a humanb = do
            b <- playRobots humanb
            updateGame connection a b

serveGameList :: S.Connection -> (Response -> IO ResponseReceived) -> IO ResponseReceived
serveGameList conn respond = do
    rows <- listGames conn
    let csv = intercalate "\n" lines
        lines = "name,creation,west,north,east,south,pc":(map toLine rows)
        toLine :: [String] -> String
        toLine row = intercalate "," row
    respond (responseLBS status200 [
        ("Content-Type","text/csv;charset=UTF8")
        ] (LBS.fromStrict (TE.encodeUtf8 (T.pack csv))))

info = putStrLn

creategame :: S.Connection -> Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
creategame conn request respond = do
    params <- fmap fst (parseRequestBody lbsBackEnd request)
    case lookup "gamename" params of
        Nothing -> badrequest respond
        Just "" -> badrequest respond
        Just gamename -> do
            fresh <- newGame (T.unpack (TE.decodeUtf8 gamename))
            createGame conn fresh
            info ("Created game " ++ (gameName fresh))
            seeother respond "."
                    
joingame :: Request -> Game -> (Response -> IO ResponseReceived) -> (Game -> Game -> IO ()) -> IO ResponseReceived
joingame request before@(Game n c p d h) respond update = do
    params <- fmap fst (parseRequestBody lbsBackEnd request)
    case lookup "position" params of
        Nothing -> badrequest respond
        Just "" -> badrequest respond
        Just positionstr ->
            case lookup positionstr params of
                Nothing -> badrequest respond
                Just "" -> badrequest respond
                Just playername -> join2nd (read (T.unpack (TE.decodeUtf8 positionstr)) :: Position) (T.unpack (TE.decodeUtf8 playername))
    where
        join2nd position name =
            let registered = registerPlayer name position before
                in do
                    after <- if ((not (gameComplete before)) 
                                && (gameComplete registered) )
                             then dealHands registered
                             else return registered
                    update before after 
                    info ("Joined " ++ n ++ " at " ++ (show position) ++ " as " ++ name)
                    seeother respond ((show position) ++ "/play.html")

historyJson :: Game -> Int -> T.Text -> (Response -> IO ResponseReceived) -> IO ResponseReceived
historyJson game@(Game n c p d h) startpos position respond = 
    respond (responseLBS status200 [
        ("Content-Type","application/json;charset=utf-8")
        ] jsonStr)
    where
        jsonStr = JSON.encode json
        json = drop startpos (reverse h)

statusJson :: Game -> T.Text -> (Response -> IO ResponseReceived) -> IO ResponseReceived
statusJson game@(Game n c p d h) position respond = 
    respond (responseLBS status200 [
        ("Content-Type","application/json;charset=utf-8")
        ] jsonStr)
    where
        jsonStr = JSON.encode json
        json = JSON.object [ "creation" JSON..= c, "title" JSON..= n,
            "pc" JSON..= pc, "hand" JSON..= hand, "board" JSON..= board,
            "players" JSON..= handCounts, "actions" JSON..= actions,
            "historyCount" JSON..= (length h),
            "lastfour" JSON..= (reverse (take 4 h)) ]
        pc = stateOf game
        me = playerAt game (read (T.unpack position))
        hand = playerHand me
        board = JSON.object (map asPawns (gamePlayers game))
        asPawns player = (K.fromString (show (playerPosition player)))
            JSON..= (playerPawns player)
        actions = nubs (sort manymoves)
        nubs [] = []
        nubs [p] = [p]
        nubs (a:b:rest) = if a == b
            then nubs (a:rest)
            else a:(nubs (b:rest))
        manymoves = movesFor me game
        handCounts = JSON.object (map asNameAndCount (gamePlayers game))
        asNameAndCount pl@(Player n p h e pw) = (K.fromString (show p)
            ) JSON..= (JSON.object ["name" JSON..= n,
                "hand" JSON..= (length h),
                "hasexchange" JSON..= (hasex pl) ])
        hasex p = case (playerExchange p) of
            Nothing -> False
            _ -> True


instance JSON.ToJSON Card where
    toJSON (Card v c) = JSON.object [ "cardValue" JSON..= (show v),
        "cardColor" JSON..= (show c) ]
instance JSON.ToJSON Pawn where
    toJSON (Out) = JSON.object [ "type" JSON..= ("Out"::String) ]
    toJSON (OnBase p) = JSON.object [ "type" JSON..= ("OnBase"::String),
        "position" JSON..= p ]
    toJSON (Pawn p) = JSON.object [ "type" JSON..= ("Pawn"::String),
        "position" JSON..= p ]
    toJSON (Parked p) = JSON.object [ "type" JSON..= ("Parked"::String),
        "position" JSON..= p ]
instance JSON.ToJSON History where
    toJSON (Registering) = JSON.object [ "type" JSON..= ("Registering"::String) ]
    toJSON (Registration pos name) = JSON.object [ "type" JSON..= ("Registration"::String),
        "position" JSON..= pos, "name" JSON..= name ]
    toJSON (Exchanging) = JSON.object [ "type" JSON..= ("Exchanging"::String) ]
    toJSON (Exchange pos card) = JSON.object [ "type" JSON..= ("Exchange"::String),
        "position" JSON..= pos ]
        -- , "card" JSON..= card ]
    toJSON (Playing p) = JSON.object [ "type" JSON..= ("Playing"::String),
        "player" JSON..= p ]
    toJSON (MoveEvent pos move) = JSON.object [ "type" JSON..= ("Move"::String),
        "player" JSON..= pos, "move" JSON..= move ]
    toJSON (WinEvent) = JSON.object [ "type" JSON..= ("Win"::String) ]
instance JSON.ToJSON Position where
    toJSON (West)  = "West"
    toJSON (North) = "North"
    toJSON (East)  = "East"
    toJSON (South) = "South"
instance JSON.ToJSON Move where
    toJSON move@(Withdraw card) = JSON.object [ 
        "type" JSON..= ("Withdraw"::String),
        "shown" JSON..= (show move),
        "card" JSON..= card
        ]
    toJSON move@(Enter card) = JSON.object [ 
        "type" JSON..= ("Enter"::String),
        "shown" JSON..= (show move),
        "card" JSON..= card
        ]
    toJSON move@(Run card from to) = JSON.object [ 
        "type" JSON..= ("Run"::String),
        "shown" JSON..= (show move),
        "card" JSON..= card,
        "from" JSON..= from,
        "to" JSON..=   to
        ]
    toJSON move@(Warp card from to) = JSON.object [ 
        "type" JSON..= ("Warp"::String),
        "shown" JSON..= (show move),
        "card" JSON..= card,
        "from" JSON..= from,
        "to" JSON..=   to
        ]
    toJSON move@(Swap card source target) = JSON.object [ 
        "type" JSON..= ("Swap"::String),
        "shown" JSON..= (show move),
        "card" JSON..= card,
        "source" JSON..= source,
        "target" JSON..= target
        ]
    toJSON move@(SevenMove card hops) = JSON.object [ 
        "type" JSON..= ("Seven"::String),
        "shown" JSON..= (show move),
        "card" JSON..= card,
        "hops" JSON..= hops
        ]

boardFor game@(Game n c p d h) = H.pre textboard H.! A.title (H.toValue shortened) where
    textboard = H.text $ T.pack textboard'
    textboard' = textBoardFor game
    shortened = shortenedPositions p

shortenedPositions :: [Player] -> String
shortenedPositions players = let
    shortened = intercalate "," (map pshort players)
    pshort player = (show (playerPosition player)) ++ "|" ++ (pshort' player)
    pshort' player = intercalate "|" (map posshort (playerPawns player))
    posshort (Pawn p) = show p
    posshort (OnBase p) = "*"++(show p)
    posshort (Parked p) = show p
    posshort p = show p
    in shortened

textBoardFor game@(Game n c p d h) = 
    intercalate "\n" $ map modified page where
    page = ["",
            "                4041424748                ",
            "                39  43  49                ",
            "                38  44  50                ",
            "                37  45  51                ",
            "                36  46  52                ",
            "                35      53                ",
            "                34      54                ",
            "                33      55                ",
            "242526272829303132      565758596061626364",
            "23                                      65",
            "1819202122                      7069686766",
            "17                                      71",
            "161514131211100908      807978777675747372",
            "                07      81                ",
            "                06      82                ",
            "                05      83                ",
            "                04  94  84                ",
            "                03  93  85                ",
            "                02  92  86                ",
            "                01  91  87                ",
            "                0095908988                "
            ]
    modified (a:b:xs) = (morph (a:b:[])) ++ (modified xs)
    modified _ = ""
    morph s@(a:b:[]) = if (a `elem` ['0'..'9']) && (b `elem` ['0'..'9'])
        then morph' (read s :: Int) s
        else s
    morph other = other
    morph' n s = case lookup n pos2label of
        Nothing -> s
        Just label -> label
    pos2label :: [(Int,String)]
    pos2label = foldr (++) [] (map pos2label' p)
    pos2label' player = pos2label'' (taggedPawns player)
    taggedPawns player = zip (map (prepend (playerPosition player)) [1..4]) (playerPawns player) -- (N1,pawn)
    prepend pos n = (head (show pos)):(show n)
    pos2label'' :: [(String,Pawn)] -> [(Int,String)]
    pos2label'' [] = []
    pos2label'' ((label,Out):xs) = pos2label'' xs
    pos2label'' ((label,pawn):xs) = (pawnPosition pawn, label):pos2label'' xs

actionsFor game@(Game n c p d h) position =
    case stateOf game of
        Exchanging     -> exchanging
        Exchange _ _   -> exchanging
        Playing _      -> playorwait (currentTurn game)
        MoveEvent _ _  -> playorwait (currentTurn game)
        _ -> H.div ""
    where
        playorwait p = if p /= position
            then H.div $ H.text $ T.pack $ (show p) ++ " is playing"
            else moveChoices
        moveChoices =
            H.ul $ forM_ moves asmove
        thePlayer = playerAt game position
        manymoves = movesFor thePlayer game
        moves = nubs (sort manymoves)
        nubs [] = []
        nubs [p] = [p]
        nubs (a:b:rest) = if a == b
            then nubs (a:rest)
            else a:(nubs (b:rest))
        exchanging =
            H.ul $ forM_ cards asexchange
        asexchange card =
            H.li $ H.form H.! A.method "POST" H.! A.action "exchange" $ do
                H.input H.! A.type_ "hidden" H.! A.name "card" H.! A.value (H.toValue (show card))
                H.input H.! A.type_ "submit" H.! A.class_ "btn btn-xs" H.! A.value (ex card)
        cards = playerHand thePlayer
        ex card = H.toValue ("Exchange " ++ (face card))
        asmove m =
            H.li $ H.form H.! A.method "POST" H.! A.action "play" $ do
                H.input H.! A.type_ "hidden" H.! A.name "move" H.! A.value (H.toValue (show m))
                H.input H.! A.type_ "hidden" H.! A.name "cells" H.! A.value (H.toValue (cellsInvolved thePlayer m game))
                H.input H.! A.type_ "submit" H.! A.class_ "btn btn-xs" H.! A.value (H.toValue (moveText m))
        moveText (Withdraw c) = "Withdraw "++(face c)
        moveText (Enter c) = "Enter with "++(face c)
        moveText (Run c p1 p2) = "Move "++(postext p1)++" to "++(postext p2)++" with "++(face c)
        moveText (Warp c p1 p2) = "Warp from "++(postext p1)++" to "++(postext p2)++" with "++(face c)
        moveText (Swap c p1 p2) = "Swap "++(postext p1)++" with "++(postext p2)++" using "++(face c)
        moveText killer@(SevenMove c hops) = "Seven "++(face c)++" "++(intercalate " " (map hopText hops))
        postext (Pawn p) = (show ( adj p)) ++ "(" ++ (show p) ++ ")"
        postext (OnBase p) = (show ( adj p)) ++ "(" ++ (show p) ++ ")"
        postext (Parked p) = (show ( adj p)) ++ "(" ++ (show p) ++ ")"
        postext p = show p
        adj p = let n = p `mod` 24 in
            if n == 23
                then 19
                else n
        hopText (from,to) = (postext from)++">"++(postext to)

cellsInvolved :: Player -> Move -> Game -> String
cellsInvolved player move before = let
    after = applyMove player move before
    status = shortenedPositions . gamePlayers
    in (status before) ++ ":" ++ (status after)

handFor game@(Game n c p d h) position =
    H.ul $ forM_ cards shown
    where
        cards = playerHand (playerAt game position)
        shown card = H.li $ H.text $ T.pack $ face card

face :: Card -> String
face card@(Card value color) = 
    (show value) ++ " of " ++ (show color)
    
exchange :: Request -> Game -> T.Text -> (Response -> IO ResponseReceived) -> (Game -> Game -> IO ()) -> IO ResponseReceived
exchange request before@(Game n c p d h) positionstr respond update =
    case stateOf before of
        (Exchanging)  -> run
        (Exchange _ _) -> run
        _             -> badrequest respond
    where
        run = do
            params <- fmap fst (parseRequestBody lbsBackEnd request)
            case lookup "card" params of
                Nothing -> badrequest respond
                Just cardstr -> exchange' (read (T.unpack (TE.decodeUtf8 cardstr)) :: Card)
        position = read (T.unpack positionstr) :: Position
        player = playerAt before position
        exchange' card = do
            info ("Exchanged " ++ (show card) ++ " at " ++ (show position))
            update before after
            seeother respond "play.html"
            where
                after = exchangedGame before player card

    
playmove :: Request -> Game -> T.Text -> (Response -> IO ResponseReceived) -> (Game -> Game -> IO ()) -> IO ResponseReceived
playmove request before@(Game n c p d h) positionstr respond update =
    if position == (currentTurn before)
        then run
        else badrequest respond
    where
        position = read (T.unpack positionstr) :: Position
        player = playerAt before position
        moves = movesFor player before
        run = do
            params <- fmap fst (parseRequestBody lbsBackEnd request)
            case lookup "move" params of
                Nothing -> badrequest2 respond "No move provided"
                Just movestr -> playmove' (read (T.unpack (TE.decodeUtf8 movestr)) :: Move)
        playmove' move = 
            if elem move moves
                then apply move
                else badrequest2 respond "Move not allowed"
        apply move = do
            after <- if (handsEmpty applied)
                then dealHands applied
                else return applied
            update before after
            -- TODO: apply robots - Timmy,Yoshi
            seeother respond "play.html"
            where
                modified = applyMove player move before
                applied = historize move modified
                handsEmpty game@(Game n c p d h) = 
                    [True,True,True,True] == map handEmpty p
                handEmpty player = null (playerHand player)
        historize move game@(Game n c p d h) =
            (Game n c p d ((MoveEvent position move):h))


