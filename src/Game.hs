module Game where


import Journey
import Data.List (nub, sort)
import Data.Time (UTCTime, getCurrentTime)
import Shuffle (shuffle)
import Debug.Trace (trace)


-- | A game has a distinguishing name and creation date,
--   a deck of cards under iteration and 0 to 4 players
data Game = Game {
    gameName :: String,
    gameCreation :: UTCTime,
    gamePlayers :: [Player],
    gameDeck :: [Card],
    gameHistory :: [History]
} deriving (Eq,Show,Read)

data History = Registering | Registration {
    registrationPosition :: Position,
    registrationName :: String
} | Exchanging | Exchange {
    exchangePosition :: Position,
    exchangeCard :: Card
} | Playing {
    firstPlayer :: Position
} | MoveEvent {
    eventPosition :: Position,
    eventMove :: Move
} | WinEvent deriving (Eq,Show,Read)

-- | A Player has a name and a position, 0 to 5 cards in hand,
--   potentially a chosen card for exchange and four pawns
data Player = Player {
    playerName :: String,
    playerPosition :: Position,
    playerHand :: [Card],
    playerExchange :: Maybe Card,
    playerPawns :: [Pawn]
} deriving (Eq,Show,Read)


-- | The positions are cardinal directions. No color is involved
--   in the position definition, but only at rendering level
data Position = North | South | West | East
    deriving (Eq,Show,Read)


-- | A card i.e. a value and a color
data Card = Card {
    cardValue :: CardValue,
    cardColor :: CardColor
} deriving (Eq,Ord,Show,Read)

-- | Straightforward
data CardValue = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine
    | Ten | Jack | Queen | King
    deriving (Eq,Ord,Show,Read)

-- | Straightforward
data CardColor = Spades | Hearts | Diamonds | Clubs
    deriving (Eq,Ord,Show,Read)


-- | A pawn may either be out of the board, just out standing on its base,
--   normally moving or already parked in its own color lane
data Pawn = Out | OnBase {
    pawnPosition :: Int
} | Pawn {
    pawnPosition :: Int
} | Parked {
    pawnPosition :: Int
} deriving (Eq,Ord,Show,Read)


-- | The full 52 cards deck
fullDeck :: [Card]
fullDeck = [ Card value color |
    value <- [Ace,Two,Three,Four,Five,Six,Seven,Eight,Nine,Ten,
              Jack,Queen,King],
    color <- [Spades,Hearts,Diamonds,Clubs] ]

-- | Return a newly re-shuffled full deck
shuffleDeck :: IO [Card]
shuffleDeck = shuffle fullDeck

-- | Return a new game with the provided title and the current system time as
--   creation time
newGame :: String -> IO Game
newGame title = do
    now <- getCurrentTime
    return (Game title now [] [] [])

-- | Register a player with provided name and position into the game.
--   Any player at same position gets out
registerPlayer :: String -> Position -> Game -> Game
registerPlayer name position (Game n c p d h) = let
    occupied = filter (((==) position) . playerPosition) p
    other = filter (((/=) position) . playerPosition) p
    player = if null occupied
             then (Player name position [] Nothing [Out,Out,Out,Out])
             else head occupied
    in (Game n c
        (player:other)
        d ((Registration position name):h))

-- | True when the game has 4 players, False otherwise
gameComplete :: Game -> Bool
gameComplete = ((==) 4) . length . gamePlayers

-- | Return a refilled and shuffled deck if needed i.e. if already empty
refillDeck :: [Card] -> IO [Card]
refillDeck [] = shuffleDeck
refillDeck deck = return deck

-- | 5 when deck is full, 4 otherwise.
--   Comes in handy (lol) as size of hands are 5,4 and 4 before refilling
handLength :: [Card] -> Int
handLength deck = if (52 == (length deck))
    then 5
    else 4

-- | Perform a distribution of hands. Provided a count for hand size
--   provide four hands of exactly this size, plus the remaining deck
hands :: Int -> [Card] -> ([Card],[Card],[Card],[Card],[Card])
hands n deck = (
    (take n deck),
    (take n (drop n deck)),
    (take n (drop (n*2) deck)),
    (take n (drop (n*3) deck)),
    (drop (n*4) deck))

-- | Given four players and four hands, replace the player hands
fillPlayers :: [Player] -> [[Card]] -> [Player]
fillPlayers [] _ = []
fillPlayers (p:ps) (h:hs) = (Player 
    (playerName p) 
    (playerPosition p) 
    h (playerExchange p)
    (playerPawns p)
    ) : (fillPlayers ps hs)
fillPlayers _ _ = []

-- | Perform a distribution of hands and apply to players. 
--   May trigger refilling the deck
dealHands :: Game -> IO Game
dealHands game = do
    deck <- refillDeck (gameDeck game)
    let n = handLength deck
        (a,b,c,d,rest) = hands n deck
        filledPlayers = fillPlayers (gamePlayers game) [a,b,c,d] in do
            return (Game (gameName game)
                (gameCreation game)
                filledPlayers
                rest
                (Exchanging:(gameHistory game)))

-- | Mark a player's card for future exchange with partner without failing
setExchanged :: Game -> Player -> Card -> Game
setExchanged game@(Game n c p d h) player card = (Game n c
    ((setExchanged' player card):(
            filter ((/=) player) (gamePlayers game)))
    d ((Exchange (playerPosition player) card):h))

-- | Just put a Just card in the player exchange slot
setExchanged' :: Player -> Card -> Player
setExchanged' player card = (Player
    (playerName player) 
    (playerPosition player) 
    (playerHand player)
    (Just card)
    (playerPawns player))

-- | True if all players exchange slot is filled
allExchanged :: Game -> Bool
allExchanged game = (4 == (length (filter f (gamePlayers game)))) where
    f player = case (playerExchange player) of
        Nothing -> False
        _ -> True

-- | Perform the final exchange phase, putting each player's exchanged card
--   into the partner's hand while removing it from its own.
endExchange :: Game -> Game
endExchange (Game n c p d h) = (Game n c
    (endExchange' (groupPlayers p)) d (step:h))
    where
        step = Playing (beginnerPlayer h)
        beginnerPlayer ((Playing p):hs) = next p
        beginnerPlayer (_:hs) = beginnerPlayer hs
        beginnerPlayer [] = South
        next South = West
        next West = North
        next North = East
        next East = South

exchangedGame game player card = let
    ex = setExchanged game player card
    in if allExchanged ex
        then endExchange ex
        else ex


-- | Regroup players by team i.e. by position pairs
groupPlayers :: [Player] -> [Player]
groupPlayers players = groupPlayers' players [] []
groupPlayers' [] ns we = ns ++ we
groupPlayers' (p:ps) ns we = case (playerPosition p) of
    North -> groupPlayers' ps (p:ns) we
    South -> groupPlayers' ps (p:ns) we
    West -> groupPlayers' ps ns (p:we)
    East -> groupPlayers' ps ns (p:we)

-- | Finish the exchange
endExchange' :: [Player] -> [Player]
endExchange' (p1:p2:p3:p4:[]) = [
    (exchanged' p1 p2),
    (exchanged' p2 p1),
    (exchanged' p3 p4),
    (exchanged' p4 p3) ]
endExchange' _ = []

-- | Make a card switch
exchanged' :: Player -> Player -> Player
exchanged' player partner = (Player
    (playerName player)
    (playerPosition player)
    (incoming:[card | card <- (playerHand player), card /= given])
    Nothing
    (playerPawns player)) where
        given = exchangedOrFirst player
        incoming = exchangedOrFirst partner

-- | Always return a card for exchange. The regularly chosen one is taken, else
--   if not defined the first is taken
exchangedOrFirst :: Player -> Card
exchangedOrFirst player = case (playerExchange player) of
    Nothing -> head (playerHand player)
    Just card -> card

movesFor :: Player -> Game -> [Move]
movesFor player game =
    let movers = allMovers game player
        moveStacks = [ (movesEnter player movers),
                       (movesForward game player movers),
                       (movesEight player movers),
                       (movesJacks game player movers),
                       (movesSeven game player)
                     ]
        moves = foldr (++) [] moveStacks in
    if (null moves)
    then [Withdraw card | card <- (playerHand player)]
    else moves

data Move = Withdraw {
    withdrawCard :: Card
} | Enter {
    enterCard :: Card
} | Run {
    runCard :: Card,
    pawnBefore :: Pawn,
    pawnAfter :: Pawn
} | Warp {
    warpCard :: Card,
    pawnBefore :: Pawn,
    pawnAfter :: Pawn
} | Swap {
    swapCard :: Card,
    pawnSource :: Pawn,
    pawnTarget :: Pawn
} | SevenMove {
    sevenMoveCard :: Card,
    sevenHops :: [(Pawn,Pawn)]
} deriving (Eq,Ord,Show,Read)


data Killer = Killer {
    sevenCard :: Card,
    beforeKill :: Game,
    afterKill :: Game
}

movesEnter :: Player -> [Pawn] -> [Move]
movesEnter player movers = if (mayEnter movers False)
    then [Enter card | card <- (aceKings (playerHand player))]
    else []

mayEnter :: [Pawn] -> Bool -> Bool
mayEnter [] b = b
mayEnter ((OnBase n):ps) _ = False
mayEnter (Out:ps) _ = mayEnter ps True
mayEnter (_:ps) b = mayEnter ps b
        
aceKings :: [Card] -> [Card]
aceKings [] = []
aceKings ( ace@(Card Ace _): cs) = ace:(aceKings cs)
aceKings (king@(Card King _):cs) = king:(aceKings cs)
aceKings (c:cs) = aceKings cs
        
movesForward :: Game -> Player -> [Pawn] -> [Move]
movesForward game player movers =
    let positionOrPartner = meOrPartner game (playerPosition player)
        runningAs = playerAt game positionOrPartner 
        runningPawns = movingPawns movers
        runs = cardRuns (playerHand player) in 
        foldr (++) [] [(computeRuns pawn card count game runningAs) |
                pawn <- runningPawns, (card,count) <- runs]


computeRuns :: Pawn -> Card -> Int -> Game -> Player -> [Move]
computeRuns pawn card count game player =
    let pos = pawnPosition pawn
        runs = journeys pos count in
        achievingRuns runs game card pawn player

achievingRuns :: [[Int]] -> Game -> Card -> Pawn -> Player -> [Move]
achievingRuns [] _ _ _ _ = []
achievingRuns (path:ps) game card pawn player = 
    let stoppers = nonJumpablePawns path game player
        forbidden = (parkingsForbidden player) ++ (wouldSelfKill player)
        finalPos = last path
        rest = achievingRuns ps game card pawn player in
        if ((null stoppers) && (not (elem finalPos forbidden)))
        then (Run card pawn (pawnOrParked finalPos)):rest
        else rest

wouldSelfKill :: Player -> [Int]
wouldSelfKill player = 
    let nonOut = filter ((/=)Out) (playerPawns player) in
        map pawnPosition nonOut

parkingsForbidden :: Player -> [Int]
parkingsForbidden player = 
    parkingsForbiddenFor (playerPosition player)

parkingsForbiddenFor :: Position -> [Int]
parkingsForbiddenFor position = 
    let notPosition = filter ((/=) position) in
        foldr (++) [] (map parkingsFor 
            (notPosition [North,South,West,East]))

parkingsFor :: Position -> [Int]
parkingsFor West   = [19..22]
parkingsFor North  = [43..46]
parkingsFor East   = [67..70]
parkingsFor South  = [91..94]

baseFor :: Position -> Int
baseFor West  = 24
baseFor North = 48
baseFor East  = 72
baseFor South = 0

stateOf :: Game -> History
stateOf game@(Game n c p d h) = noreg h where
    noreg ((Registration p n):hs) = noreg hs
    noreg (h:hs) = h
    noreg [] = Registering


nonJumpablePawns :: [Int] -> Game -> Player -> [Pawn]
nonJumpablePawns path game player =
    --FIXME: may return unaccurate empty lists
    let positionsAtRisk = tail path in
        --basedOrParked game positionsAtRisk
        basedOrParkedOrMe game player positionsAtRisk

basedOrParkedOrMe :: Game -> Player -> [Int] -> [Pawn]
basedOrParkedOrMe game player positions = 
    let maybeStoppers = filter (isBasedOrParkedOrMe player) (allPawns game) in
        inPositions positions maybeStoppers

allPawns :: Game -> [Pawn]
allPawns game = foldr (++) [] (map playerPawns (gamePlayers game))

isBasedOrParkedOrMe :: Player -> Pawn -> Bool
isBasedOrParkedOrMe _ pawn@(OnBase _) = True
isBasedOrParkedOrMe _ pawn@(Parked _) = True
isBasedOrParkedOrMe _ Out = False
isBasedOrParkedOrMe player pawn = pawn `elem` (playerPawns player)

inPositions :: [Int] -> [Pawn] -> [Pawn]
inPositions positions [] = []
inPositions positions (p:ps) =
    let pos = pawnPosition p
        rest = inPositions positions ps in
        if (elem pos positions)
        then p:rest
        else rest 

pawnOrParked :: Int -> Pawn
pawnOrParked finalPos =
    if (isParking finalPos)
    then Parked finalPos
    else Pawn finalPos

movingPawns :: [Pawn] -> [Pawn]
movingPawns = filter ((/=) (Out))

cardRuns :: [Card] -> [(Card,Int)]
cardRuns [] = []
cardRuns (card@(Card Ace _):cs) = (card,1):(card,14):(cardRuns cs)
cardRuns (card@(Card Two _):cs) = (card,2):(cardRuns cs)
cardRuns (card@(Card Three _):cs) = (card,3):(cardRuns cs)
cardRuns (card@(Card Four _):cs) = (card,(-4)):(cardRuns cs)
cardRuns (card@(Card Five _):cs) = (card,5):(cardRuns cs)
cardRuns (card@(Card Six _):cs) = (card,6):(cardRuns cs)
cardRuns (card@(Card Eight _):cs) = (card,8):(cardRuns cs)
cardRuns (card@(Card Nine _):cs) = (card,9):(cardRuns cs)
cardRuns (card@(Card Ten _):cs) = (card,10):(cardRuns cs)
cardRuns (card@(Card Queen _):cs) = (card,12):(cardRuns cs)
cardRuns (card@(Card King _):cs) = (card,13):(cardRuns cs)
cardRuns (_:cs) = cardRuns cs

playerAt :: Game -> Position -> Player
playerAt game position =
    head (filter (((==) position) . playerPosition) (gamePlayers game))

movesEight :: Player -> [Pawn] -> [Move]
movesEight player movers =
    let eightAceKings = warpCards (playerHand player)
        pawns = onEights movers 
        destinations = otherEights (map pawnPosition pawns) in
        [Warp height pawn (Pawn position) |
                height <- eightAceKings,
                pawn <- pawns,
                position <- destinations]

warpCards :: [Card] -> [Card]
warpCards = filter (isWarp . cardValue) where
    isWarp Eight = True
    isWarp Ace = True
    isWarp King = True
    isWarp _ = False

onEights :: [Pawn] -> [Pawn]
onEights pawns = filter onHeight pawns
    where
        onHeight (Pawn 8) = True
        onHeight (Pawn 32) = True
        onHeight (Pawn 56) = True
        onHeight (Pawn 80) = True
        onHeight _ = False

otherEights :: [Int] -> [Int]
otherEights taken = filter (\n -> not (elem n taken)) [8,32,56,80]

movesJacks :: Game -> Player -> [Pawn] -> [Move]
movesJacks game player movers =
    let jacks = jacksOnly (playerHand player)
        sources = filter notParkedNorOut movers
        destinations = notBasedNorParkedNorOut game in
        foldr (++) [] [ jackExchanges jack sources destinations |
            jack <- jacks ]
        
jackExchanges :: Card -> [Pawn] -> [Pawn] -> [Move]
jackExchanges jack sources targets = 
    [(Swap jack source target) |
        source <- sources,
        target <- targets,
        target /= source]
        
notParkedNorOut :: Pawn -> Bool
notParkedNorOut (Parked _) = False
notParkedNorOut Out = False
notParkedNorOut _ = True

jacksOnly :: [Card] -> [Card]
jacksOnly = filter (((==) Jack) . cardValue)

notBasedNorParkedNorOut :: Game -> [Pawn]
notBasedNorParkedNorOut game = filter valid (allPawns game)
    where
        valid Out = False
        valid (OnBase _) = False
        valid (Parked _) = False
        valid _ = True

movesSeven :: Game -> Player -> [Move]
movesSeven game player =
    let position = playerPosition player
        sevens = filter (((==) Seven) . cardValue) (playerHand player) in
        if (null sevens)
        then []
        else [(asSevenMove (Killer seven game future)) |
                future <- sevenBranches 7 position [game],
                seven <- sevens]

asSevenMove :: Killer -> Move
asSevenMove (Killer seven game future) = 
    (SevenMove seven (ashops (zip (gamePlayers game) (gamePlayers future))))
    where ashops [] = []
          ashops ((p1,p2):ps) = (ashops' (sortedPawns p1,sortedPawns p2))++(ashops ps)
          sortedPawns player = sort (playerPawns player)
          ashops' (p1:p1s,p2:p2s) = if p1 /= p2
              then (p1,p2):(ashops' (p1s,p2s))
              else (ashops' (p1s,p2s))
          ashops' _ = []


-- The maximum combinations is majored by
-- * 4 pawns max each time
-- * Having at most 5 outcomes at a whole
--   because only one may branch into parking
-- so 5^7 max
-- <= 78125
sevenBranches :: Int -> Position -> [Game] -> [Game]
sevenBranches 0 position games = games
sevenBranches n position games =
    let makeStep = sevenStepOrWin position
        futures = map makeStep games
        flattened = foldr (++) [] futures in
        sevenBranches (n-1) position flattened

-- Sept rentrant :D
--       when allParked and allPartnerParked, game is won
sevenStepOrWin :: Position -> Game -> [Game]
sevenStepOrWin position game =
    let player = playerAt game position
        partner = playerAt game (oppositeDirection position)
        finished = allParked player && allParked partner
        in if finished
            then [game]
            else sevenOneStep position game

sevenOneStep :: Position -> Game -> [Game]
sevenOneStep position game =
    let player = playerAt game position
        moveables = mineOrPartners game player
        positionOrPartner = meOrPartner game position
        temptable = foldr (++) [] (map nextPlusOnes moveables)
        notblocked = filter (dontCollide game player) temptable
        feasible = filter (dontKillself moveables) notblocked
        valid = filter (notInOthersParking positionOrPartner) feasible in 
        -- don't park anywhere
        asSevenBranches game valid
        -- XXX: transform feasibles into branches
        --   ie: [X] remove impossible by OnBase or Parked
        --   ie: [X] remove impossible by self-or-partnerIamActingAs-kill
        --   ie: add killed pawns to accumulator
        --   ie: mutate game to impact players' pawns ?

notInOthersParking :: Position -> (Pawn,Pawn) -> Bool
notInOthersParking position (before,after) =
    let forbidden = parkingsForbiddenFor position
        afterpos = pawnPosition after in
        not (elem afterpos forbidden)

asSevenBranches :: Game -> [(Pawn,Pawn)] -> [Game]
asSevenBranches _ [] = []
asSevenBranches game ((before,after):ps) =
    (moveKill before after game):(
     asSevenBranches game ps)

moveKill :: Pawn -> Pawn -> Game -> Game
moveKill before after game@(Game n c p d h) = (Game 
    n c (moveKill' before after p) d h)

moveKill' :: Pawn -> Pawn -> [Player] -> [Player]
moveKill' _ _ [] = []
moveKill' before after ((Player n p h e pawns):ps) =
    let target = pawnPosition after
        filtered = movePawn before after (killAt target pawns) in
        (Player n p h e filtered):(moveKill' before after ps)

killAt :: Int -> [Pawn] -> [Pawn]
killAt target pawns =
    map (targetToOut target) pawns 

targetToOut :: Int -> Pawn -> Pawn
targetToOut _ pawn@(OnBase _) = pawn
targetToOut _ pawn@(Parked _) = pawn
targetToOut _ Out = Out
targetToOut pos1 pawn@(Pawn pos2) =
    if pos1 == pos2
    then Out
    else pawn

movePawn _ _ [] = []
movePawn before after (p:ps) =
    if before == p
    then after:ps
    else p:(movePawn before after ps)

dontKillself :: [Pawn] -> (Pawn,Pawn) -> Bool
dontKillself pawnsBefore (before,after) =
    let afterPos = pawnPosition after
        impacted = filter (((==) afterPos) . pawnPosition) pawnsBefore in 
        -- Pour chaque couple avant apres je vais filtrer
        -- les pions bougeables par le passe qui sont en position finale
        -- du mouvement futur ..
        -- .. et esperer qu'il n'y en a pas.
        null impacted

dontCollide :: Game -> Player -> (Pawn,Pawn) -> Bool
dontCollide game player (before,after) =
    -- LULZ: can you collide your 'before self' when stepping once ?
    let stoppers = filter (isBasedOrParkedOrMe player) (allPawns game)
        afterPos = pawnPosition after
        stopperHit = filter (((==) afterPos) . pawnPosition) stoppers in 
        null stopperHit
        

nextPlusOnes :: Pawn -> [(Pawn,Pawn)]
nextPlusOnes pawn =
    let position = pawnPosition pawn
        list = nextPositions position in
        [(pawn,(pawnRebased i pawn)) | i<-list]

pawnRebased :: Int -> Pawn -> Pawn
pawnRebased _ Out = Out
pawnRebased n pawn@(Parked _) = Parked n
pawnRebased n pawn@(OnBase base) = if n == base
    then pawn
    else (Pawn n)
pawnRebased n pawn = if elem n parkings
    then (Parked n)
    else (Pawn n)

allParked :: Player -> Bool
allParked player =
    let pawns = playerPawns player
        parked = filter isParked pawns in
        (length parked) == 4

isParked :: Pawn -> Bool
isParked (Parked _) = True
isParked _ = False

oppositeDirection :: Position -> Position
oppositeDirection North = South
oppositeDirection South = North
oppositeDirection West = East
oppositeDirection East = West

allMovers :: Game -> Player -> [Pawn]
allMovers game player =
    if allParked player
    then partnerPawns player game
    else playerPawns player

meOrPartner :: Game -> Position -> Position
meOrPartner game position =
    let me = playerAt game position
        opposite = oppositeDirection position
        in if allParked me
            then opposite
            else position

mineOrPartners :: Game -> Player -> [Pawn]
mineOrPartners game player =
    if allParked player
    then movingPawns (partnerPawns player game)
    else movingPawns (playerPawns player)

partnerPawns :: Player -> Game -> [Pawn]
partnerPawns player game =
    let position = playerPosition player
        opposite = oppositeDirection position
        allPlayers = gamePlayers game
        partners = filter (((==) opposite) . playerPosition) allPlayers
        partner = head partners in
        playerPawns partner

applyMove :: Player -> Move -> Game -> Game
applyMove player (Withdraw card) game = filterCard card game
applyMove player (Enter card) game =
    let deposited = filterCard card game
        initialPosition = playerPosition player
        position = if allParked player
            then oppositeDirection initialPosition
            else initialPosition
        entered = enterPawn' position deposited in 
        entered
applyMove player (Swap card source target) game = 
    let g1 = filterCard card game
        g2 = moveAllPawns source (Pawn 256) g1
        g3 = moveAllPawns target (nobase source) g2
        g4 = moveAllPawns (Pawn 256) (nobase target) g3
        in g4
applyMove player (Run card before after) game = 
    applyMoveRW card before after game
applyMove player (Warp card before after) game = 
    applyMoveRW card before after game
applyMove player (SevenMove card hops) game@(Game n c p d h) = 
    filterCard card (Game n c (map (makeHops hops) p) d h) where
    makeHops hops player@(Player n po h e pa) = Player n po h e (map (hopPawn hops) pa)
    hopPawn _ Out = Out
    hopPawn [] pawn = pawn
    hopPawn ((a,b):ps) pawn = if a == pawn
        then b
        else hopPawn ps pawn
applyMoveRW card before after game = 
    let deposited = killAt (pawnPosition after) (filterCard card game)
        killAt n game@(Game nm c p d h) = 
            Game nm c (map (kill2 n) p) d h
        kill2 n player@(Player nm po h e pa) = 
            Player nm po h e (map (kill3 n) pa)
        kill3 a p@(Pawn b) = if a == b
            then Out
            else p
        kill3 _ p = p
        in moveAllPawns before after deposited


nobase :: Pawn -> Pawn
nobase (OnBase n) = Pawn n
nobase pawn = pawn

enterPawn' :: Position -> Game -> Game
enterPawn' position game@(Game n c p d h) = let
    base = baseFor position
    kill (Player pn po ph pe pa) = 
        Player pn po ph pe (map kill2 pa)
    kill2 pawn@(Pawn pos) = if pos == base
        then Out
        else pawn
    kill2 x = x
    enterif player@(Player pn po ph pe pa) = if po == position
        then enterif2 player
        else player
    enterif2 player@(Player pn po ph pe pa) = 
        Player pn po ph pe (oneOutIn pa)
    oneOutIn [] = []
    oneOutIn (Out:ps) = (OnBase base):ps
    oneOutIn (p:ps) = p:(oneOutIn ps)
    in Game n c (map (enterif . kill) p) d h

enterPawn :: Position -> Game -> Game
enterPawn position (Game n c p d h) = let
    killAtBase position = kill' (baseFor position) p
    kill' _ [] = []
    kill' b (pl:pls) = (kill'' b pl):(kill' b pls)
    kill'' base player@(Player n po h e pa) =
        (Player n po h e (filter (notat base) pa))
    notat a (Pawn b) = a /= b
    notat _ _ = True
    in (Game n c (enterPlayerPawn position (killAtBase position)) d h)
enterPlayerPawn :: Position -> [Player] -> [Player]
enterPlayerPawn _ [] = []
enterPlayerPawn position (player@(Player n po h e pa):ps) = 
    let rest = enterPlayerPawn position ps 
        base = baseFor position in
        if position == po
            then (Player n po h e (enterOnePawn base pa)):rest
            else player:rest 
enterOnePawn :: Int -> [Pawn] -> [Pawn]
enterOnePawn _ [] = []
enterOnePawn base (Out:ps) = (OnBase base):ps
enterOnePawn base (pawn:ps) = pawn:(enterOnePawn base ps)
    

filterCard :: Card -> Game -> Game
filterCard card (Game n c p d h) = 
    (Game n c (map (filterPlayersCard card) p) d h)
filterPlayersCard :: Card -> Player -> Player
filterPlayersCard card (Player n po h e pa) = 
    (Player n po (filterHand card h) e pa)
filterHand :: Card -> [Card] -> [Card]
filterHand card = filter ((/=) card)

moveAllPawns :: Pawn -> Pawn -> Game -> Game
moveAllPawns before after (Game n c p d h) = 
    Game n c (map (moveAllPawns' before after) p) d h
moveAllPawns' :: Pawn -> Pawn -> Player -> Player
moveAllPawns' before after (Player n po h e pa) = 
    (Player n po h e (movePawn before after pa))
    

currentTurn :: Game -> Position
currentTurn game = currentTurnOf (gameHistory game) where
    currentTurnOf ((MoveEvent p m):hs) = nextOf p
    currentTurnOf ((Playing p):hs) = p
    currentTurnOf [] = South
    currentTurnOf (_:hs) = currentTurnOf hs
    nextOf South = West
    nextOf West = North
    nextOf North = East
    nextOf East = South

isWon :: Game -> Bool
isWon game = isWon' (map allParked [
        playerAt game South,
        playerAt game West,
        playerAt game North,
        playerAt game East
    ])
    where isWon' (True:_:True:_:[]) = True
          isWon' (_:True:_:True:[]) = True
          isWon' _ = False

wonBy :: Game -> Maybe Position
wonBy game = wonBy' (map allParked [
        playerAt game South,
        playerAt game West,
        playerAt game North,
        playerAt game East
    ])
    where wonBy' (True:_:True:_:[]) = Just South
          wonBy' (_:True:_:True:[]) = Just West
          wonBy' _ = Nothing

