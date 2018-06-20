module Robots where


import Game
import Shuffle
import Data.List

-- | When a robot chooses a card to give
type ChooseGivenCard = Player -> Game -> IO Card

-- | When a robot chooses a move to play
type ChooseMove = Game -> IO Move


-- Timmy ! Yoshi !
isRobot :: Player -> Bool
isRobot player@(Player name _ _ _ _) = (name == "Timmy") || (name == "Yoshi")


-- | Timmy always gives first card !
timmyChooseGivenCard :: ChooseGivenCard
timmyChooseGivenCard timmy game@(Game n c p d h) = do
    hand <- shuffle $ playerHand timmy
    return $ head hand

-- | Timmy always takes random move !
timmyChooseMove :: ChooseMove
timmyChooseMove game@(Game n c p d h) = let
    player = playerAt game $ currentTurn game
    moves = movesFor player game
    in do
        m <- shuffle moves
        return (head m)



-- | Yoshi gives a card to enter
yoshiChooseGivenCard :: ChooseGivenCard
yoshiChooseGivenCard yoshi game@(Game n c p d h) = let
    enterings = aceKings $ playerHand yoshi
    partner = playerAt game $ oppositeDirection $ playerPosition yoshi
    n = length enterings
    in if (not (allParked partner)) && (n>=2)
        then return $ head enterings
        else if (not (allParked partner)) && (allParked yoshi) && (n>=1)
            then return $ head enterings
            else do
                hand <- shuffle $ playerHand yoshi
                return $ head hand

-- | Yoshi parks or enters first
yoshiChooseMove :: ChooseMove
yoshiChooseMove game@(Game n c p d h) = let
    yoshi = playerAt game $ currentTurn game
    moves = movesFor yoshi game
    enterings = filter isEnter moves
    isEnter (Enter _) = True
    isEnter _ = False
    byrack = map rackDiff moves
    rackDiff m = ((rackCount (applyMove yoshi m game))-(rackCount game),m)
    rackCount g = length $ filter isParked $ allPawns g
    racking = head $ sortBy betterRack byrack
    betterRack (r1,m1) (r2,m2) =
        if r1>r2 then LT
        else if r1<r2 
            then GT
            else EQ
    bydist = map distanceOf moves
    distanceOf m = ((yoshiDistance yoshi (applyMove yoshi m game)),m)
    shorty = head $ sortBy betterDist bydist
    betterDist (r1,m1) (r2,m2) =
        if r1>r2 then GT
        else if r1<r2 
            then LT
            else EQ
    in if not $ null enterings
        then return $ head enterings
        else if (fst racking) > 0
            then return $ snd racking
            else return $ snd shorty

yoshiDistance :: Player -> Game -> Int
yoshiDistance yoshi game = let
    position = playerPosition yoshi
    partner = oppositeDirection position
    posPawns = [(pos,pawn) | pos <- [West,North,East,South], pawn <- (playerPawns (playerAt game pos))]
    dists = map distOf posPawns
    distOf :: (Position,Pawn) -> Int
    isTeam pos = (pos == position)||(pos == partner)
    distOf (pos,Out) = if isTeam pos
        then 96*2
        else -96
    distOf (pos,pawn) = let
        n = pawnPosition pawn
        b = (baseFor pos) - 2
        in ((96+(b-n)) `mod` 96)
    in foldr (+) 0 dists


-- | Yoshi or Timmy gives a card
robotExchange :: Player -> Game -> IO Game
robotExchange robot@(Player name _ _ _ _) game = case name of 
    "Timmy" -> do
        card <- timmyChooseGivenCard robot game
        return (exchangedGame game robot card)
    "Yoshi" -> do
        card <- yoshiChooseGivenCard robot game
        return (exchangedGame game robot card)
    _ -> return game


-- | Yoshi or Timmy play
robotMoves :: Player -> Game -> IO Game
robotMoves robot@(Player name _ _ _ _) game = case name of
    "Timmy" -> do 
        move <- timmyChooseMove game
        robotMoves' robot move game
    "Yoshi" -> do
        move <- yoshiChooseMove game
        robotMoves' robot move game
    _ -> return game

robotMoves' robot move game = let
    applied = applyMove robot move game
    historized = historize move applied
    position = playerPosition robot
    historize move game@(Game n c p d h) = (Game n c p d ((MoveEvent position move):h))
    handsEmpty game@(Game n c p d h) = [True,True,True,True] == map handEmpty p
    handEmpty player = null (playerHand player)
    in do
        after <- if (handsEmpty historized)
            then dealHands historized
            else return historized
        return after


-- | Play
playRobots :: Game -> IO Game
playRobots game = let
    st = stateOf game
    ended = isWon game
    players = gamePlayers game
    exc = case st of
        Exchanging    -> not ended
        Exchange _ _  -> not ended
        _             -> False
    notexc = filter isRobot $ filter notExchanged players
    notExchanged player@(Player _ _ _ (Just c) _) = False
    notExchanged player@(Player _ _ _ Nothing _) = True
    pl = case st of
        Playing _     -> not ended
        MoveEvent _ _ -> not ended
        _             -> False
    cp = playerAt game $ currentTurn game
    in
        -- if state == exchanging and one robot has not exchanged yet
        --    make a robot exchange
        --    continue to iterate
        if exc && (not (null notexc))
            then do
                after <- robotExchange (head notexc) game
                playRobots after
            else if pl && (isRobot cp)
                -- if state == playing and one robot must play
                --    make the robot play
                --    continue to iterate
                then do
                    after <- robotMoves cp game
                    playRobots after
                else return game -- else freeze game


