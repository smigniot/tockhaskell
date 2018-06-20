import System.Exit (exitFailure)
import Game
import Robots
import Journey
import Data.List (intercalate, sort, nub, find)
import System.Random

joinAlice = registerPlayer "Alice" North
joinBob =   registerPlayer "Bob" South
joinCarol = registerPlayer "Carol" West
joinDavid = registerPlayer "David" East
allIn = joinAlice.joinBob.joinCarol.joinDavid

testBasics = do
    fresh <- newGame "Fresh game"
    if ("Fresh game" /= (gameName fresh))
        then exitFailure
        else putStrLn "Game creation OK"

testJoining = do
    fresh <- newGame "Fresh game"
    let notjoined = (joinAlice . joinBob . joinCarol) fresh in
        if (gameComplete notjoined)
            then exitFailure
            else putStrLn "Partial join OK"
    let joined = allIn fresh in
        if (gameComplete joined)
            then putStrLn "Full join OK"
            else exitFailure

testDeal = do
    fresh <- newGame "Fresh game"
    if 0 /= (length (gameDeck fresh))
        then exitFailure
        else putStrLn "No dealing OK"
    let joined = allIn fresh in
        if 0 /= (length (gameDeck joined))
            then exitFailure
            else putStrLn "No automatic dealing OK"
    let game = allIn fresh in do
        dealt <- dealHands game
        if 32 /= (length (gameDeck dealt))
            then exitFailure
            else putStrLn "Dealing once OK"
        d2 <- dealHands dealt
        if 16 /= (length (gameDeck d2))
            then exitFailure
            else putStrLn "Dealing twice OK"
        d3 <- dealHands d2
        if 0 /= (length (gameDeck d3))
            then exitFailure
            else putStrLn "Dealing again OK"
        d4 <- dealHands d3
        if 32 /= (length (gameDeck d4))
            then exitFailure
            else putStrLn "Refill OK"


blindExchange game = 
    mapply (map exchanger (gamePlayers game)) game where
        exchanger player game = 
            setExchanged game player (head (playerHand player))
        mapply [] game = game
        mapply (f:fs) game = mapply fs (f game)

testExchange = do
    fresh <- newGame "Fresh game"
    let game = allIn fresh in do
        if allExchanged game
            then exitFailure
            else putStrLn "Not exchanged OK"
        let done = blindExchange game in
            if not (allExchanged done)
                then exitFailure
                else putStrLn "Exchanging OK"

testJourney = do
    let j1 = journeys 0 5
        j2 = journeys 16 5
        l1 = length j1
        ns = map last j2
        l2 = length (head j1) in do
        if ((l1 /= 1) || (l2 /= 6))
            then exitFailure
            else putStrLn "Simple journey OK"
        if not ((elem 21 ns) && (elem 25 ns))
            then exitFailure
            else putStrLn "Parking branch OK"

testEnter = do
    fresh <- newGame "Test game"
    let game = (Game "Test game" (gameCreation fresh) [
            (Player "a" South [(Card Ace Spades)] Nothing [Out]),
            (Player "b" West  [] Nothing []),
            (Player "c" North [] Nothing []),
            (Player "d" East  [] Nothing []) ] []
            [])
        player = head (gamePlayers game)
        moves = movesFor player game in
        if not (elem (Enter (Card Ace Spades)) moves)
            then exitFailure
            else putStrLn "Entering OK"
    let game = (Game "Test game" (gameCreation fresh) [
            (Player "a" South [(Card Ace Spades)] Nothing [
                    Out, (OnBase 0)]),
            (Player "b" West  [] Nothing []),
            (Player "c" North [] Nothing []),
            (Player "d" East  [] Nothing []) ] []
            [])
        player = head (gamePlayers game)
        moves = movesFor player game in
        if (elem (Enter (Card Ace Spades)) moves)
            then exitFailure
            else putStrLn "No entering over pawn on base OK"

testMove = do
    fresh <- newGame "Test game"
    let game = (Game "Test game" (gameCreation fresh) [
            (Player "a" South [(Card Five Spades)] Nothing [
                    (OnBase 0)]),
            (Player "b" West  [] Nothing []),
            (Player "c" North [] Nothing []),
            (Player "d" East  [] Nothing []) ] []
            [])
        player = head (gamePlayers game)
        moves = movesFor player game 
        expected = Run (Card Five Spades) (OnBase 0) (Pawn 5) in
        if not (elem expected moves)
            then exitFailure
            else putStrLn "Simple move OK"
    let game = (Game "Test game" (gameCreation fresh) [
            (Player "a" South [(Card Five Spades)] Nothing [
                    (Pawn 1)]),
            (Player "b" West  [] Nothing []),
            (Player "c" North [] Nothing []),
            (Player "d" East  [] Nothing []) ] []
            [])
        player = head (gamePlayers game)
        moves = movesFor player game 
        expected = Run (Card Five Spades) (Pawn 1) (Pawn 6) in
        if not (elem expected moves)
            then exitFailure
            else putStrLn "Simple move without state change OK"
    let game = (Game "Test game" (gameCreation fresh) [
            (Player "a" West [(Card Five Spades)] Nothing [
                    (Pawn 16)]),
            (Player "b" South  [] Nothing []),
            (Player "c" North [] Nothing []),
            (Player "d" East  [] Nothing []) ] []
            [])
        player = head (gamePlayers game)
        moves = movesFor player game 
        ex1 = Run (Card Five Spades) (Pawn 16) (Parked 21)
        ex2 = Run (Card Five Spades) (Pawn 16) (Pawn 25) in
        if not ((elem ex1 moves) && (elem ex2 moves))
            then exitFailure
            else putStrLn "Moving including parking OK"
    let game = (Game "Test game" (gameCreation fresh) [
            (Player "a" North [(Card Five Spades)] Nothing [
                    (Pawn 16)]),
            (Player "b" West  [] Nothing []),
            (Player "c" South [] Nothing []),
            (Player "d" East  [] Nothing []) ] []
            [])
        player = head (gamePlayers game)
        moves = movesFor player game 
        exa = Run (Card Five Spades) (Pawn 16) (Pawn 21)
        exb = Run (Card Five Spades) (Pawn 16) (Parked 21)
        ex2 = Run (Card Five Spades) (Pawn 16) (Pawn 25) in
        if ((elem exa moves) || 
            (elem exb moves) ||
            (not $ elem ex2 moves))
            then exitFailure
            else putStrLn "No parking in others' OK"
    let game = (Game "Test game" (gameCreation fresh) [
            (Player "a" North [(Card Five Spades)] Nothing [
                    (Pawn 16)]),
            (Player "b" West  [] Nothing [(OnBase 24)]),
            (Player "c" South [] Nothing []),
            (Player "d" East  [] Nothing []) ] []
            [])
        player = head (gamePlayers game)
        moves = movesFor player game  in
        if (moves == [(Withdraw (Card Five Spades))])
            then putStrLn "Blocked by pawn on base OK"
            else exitFailure
    let game = (Game "Test game" (gameCreation fresh) [
            (Player "a" West [(Card Five Spades)] Nothing [
                    (Parked 19)]),
            (Player "b" South  [] Nothing []),
            (Player "c" North [] Nothing []),
            (Player "d" East  [] Nothing []) ] []
            [])
        player = head (gamePlayers game)
        moves = movesFor player game in
        if (moves == [(Withdraw (Card Five Spades))])
            then putStrLn "Parking is a dead end OK"
            else exitFailure
    let game = (Game "Test game" (gameCreation fresh) [
            (Player "a" South [(Card Ace Spades)] Nothing [
                    (Pawn 1)]),
            (Player "b" West  [] Nothing []),
            (Player "c" North [] Nothing []),
            (Player "d" East  [] Nothing []) ] []
            [])
        exa = Run (Card Ace Spades) (Pawn 1) (Pawn 2)
        exb = Run (Card Ace Spades) (Pawn 1) (Pawn 15)
        player = head (gamePlayers game)
        moves = movesFor player game in
        if ((elem exa moves) && (elem exb moves))
            then putStrLn "Ace checked OK"
            else exitFailure
    let game = (Game "Test game" (gameCreation fresh) [
            (Player "a" South [(Card Four Spades)] Nothing [
                    (Pawn 1)]),
            (Player "b" West  [] Nothing []),
            (Player "c" North [] Nothing []),
            (Player "d" East  [] Nothing []) ] []
            [])
        m = Run (Card Four Spades) (Pawn 1) (Pawn 89)
        player = head (gamePlayers game)
        moves = movesFor player game in
        if (moves == [m])
            then putStrLn "Four back OK"
            else exitFailure
    let game = (Game "Test game" (gameCreation fresh) [
            (Player "a" South [(Card Four Spades)] Nothing [
                    (OnBase 0)]),
            (Player "b" West  [] Nothing []),
            (Player "c" North [] Nothing []),
            (Player "d" East  [] Nothing []) ] []
            [])
        m = Run (Card Four Spades) (OnBase 0) (Pawn 88)
        player = head (gamePlayers game)
        moves = movesFor player game in
        if (moves == [m])
            then putStrLn "Four from base OK"
            else exitFailure
    let game = (Game "Test game" (gameCreation fresh) [
            (Player "a" South [(Card Four Spades)] Nothing [
                    (Pawn 26)]),
            (Player "b" West  [] Nothing [(OnBase 24)]),
            (Player "c" North [] Nothing []),
            (Player "d" East  [] Nothing []) ] []
            [])
        player = head (gamePlayers game)
        moves = movesFor player game in
        if (moves == [(Withdraw (Card Four Spades))])
            then putStrLn "Four blocked by based pawns OK"
            else exitFailure
    let game = (Game "Test game" (gameCreation fresh) [
            (Player "a" North [(Card Four Spades)] Nothing [
                    (Parked 46)]),
            (Player "b" West  [] Nothing [(OnBase 24)]),
            (Player "c" South [] Nothing []),
            (Player "d" East  [] Nothing []) ] []
            [])
        player = head (gamePlayers game)
        moves = movesFor player game in
        if (moves == [(Withdraw (Card Four Spades))])
            then putStrLn "Four blocked when parked OK"
            else exitFailure
    let game = (Game "Test game" (gameCreation fresh) [
            (Player "a" South [(Card Six Spades)] Nothing [
                    (Pawn 18), (Pawn 12)]),
            (Player "b" West  [] Nothing [(OnBase 24)]),
            (Player "c" North [] Nothing []),
            (Player "d" East  [] Nothing []) ] []
            [])
        player = head (gamePlayers game)
        moves = movesFor player game in
            if (moves == [(Withdraw (Card Six Spades))])
                then putStrLn "Dont kill self OK"
                else exitFailure


testEight = do
    fresh <- newGame "Test game"
    let game = (Game "Test game" (gameCreation fresh) [
            (Player "a" South [(Card Eight Hearts)] Nothing [
                    (Pawn 8)]),
            (Player "b" West  [] Nothing []),
            (Player "c" North [] Nothing []),
            (Player "d" East  [] Nothing []) ] []
            [])
        expected = (Run (Card Eight Hearts) (Pawn 8) (Pawn 16)):[
            (Warp (Card Eight Hearts) (Pawn 8) (Pawn n)) | 
                n <- [32,56,80]]
        player = head (gamePlayers game)
        moves = movesFor player game in
        if (moves == expected)
            then putStrLn "Eight warp OK"
            else exitFailure

testJack = do
    fresh <- newGame "Test game"
    let game = (Game "Test game" (gameCreation fresh) [
            (Player "a" South [(Card Jack Clubs)] Nothing [(Pawn 8)]),
            (Player "b" West  [] Nothing [(Pawn 86)]),
            (Player "c" North [] Nothing []),
            (Player "d" East  [] Nothing []) ] []
            [])
        player = head (gamePlayers game)
        moves = movesFor player game in
        if (moves == [(Swap (Card Jack Clubs) (Pawn 8) (Pawn 86))])
            then putStrLn "Jack swap OK"
            else exitFailure
    fresh <- newGame "Test game"
    let game = (Game "Test game" (gameCreation fresh) [
            (Player "a" South [(Card Jack Clubs)] Nothing [(Pawn 8)]),
            (Player "b" West  [] Nothing []),
            (Player "c" North [] Nothing []),
            (Player "d" East  [] Nothing []) ] []
            [])
        player = head (gamePlayers game)
        moves = movesFor player game in
        if (moves == [(Withdraw (Card Jack Clubs))])
            then putStrLn "Jack alone withdrawn OK"
            else exitFailure
    let game = (Game "Test game" (gameCreation fresh) [
            (Player "a" South [(Card Jack Clubs)] Nothing [
                (Pawn 8),(OnBase 0)]),
            (Player "b" West  [] Nothing []),
            (Player "c" North [] Nothing []),
            (Player "d" East  [] Nothing []) ] []
            [])
        player = head (gamePlayers game)
        moves = movesFor player game in
        if (moves == [(Swap (Card Jack Clubs) (OnBase 0) (Pawn 8))])
            then putStrLn "Jack swap onbase pawn OK"
            else exitFailure
    let game = (Game "Test game" (gameCreation fresh) [
            (Player "a" South [(Card Jack Clubs)] Nothing [(Pawn 8)]),
            (Player "b" West  [] Nothing [(OnBase 24)]),
            (Player "c" North [] Nothing []),
            (Player "d" East  [] Nothing []) ] []
            [])
        player = head (gamePlayers game)
        moves = movesFor player game in
        if (moves == [(Withdraw (Card Jack Clubs))])
            then putStrLn "Jack no swap onbase other OK"
            else exitFailure
    let game = (Game "Test game" (gameCreation fresh) [
            (Player "a" South [(Card Jack Clubs)] Nothing [(Pawn 8)]),
            (Player "b" West  [] Nothing [(Parked 44)]),
            (Player "c" North [] Nothing []),
            (Player "d" East  [] Nothing []) ] []
            [])
        player = head (gamePlayers game)
        moves = movesFor player game in
        if (moves == [(Withdraw (Card Jack Clubs))])
            then putStrLn "Jack no swap parked OK"
            else exitFailure

dbgFeasible :: Position -> Game -> [(Pawn,Pawn)]
dbgFeasible position game =
    let players = gamePlayers game
        player = playerAt game position
        moveables = mineOrPartners game player
        temptable = foldr (++) [] (map nextPlusOnes moveables)
        notblocked = filter (dontCollide game player) temptable
        feasible = filter (dontKillself moveables) notblocked in 
        feasible

testSevenBranches = do
    fresh <- newGame "Test game"
    let game = (Game "Test game" (gameCreation fresh) [
            (Player "a" South [(Card Seven Clubs)] Nothing [
                    (Pawn 8)]),
            (Player "b" West  [] Nothing [(Pawn 9)]),
            (Player "c" North [] Nothing []),
            (Player "d" East  [] Nothing []) ] []
            [])
        branches = sevenBranches 1 South [game]
        n = length branches
        g = head branches
        p1 = playerPawns (playerAt g South)
        p2 = playerPawns (playerAt g West)
        i = length (allPawns g) in
        if (n==1) && (elem (Pawn 9) p1) && (elem Out p2) && (i == 2)
            then putStrLn "7: One step kill OK"
            else exitFailure
    let game = (Game "Test game" (gameCreation fresh) [
            (Player "a" South [(Card Seven Clubs)] Nothing [
                    (Pawn 8)]),
            (Player "b" West  [] Nothing [(Pawn 9)]),
            (Player "c" North [] Nothing [(Pawn 10)]),
            (Player "d" East  [] Nothing []) ] []
            [])
        branches = sevenBranches 1 South [game]
        n = length branches
        g = head branches
        p1 = playerPawns (playerAt g South)
        p2 = playerPawns (playerAt g West)
        i = length (allPawns g) in
        if (n==1) && (elem (Pawn 9) p1) && (elem Out p2) && (i == 3)
            then putStrLn "7: Partner don't move OK"
            else do
                putStrLn ("DBG: " ++ (show n))
                putStrLn ("DBG: " ++ (show p1))
                putStrLn ("DBG: " ++ (show p2))
                putStrLn ("DBG: " ++ (show i))
                putStrLn ("DBG: " ++ (show (playerAt game South)))
                putStrLn ("DBG: " ++ (show (mineOrPartners game (playerAt game South))))
                putStrLn ("DBG: " ++ (show (sevenOneStep South game)))
                exitFailure
    let game = (Game "Test game" (gameCreation fresh) [
            (Player "a" South [(Card Seven Clubs)] Nothing [
                    (Pawn 8)]),
            (Player "b" West  [] Nothing [(Pawn 10)]),
            (Player "c" North [] Nothing []),
            (Player "d" East  [] Nothing []) ] []
            [])
        branches = sevenBranches 2 South [game]
        n = length branches
        g = head branches
        p1 = playerPawns (playerAt g South)
        p2 = playerPawns (playerAt g West)
        i = length (allPawns g) in
        if (n==1) && (elem (Pawn 10) p1) && (elem Out p2) && (i == 2)
            then putStrLn "7: Two steps kill OK"
            else exitFailure
    let game = (Game "Test game" (gameCreation fresh) [
            (Player "a" West [(Card Seven Clubs)] Nothing [
                    (Pawn 8),(Pawn 0),(Pawn 18)]),
            (Player "b" South  [] Nothing [(Pawn 86)]),
            (Player "c" North [] Nothing [(Pawn 24)]),
            (Player "d" East  [] Nothing []) ] []
            [])
        branches = sevenBranches 2 West [game]
        onethat f l = not (null (filter f l)) in do
            if onethat (\g -> elem Out (allPawns g)) branches
                then putStrLn "7: Complex two steps 1 kill OK"
                else exitFailure
            if onethat (\g -> 
                        (elem (Pawn 9) (allPawns g)) &&
                        (elem (Parked 19) (allPawns g))
                    ) branches
                then putStrLn "7: Complex park-move split OK"
                else exitFailure
            if onethat (\g -> 
                        (elem (Pawn 1) (allPawns g)) &&
                        (elem (Pawn 23) (allPawns g))
                    ) branches
                then putStrLn "7: Complex two steps 2 OK"
                else exitFailure
    let game = (Game "Test game" (gameCreation fresh) [
            (Player "a" South [(Card Seven Clubs)] Nothing [
                    (Pawn 4),(Pawn 5),(Pawn 6)]),
            (Player "b" West  [] Nothing []),
            (Player "c" North [] Nothing []),
            (Player "d" East  [] Nothing []) ] []
            [])
        branches = sevenBranches 7 South [game]
        onethat f l = not (null (filter f l))
        chk1 = onethat (\g -> (elem (Pawn 6) (allPawns g)) &&
                    (elem (Pawn 7) (allPawns g)) &&
                    (elem (Pawn 9) (allPawns g))) branches
        chk2 = onethat (\g -> (elem (Pawn 4) (allPawns g)) &&
                    (elem (Pawn 5) (allPawns g)) &&
                    (elem (Pawn 13) (allPawns g))) branches
        chk3 = onethat (\g -> (elem (Pawn 5) (allPawns g)) &&
                    (elem (Pawn 8) (allPawns g)) &&
                    (elem (Pawn 9) (allPawns g))) branches
        in
            if chk1 && chk2 && chk3
                then putStrLn "7: Small steppings OK"
                else exitFailure
    let game = (Game "Test game" (gameCreation fresh) [
            (Player "a" West [(Card Seven Clubs)] Nothing [
                    (Parked 19),(Pawn 16)]),
            (Player "b" South  [] Nothing [
                    (Pawn 24)]),
            (Player "c" North [] Nothing []),
            (Player "d" East  [] Nothing []) ] []
            [])
        p1 = nextPositions 19
        plus = nextPlusOnes (Parked 19) in do
        if elem 20 p1
            then putStrLn "7: Stepping 1/2 in parking OK"
            else exitFailure
        if elem ((Parked 19),(Parked 20)) plus
            then putStrLn "7: Stepping 2/2 in parking OK"
            else exitFailure

    let game = (Game "Test game" (gameCreation fresh) [
            (Player "a" West [(Card Seven Clubs)] Nothing [
                    (Parked 19),(Pawn 16)]),
            (Player "b" South  [] Nothing [
                    (Pawn 24)]),
            (Player "c" North [] Nothing []),
            (Player "d" East  [] Nothing []) ] []
            [])
        branches = sevenBranches 1 West [game]
        onethat f l = not (null (filter f l))
        chk1 = onethat (\g -> (elem (Parked 20) (allPawns g))) branches
        in
            if chk1
                then putStrLn "7: Parking once more OK"
                else exitFailure

testSeven = do
    fresh <- newGame "Test game"
    let game = (Game "Test game" (gameCreation fresh) [
            -- 24 25
            -- 23
            -- 18 19 20 21 22
            -- 17
            -- 16 15 14
            (Player "a" West [(Card Seven Clubs)] Nothing [
                    (Parked 19),(Pawn 16)]),
            (Player "b" South  [] Nothing [
                    (Pawn 24)]),
            (Player "c" North [] Nothing []),
            (Player "d" East  [] Nothing []) ] []
            [])
        future = (Game "Test game" (gameCreation fresh) [
            (Player "a" West [(Card Seven Clubs)] Nothing [
                    (Parked 22),(Pawn 24)]),
            (Player "b" South  [] Nothing [
                    Out]),
            (Player "c" North [] Nothing []),
            (Player "d" East  [] Nothing []) ] []
            [])
        future2 = (Game "Test game" (gameCreation fresh) [
            (Player "a" West [(Card Seven Clubs)] Nothing [
                    (Parked 22),(Parked 20)]),
            (Player "b" South  [] Nothing [
                    (Pawn 24)]),
            (Player "c" North [] Nothing []),
            (Player "d" East  [] Nothing []) ] []
            [])
        moves = movesFor (playerAt game West) game
        chk1 = elem (asSevenMove (Killer (Card Seven Clubs) game future)) moves
        chk2 = elem (asSevenMove (Killer (Card Seven Clubs) game future2)) moves
        in do
            if chk1
                then putStrLn "7: Full parking killing OK"
                else exitFailure
            if chk2
                then putStrLn "7: Parking two pawns OK"
                else exitFailure
    let game = (Game "Test game" (gameCreation fresh) [
            -- 18 19 20 21 22
            -- 17
            (Player "a" West [(Card Seven Clubs)] Nothing [
                    (Pawn 17),(Pawn 18),
                    (Parked 19),(Parked 21)
                    ]),
            (Player "b" South  [] Nothing [
                    (Pawn 24)]),
            (Player "c" North [] Nothing []),
            (Player "d" East  [] Nothing []) ] []
            [])
        who = playerAt game West
        moves = movesFor who game
        futures = map getFuture moves
        getFuture move = applyMove who move game
        allWest g = allParked (playerAt g West)
        winnings = filter allWest futures in
            if not (null winnings)
                then putStrLn "7: Ultimate allpark OK"
                else exitFailure

testPartnerSeven = do
    fresh <- newGame "Test game"
    let game = (Game "Triple seven" (gameCreation fresh) [
            (Player "a" West [(Card Seven Clubs)] Nothing [
                    (Pawn 17),(Parked 20),
                    (Parked 21),(Parked 22)
                    ]),
            (Player "b" East  [] Nothing [
                    (Pawn 30)]),
            (Player "c" North [] Nothing []),
            (Player "d" South  [] Nothing []) ] []
            [])
        who = playerAt game West
        moves = movesFor who game
        futures = map getFuture moves
        getFuture move = applyMove who move game
        allWest g = allParked (playerAt g West)
        at35 (Pawn n) = n == 35
        at35 _ = False
        east35 g = not (null (filter at35 (playerPawns 
                        (playerAt g East))))
        cont g = (allWest g) && (east35 g)
        winnings = filter cont futures in do
            -- putStrLn $ show game
            if not (null winnings)
                then putStrLn "7: Continued on partner OK"
                else exitFailure

testPartnerBasics = do
    fresh <- newGame "Test game"
    let game = (Game "Test game" (gameCreation fresh) [
            (Player "a" West [(Card Five Clubs)] Nothing [
                    (Parked 19),(Parked 20),
                    (Parked 21),(Parked 22)
                    ]),
            (Player "b" East  [] Nothing [
                    (Pawn 30)]),
            (Player "c" North [] Nothing []),
            (Player "d" South  [] Nothing []) ] []
            [])
        moves = movesFor (playerAt game West) game
        expected = Run (Card Five Clubs) (Pawn 30) (Pawn 35) in
        if elem expected moves
            then putStrLn "Running on partner OK"
            else exitFailure

testPartnerEnterWarp = do
    fresh <- newGame "Test game"
    let game = (Game "Test game" (gameCreation fresh) [
            (Player "a" West [(Card Ace Clubs),
                    (Card Eight Clubs)] Nothing [
                    (Parked 19),(Parked 20),
                    (Parked 21),(Parked 22)
                    ]),
            (Player "b" East  [] Nothing [
                    (Pawn 8),Out]),
            (Player "c" North [] Nothing []),
            (Player "d" South  [] Nothing []) ] []
            [])
        moves = movesFor (playerAt game West) game
        expected = Enter (Card Ace Clubs)
        expected2 = Warp (Card Eight Clubs) (Pawn 8) (Pawn 80) in
        if elem expected moves
            then putStrLn "Warping on partner OK"
            else exitFailure

testApplyMove = do
    fresh <- newGame "Test game"
    let game = (Game "Test game" (gameCreation fresh) [
            (Player "a" South [(Card Five Spades)] Nothing [
                    (OnBase 0)]),
            (Player "b" West  [] Nothing []),
            (Player "c" North [] Nothing []),
            (Player "d" East  [] Nothing []) ] []
            [])
        player = head (gamePlayers game)
        moves = movesFor player game 
        expected = Run (Card Five Spades) (OnBase 0) (Pawn 5)
        chk1 = (moves == [expected])
        g2 = applyMove player expected game
        chk2 = elem (Pawn 5) (allPawns g2)
        south2 = playerAt g2 South
        chk3 = not (elem (Card Five Spades) (playerHand south2)) in
        if chk1 && chk2 && chk3
            then putStrLn "Applied run move OK"
            else exitFailure
    let game = (Game "Test game" (gameCreation fresh) [
            (Player "a" South [(Card Ace Spades)] Nothing [
                    Out,Out,Out,Out]),
            (Player "b" West  [] Nothing []),
            (Player "c" North [] Nothing []),
            (Player "d" East  [] Nothing []) ] []
            [])
        south = playerAt game South
        moves = movesFor south game 
        enter = (Enter (Card Ace Spades))
        chk1 = (moves == [enter])
        after = applyMove south enter game
        south2 = playerAt after South
        chk2 = (playerPawns south2) == [(OnBase 0),Out,Out,Out]
        chk3 = (playerHand south2) == [] in
        if chk1 && chk2 && chk3
            then putStrLn "Applied enter move OK"
            else exitFailure
    let game = (Game "Test game" (gameCreation fresh) [
            (Player "a" South [(Card Eight Spades)] Nothing [
                    (Pawn 8)]),
            (Player "b" West  [] Nothing []),
            (Player "c" North [] Nothing []),
            (Player "d" East  [] Nothing []) ] []
            [])
        south = playerAt game South
        warp = Warp (Card Eight Spades) (Pawn 8) (Pawn 80)
        chk1 = elem warp (movesFor south game)
        after = applyMove south warp game
        south2 = playerAt after South
        chk2 = not (elem (Card Eight Spades) (playerHand south2))
        chk3 = [(Pawn 80)] == (playerPawns south2) in do
        if chk1 && chk2 && chk3
            then putStrLn "Applied warp move OK"
            else exitFailure
    let jack = Card Jack Diamonds
        game = (Game "Test game" (gameCreation fresh) [
            (Player "a" South [jack] Nothing [
                    (Pawn 10),(OnBase 0)]),
            (Player "b" West  [] Nothing []),
            (Player "c" North [] Nothing []),
            (Player "d" East  [] Nothing []) ] []
            [])
        south = playerAt game South
        swap = Swap jack (OnBase 0) (Pawn 10)
        chk1 = elem swap (movesFor south game)
        after = applyMove south swap game
        south2 = playerAt after South
        chk2 = not (elem jack (playerHand south2))
        chk3 = elem (Pawn 0)  (playerPawns south2)
        chk4 = elem (Pawn 10) (playerPawns south2) in
        if chk1 && chk2 && chk3 && chk4
            then putStrLn "Applied swap unbases OK"
            else exitFailure
    let jack = Card Jack Diamonds
        game = (Game "Test game" (gameCreation fresh) [
            (Player "a" South [jack] Nothing [(Pawn 10)]),
            (Player "b" West  [] Nothing []),
            (Player "c" North [] Nothing [(Pawn 80)]),
            (Player "d" East  [] Nothing []) ] []
            [])
        south = playerAt game South
        swap = Swap jack (Pawn 10) (Pawn 80)
        after = applyMove south swap game
        south2 = playerAt after South
        north2 = playerAt after North
        chk1 = elem (Pawn 80) (playerPawns south2)
        chk2 = elem (Pawn 10) (playerPawns north2) in
        if chk1 && chk2
            then putStrLn "Applied swap move OK"
            else exitFailure
    let game = (Game "Test game" (gameCreation fresh) [
            (Player "a" West [(Card Seven Clubs)] Nothing [
                    (Pawn 17),(Parked 20),
                    (Parked 21),(Parked 22)
                    ]),
            (Player "b" East  [] Nothing [
                    (Pawn 30)]),
            (Player "c" North [] Nothing []),
            (Player "d" South  [] Nothing []) ] []
            [])
        who = playerAt game West
        moves = movesFor who game
        isGoodOne sm@(SevenMove c hops) = isGoodGame (applyMove who sm game)
        isGoodOne _ = False
        isGoodGame (Game n c (west:east:ps) d h) = 
            (allParked west) && (elem (Pawn 35) (playerPawns east))
        winner = head (filter isGoodOne moves)
        after = applyMove (playerAt game West) winner game
        west2 = playerAt after West
        chk1 = not (elem (Card Seven Clubs) (playerHand west2))
        chk2 = isGoodGame after in
        if chk1 && chk2
            then putStrLn "Applied multi-seven move OK"
            else exitFailure

testEnterKills = do
    fresh <- newGame "Test game"
    let before = Game "Test game" (gameCreation fresh) [
                (Player "Alice" South [(Card Ace Spades)] Nothing [
                    Out,Out,Out,Out]),
                (Player "Bob"   West  [] Nothing [
                    (Pawn 0),Out,Out,Out]),
                (Player "Carol" North [] Nothing [Out,Out,Out,Out]),
                (Player "David" East  [] Nothing [Out,Out,Out,Out])
            ] [] []
        move = Enter (Card Ace Spades)
        after = applyMove (head (gamePlayers before)) move before
        intpos Out = -1
        intpos p = pawnPosition p
        haszero p = not (null (filter (((==) 0) . intpos) (playerPawns p)))
        alice = head (gamePlayers after)
        bob = head (tail (gamePlayers after))
        in do
            if (haszero alice) && (not (haszero bob))
                then putStrLn "Entering kills onbase OK"
                else exitFailure

testApplyRunKills = do
    fresh <- newGame "Test game"
    let before = Game "Test game" (gameCreation fresh) [
                (Player "Alice" South [(Card Five Spades)] Nothing [
                    (Pawn 5), Out,Out,Out]),
                (Player "Bob"   West  [] Nothing [
                    (Pawn 10),Out,Out,Out]),
                (Player "Carol" North [] Nothing [Out,Out,Out,Out]),
                (Player "David" East  [] Nothing [Out,Out,Out,Out])
            ] [] []
        move = Run (Card Five Spades) (Pawn 5) (Pawn 10)
        after = applyMove (head (gamePlayers before)) move before
        intpos Out = -1
        intpos p = pawnPosition p
        hasten p = not (null (filter (((==) 10) . intpos) (playerPawns p)))
        alice = head (gamePlayers after)
        bob = head (tail (gamePlayers after))
        in do
            if (hasten alice) && (not (hasten bob))
                then putStrLn "Running 5 kills destination OK"
                else do
                    putStrLn ("FaultyMoveKills:\n"++(show alice)++"\n"++(show bob))
                    exitFailure


testSelfJump = do
    fresh <- newGame "Test game"
    let game = (Game "Test game" (gameCreation fresh) [
            (Player "a" South [(Card Five Spades)] Nothing []),
            (Player "b" West  [] Nothing []),
            (Player "c" North [(Card Nine Spades)] Nothing [
                    (OnBase 48),
                    (Pawn 55)]),
            (Player "d" East  [] Nothing []) ] []
            [])
        player = playerAt game North
        moves = movesFor player game 
        expected = Run (Card Nine Spades) (Pawn 55) (Pawn 64)
        unexpected = Run (Card Nine Spades) (Pawn 48) (Pawn 57)
        chk1 = expected `elem` moves
        chk2 = not (unexpected `elem` moves) in
        if chk1 && chk2
            then putStrLn "Non-jumping runs only OK"
            else exitFailure

testApplyWeirdSeven = do
    fresh <- newGame "Test game"
    let game = (Game "Test game" (gameCreation fresh) [
            (Player "a" South [(Card Seven Spades)] Nothing [(Pawn 5)]),
            (Player "b" West [] Nothing [(Pawn 12)]),
            (Player "c" North [(Card Nine Spades)] Nothing []),
            (Player "d" East  [] Nothing []) ] []
            [])
        player = playerAt game South
        moves = movesFor player game 
        expected = SevenMove (Card Seven Spades) [(Pawn 5,Pawn 12),(Pawn 12,Out)]
        chk1 = expected `elem` moves 
        after = applyMove player expected game
        chk2 = not (Out `elem` (playerPawns (playerAt after South)))
        in if chk1 && chk2
            then putStrLn "Seven hops sequencing OK"
            else exitFailure

testIterateGames = do
    mapM_ iterateGame [1..10] 
    putStrLn "Iterating some games OK"
    where
    iterateGame i = do
        g0 <- newGame ("Test game #"++(show i))
        result <- iteratePhases 1000 (register g0) 
        putStrLn ("Game " ++ (gameName result)
                  ++ " played " ++ (show (length (gameHistory result)))
                  ++ " turns")
        -- putStrLn (show result)
    register = ((registerPlayer "Alice" South) .
                (registerPlayer "Bob" West) .
                (registerPlayer "Carol" North) .
                (registerPlayer "David" East))
    iteratePhases n game = if n <= 0
        then return game
        else do
            dealt <- dealHands game
            next <- iteratePhase dealt
            checkIterate n next
    checkIterate n next = 
        if isWon next
            then return next
            else iteratePhases (n-1) next
    iteratePhase g0 = let
        makeChance = generateArray 24 0 100
        applied chance = let
            ex = take 4 chance
            ch = take 20 (drop 4 chance)
            g1 = exchanged ex g0
            g2 = moved ch g1 in
            g2
        in do
            chance <- makeChance
            return (applied chance)
    generateArray :: Int -> Int -> Int -> IO [Int]
    generateArray size min max =
        getStdGen >>= return . take size . randomRs (min, max)
    exchanged choices game = exchanged' choices (gamePlayers game) game
    exchanged' choices players game = if null choices
        then endExchange game
        else let
            first = head players
            card = chooseCard (playerHand first) (head choices)
            one = setExchanged game first card
            chooseCard cards n = cards !! (mod n (length cards))
            in exchanged' (tail choices) (tail players) one
    empty game = null (foldr (++) [] (map playerHand (gamePlayers game)))
    moved choices game = if empty game
        then game
        else let
            turn = currentTurn game
            who = playerAt game turn
            moves = movesFor who game
            chosen = chooseMove (head choices) moves
            modified = applyMove who chosen game
            applied = historize chosen modified
            historize move game@(Game n c p d h) =
                (Game n c p d ((MoveEvent (playerPosition who) move):h))
            chooseMove n moves = moves !! (mod n (length moves))
            in moved (tail choices) applied

testWeirdSeven = do
    fresh <- newGame "Test game"
    let game = (Game "Test game" (gameCreation fresh) [
            (Player "a" West [(Card Seven Spades)] Nothing [
                (Pawn 16), (Parked 20)
            ]),
            (Player "b" South  [] Nothing []),
            (Player "c" North [] Nothing []),
            (Player "d" East  [] Nothing []) ] []
            [])
        player = playerAt game West
        moves = movesFor player game 
        uniks = nub (sort moves)
        expected = (SevenMove (Card Seven Spades) [
                ((Pawn 16),(Parked 21)),
                ((Parked 20),(Parked 22))
            ])
        in 
            if not (elem expected moves)
                then exitFailure
                else putStrLn "Weird Seven OK"

testWeirdSeven2 = do
    fresh <- newGame "Test game"
    let game = (Game "Test game" (gameCreation fresh) [
            (Player "a" West [(Card Seven Spades)] Nothing [
                (Pawn 80),
                (Parked 20),
                (Pawn 36),
                (Pawn 16)
            ]),
            (Player "b" South  [] Nothing []),
            (Player "c" North [] Nothing []),
            (Player "d" East  [] Nothing []) ] []
            [])
        futures = map pshow (nub (sevenBranches 7 West [game]))
        pshow game = intercalate "," (map (show . pawnPosition) (allPawns game))
        expected = "80,22,36,21"
        in do
            if not (elem expected futures)
                then exitFailure
                else putStrLn "Weird Seven 2 OK"


testWeirdSeven3 = do
    fresh <- newGame "Test game"
    let game = (Game "Test game" (gameCreation fresh) [
            (Player "a" West [(Card Seven Spades)] Nothing [
                (Pawn 80),
                (Parked 20),
                (Pawn 36),
                (Pawn 16)
            ]),
            (Player "b" South  [] Nothing []),
            (Player "c" North [] Nothing []),
            (Player "d" East  [] Nothing []) ] []
            [])
        player = playerAt game West
        moves = movesFor player game 
        leadto (SevenMove c h) = intercalate "," $ map (show . pawnPosition . snd) h
        uniks = nub (sort moves)
        m = find ((== "36,80,21,22") . leadto) uniks
        expected = (SevenMove (Card Seven Spades) [
                ((Parked 20),(Parked 22)),
                ((Pawn 16),(Parked 21))
            ])
        dashed (SevenMove c h) = intercalate "," $ map (show . pawnPosition . snd) h
        in do
            if not (elem expected uniks)
                then case m of
                    Just weird -> do
                        putStrLn ("FIXME sometime : " ++ (show weird))
                        putStrLn "Weird Seven 3 semi OK"
                    _ -> exitFailure
                else putStrLn "Weird Seven 3 OK"

testWeirdSeven4 = do
    fresh <- newGame "Test game"
    let game = (Game "Test game" (gameCreation fresh) [
            (Player "a" West [(Card Seven Spades)] Nothing [
                (Parked 19),
                (Parked 21),
                (Parked 22),
                (Pawn 3)
            ]),
            (Player "b" South  [] Nothing [
                (Parked 94)
            ]),
            (Player "c" North [] Nothing [
                (Pawn 0)
            ]),
            (Player "d" East  [] Nothing [
                (Parked 70),
                (Parked 69),
                (Pawn 66),
                (Pawn 63)
            ]) ] [] [])
        futures = map pshow (nub (sevenBranches 7 East [game]))
        pshow game = intercalate "," (map (show . pawnPosition) (allPawns game))
        expected = "20,21,22,3,94,0,70,69,68,67"
        in do
            if not (elem expected futures)
                then exitFailure
                else putStrLn "Weird Seven 4 OK"

testWeirdParkAce = do
    fresh <- newGame "Test game"
    let game = (Game "Test game" (gameCreation fresh) [
            (Player "a" West [
                     (Card Ace Diamonds),
                     (Card Five Hearts)
                ] Nothing [
                    (Parked 19), (Parked 20), (Parked 21), (Parked 22)
            ]),
            (Player "b" South  [] Nothing [
                (Parked 91), (Parked 92), (Parked 93), (Parked 94)
            ]),
            (Player "c" North [] Nothing [
                (Parked 46), (Parked 45), (Parked 44), (Pawn 66)
            ]),
            (Player "d" East [] Nothing [
                (Pawn 54),
                (Pawn 76),
                (OnBase 72)
            ]) ] [] [])
        player = playerAt game West
        moves = movesFor player game 
        expected = Run (Card Ace Diamonds) (Pawn 54) (Parked 68)
        in 
            if not (elem expected moves)
                then exitFailure
                else putStrLn "Weird Park Ace OK"

testSevenFinishes = do
    fresh <- newGame "Test game"
    let game = (Game "Test game" (gameCreation fresh) [
            (Player "a" West  [] Nothing []),
            (Player "d" East [] Nothing []),
            (Player "b" South [] Nothing [
                (Parked 91), (Parked 92), (Parked 93), (Parked 94) ]),
            (Player "c" North [
                 (Card Seven Spades)
            ] Nothing [
                (Parked 46), (Parked 45), (Parked 44), (Pawn 38) ])
            ] [] [])
        player = playerAt game North
        moves = movesFor player game 
        expected = (SevenMove (Card Seven Spades) [
                ((Pawn 38),(Parked 43))])
        in
            if not (elem expected moves)
                then exitFailure
                else putStrLn "Finishing seven OK"

testTimmy = do
    g0 <- newGame "Timmy game"
    let g1 = (Game "Test game" (gameCreation g0) [
            (Player "Timmy" West [] Nothing []),
            (Player "b" South  [] Nothing []),
            (Player "c" North [] Nothing []),
            (Player "d" East  [] Nothing []) ] []
            [])
        in do
            game <- dealHands g1
            let before = playerExchange (playerAt game West) in do
                after <- robotExchange (playerAt game West) game
                let maybecard = playerExchange (playerAt after West) in do
                    if (Nothing == maybecard) || (Nothing /= before)
                        then exitFailure
                        else putStrLn "Finished timmy gift OK"
                

testTimmyExchange = do
    g0 <- newGame "Timmy game"
    let g1 = (Game "Test game" (gameCreation g0) [
            (Player "Timmy" West [] Nothing []),
            (Player "b" South  [] Nothing []),
            (Player "c" North [] Nothing []),
            (Player "d" East  [] Nothing []) ] []
            [])
        exfast g = exfast' [South,East,North] g
        exfast' (p:ps) g = exfast' ps (setExchanged g (playerAt g p) (head (playerHand (playerAt g p))))
        exfast' [] g = g
        in do
            game <- dealHands g1
            let exchanged = exfast game in do
                after <- robotExchange (playerAt exchanged West) exchanged
                let hands = map (\p -> length (playerHand p)) (gamePlayers after) in do
                    if not ([5,5,5,5] == hands)
                        then exitFailure
                        else putStrLn "Finished timmy exchange OK"

testTimmyVsYoshi = let
    register = ((registerPlayer "Yoshi" South) .
                (registerPlayer "Timmy" West) .
                (registerPlayer "Yoshi" North) .
                (registerPlayer "Timmy" East))
    playOnce :: IO (Maybe Position)
    playOnce = do
        g0 <- newGame "Battle"
        g1 <- dealHands $ register g0
        g2 <- playRobots g1
        return $ wonBy g2
    playMore :: Int -> [(Maybe Position)] -> IO [(Maybe Position)]
    playMore 0 l = return l
    playMore n l = do
        w <- playOnce
        playMore (n-1) (w:l)
    counts a l = length $ filter ((==) a) l
    in do
        l <- playMore 30 []
        let ys = counts (Just South) l
            ts = counts (Just West) l
        putStrLn ("Yoshi "++(show ys)
            ++" vs Timmy "++(show ts))
        if ts > ys
            then exitFailure
            else putStrLn "Finished timmy vs yoshi OK"
                

main = do
    testBasics
    testJoining
    testDeal
    testExchange
    testJourney
    testEnter
    testMove
    testEight
    testJack
    testSevenBranches
    testSeven
    testPartnerSeven
    testPartnerBasics
    testPartnerEnterWarp
    testApplyMove
    testEnterKills
    testApplyRunKills
    testSelfJump
    testApplyWeirdSeven
    testIterateGames
    testWeirdSeven
    testWeirdSeven2
    testWeirdSeven3
    testWeirdSeven4
    testWeirdParkAce
    testSevenFinishes
    testTimmy
    testTimmyExchange
    testTimmyVsYoshi


