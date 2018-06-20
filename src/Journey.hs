module Journey where


-- | The different branchings onto the Tock board
--   Entering bases are always labelled 20, but are in fact 0,24,48 and 72
--   Branching happens at parking entries, always labelled 18
--   This function returns the next possible positions by advancing one
--   from the starting, provided position. There may be one, two or zero
nextPositions :: Int -> [Int]
nextPositions 18 = [19,23]
nextPositions 22 = []
nextPositions 42 = [43,47]
nextPositions 46 = []
nextPositions 66 = [67,71]
nextPositions 70 = []
nextPositions 90 = [91,95]
nextPositions 94 = []
nextPositions 95 = [0]
nextPositions n = [n+1]

-- | Return the position before the given one, when going backwards
previousPosition 0 = 95
previousPosition 95 = 90
previousPosition 71 = 66
previousPosition 47 = 42
previousPosition 23 = 18
previousPosition n = (n-1)

-- | True when the position is inside a parking lane, i.e.
--   [19..22]++[43..46]++[67..70]++[91..94]
parkings = [19..22]++[43..46]++[67..70]++[91..94]
isParking :: Int -> Bool
isParking n = elem n parkings

-- | Given a start position (arg1) and a running count (arg2), returns
--   the possible pathes. All returned pathes, if any, start at arg1
--   and have arg2+1 length.
journeys :: Int -> Int -> [[Int]]
journeys start n = 
    if n >= 0
    then map reverse (journeys' n [[start]])
    else [(fourBack start (-n))]

-- | Given a start position (arg1) and a running count (arg2), returns
--   the wayback path.
fourBack :: Int -> Int -> [Int]
fourBack position 0 = [position]
fourBack position n = if isParking position
    then [position]
    else let before = previousPosition position in
             position:(fourBack before (n-1))

-- | Given a running count and existing pathes, complete the pathes
--   prepending a traversed place each time 
journeys' :: Int -> [[Int]] -> [[Int]]
journeys' _ [] = []
journeys' 0 pathes = pathes
journeys' n pathes = journeys' (n-1) (foldr (++) [] (map allNextJourneys pathes))

-- | Return possible successors of a given set of pathes
allNextJourneys :: [Int] -> [[Int]]
allNextJourneys path =
    let pos = head path
        next = nextPositions pos in
        [p:path | p <- next]


