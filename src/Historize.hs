{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (forM_)
import System.Environment(getArgs)
import Data.List (intercalate, sort)

-- import Server
import Game
import Storage

-- | Tock server application
main = do
    -- e.g. ["test/9_stupid_sevens.db"]
    args <- getArgs
    case args of
        "-db":argfile:[]   -> mainDB argfile
        argfile:[]         -> mainDB argfile
        "-game":argfile:[] -> mainREPR argfile
        _ -> putStrLn ("Usage: Historize [-db] sqlitefile.db\n" ++
                       "       Historize -game representation\n")

mainDB dbfile = do
    connection <- connectTo dbfile
    games <- listGames connection
    processGames games connection

mainREPR gamefile = do
    repr <- readFile gamefile
    let game = (read repr) :: Game
        name = gameName game
        nospace [] = []
        nospace (' ':cs) = '_':(nospace cs)
        nospace (c:cs) = c:(nospace cs)
        filename = "history_" ++ (nospace name) ++ ".html"
        in replay game filename

stateOf game@(Game n c p d h) = noreg' h where
    noreg ((Registration p n):hs) = noreg hs
    noreg (h:hs) = h
    noreg [] = Registering
    noreg' h = visible $ noreg h
    visible (Exchange who _) = (show who) ++ " exchanged"
    visible h = show h

processGames games connection = forM_ games (processGame connection)
processGame connection row =
    let name = head row
        nospace [] = []
        nospace (' ':cs) = '_':(nospace cs)
        nospace (c:cs) = c:(nospace cs)
        filename = "history_" ++ (nospace name) ++ ".html"
        creation = head (tail row) in do
    game <- readGame connection creation name
    putStrLn ("Replaying game = [" 
            ++ (gameName game) ++ "], pc = ["
            ++ (stateOf game) ++ "], history = [#"
            ++ (show (length (gameHistory game))) 
            ++ "], to file = [" ++ filename ++ "]")
    replay game filename

replay :: Game -> String -> IO()
replay game filename = do
    putStrLn (">> Writing " ++ filename)
    writeFile filename (storyboard game)
    putStrLn ("<< Written " ++ filename)

storyboard :: Game -> String
storyboard one@(Game n c p d h) = let
    body :: String
    body = "<main>" ++ (intercalate "\n" slides) ++ "</main>"
    slides :: [String]
    slides = applyall' (reverse h) virgin
    applyall' :: [History] -> Game -> [String]
    applyall' h g = map repr $ applyall h g
    repr :: (Game,History) -> String
    repr (game,event) = ("<pre>"++(textBoardFor game)++"\n\nNext = "
        ++(eventSimple event)++"\n\n</pre>")
    eventSimple (MoveEvent p m) = "["++(show p)++"] "++(show m)
    eventSimple m = show m
    applyall :: [History] -> Game -> [(Game,History)]
    applyall [] game = [(game,(Playing South))]
    applyall (e:es) game = (game,e):(
        applyall es (applyHistory e game))
    applyHistory :: History -> Game -> Game
    applyHistory e game = historize e $ applied e game
    historize :: History -> Game -> Game
    historize h g@(Game ng cg pg dg hs) = Game ng cg pg dg (h:hs)
    applied :: History -> Game -> Game
    applied (MoveEvent p m) game = applyMove (playerAt game p) m game
    applied _ game = game
    virgin :: Game
    virgin = Game n c (map registered p) [] []
    registered player@(Player n po h e pa) =
        Player n po [] Nothing [Out,Out,Out,Out]
    script = intercalate "\n" [ 
        "    function _(selector) {",
        "        return [].map.call(document.querySelectorAll(selector), ",
        "                function(x) { return x; });",
        "    }",
        "    function hidePres(except) {",
        "        var pres = _('pre');",
        "        pres.forEach(function(pre) {",
        "            pre.style.display = 'none';",
        "        });",
        "        pres[except || 0].style.display = '';",
        "    }",
        "    var current = 0;",
        "    function prev() {",
        "        current = Math.max(0, current-1);",
        "        hidePres(current);",
        "    }",
        "    function next() {",
        "        current = Math.min(_('pre').length-1, current+1);",
        "        hidePres(current);",
        "    }",
        "    window.onload = function() {",
        "        hidePres();",
        "        window.addEventListener('keypress', function(event) {",
        "            var c = event.keyCode;",
        "            if(37 == c) {",
        "                prev();",
        "            } else if(39 == c) {",
        "                next();",
        "            }",
        "        });",
        "    };"
        ]
    in ("<html><head>\n<script type='text/javascript'>\n"
        ++ script ++ "\n</script>\n</head>\n<body>" 
        ++ body ++ "</body></html>")

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
    prepend pos n =
        "<span style='color:"++(color pos)++";'>"++text++"</span>" where
        color South = "green"
        color West = "yellow"
        color North = "red"
        color East = "blue"
        text = prepend' pos n
    prepend' pos n = (head (show pos)):(show n)
    pos2label'' :: [(String,Pawn)] -> [(Int,String)]
    pos2label'' [] = []
    pos2label'' ((label,Out):xs) = pos2label'' xs
    pos2label'' ((label,pawn):xs) = (pawnPosition pawn, label):pos2label'' xs



