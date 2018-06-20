{-# LANGUAGE OverloadedStrings #-}

module Requests where

import Network.Wai
import Network.HTTP.Types.Status
import Game
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.List (intercalate)


type Answer     = IO ResponseReceived
type Responder  = Response -> Answer


statichtml :: Responder -> T.Text -> Answer
statichtml respond name =
    let filepath = intercalate "/" ("www":(T.unpack name):[]) in
        respond (responseFile status200 [
            ("Content-Type","text/html;charset=utf-8")
            ] filepath Nothing)

font :: Responder -> BS.ByteString -> T.Text -> Answer
font respond mimetype name =
    let filepath = intercalate "/" ("www":"fonts":(T.unpack name):[]) in
        respond (responseFile status200 [
            ("Content-Type",mimetype)
            ] filepath Nothing)

img :: Responder -> [T.Text] -> Answer
img respond subpath =
    static "img" "image/png" respond subpath

svg :: Responder -> [T.Text] -> Answer
svg respond subpath =
    static "img" "image/svg+xml" respond subpath

png :: Responder -> [T.Text] -> Answer
png respond subpath =
    static "img" "image/png" respond subpath

js :: Responder -> [T.Text] -> Answer
js respond subpath =
   static "js" "text/javascript;charset=utf-8" respond subpath

css :: Responder -> [T.Text] -> Answer
css respond subpath =
    static "css" "text/css;charset=utf-8" respond subpath

static :: FilePath -> BS.ByteString -> Responder -> [T.Text] -> Answer
static dir ctype respond subpath =
    let parts = map T.unpack subpath
        fullparts = "www":dir:parts
        filepath = intercalate "/" fullparts in
        respond (responseFile status200 [
            ("Content-Type",ctype)
            ] filepath Nothing)

badrequest :: Responder -> Answer
badrequest respond =
    respond (responseLBS status400 [] "Bad request")

badrequest2 :: Responder -> String -> Answer
badrequest2 respond message =
    respond (responseLBS status400 [] (LBS.pack ("Bad request: " ++ message)))

seeother :: Responder -> String -> Answer
seeother respond location =
    respond (responseLBS status303 [("Location",
        (BS.pack location))] "See other")

seeothercookie :: Responder -> String -> Position -> Answer
seeothercookie respond location position =
    respond (responseLBS status303 [
            ("Location", (BS.pack location)),
            ("Set-Cookie", (cookieFor position))
        ] "See other")
    where
        cookieFor position = BS.pack ("position=" ++ (show position))

notfound :: Responder -> Answer
notfound respond =
    respond (responseLBS status404 [] "Not found")



