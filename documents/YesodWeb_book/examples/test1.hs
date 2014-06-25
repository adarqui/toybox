{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}

import Yesod

data HelloWorld = HelloWorld

mkYesod "HelloWorld" [parseRoutes|
/ HomeR GET
|]

instance Yesod HelloWorld

getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|Hello World!|]

main :: IO ()
main = do
 print "test1"
 warp 3000 HelloWorld
