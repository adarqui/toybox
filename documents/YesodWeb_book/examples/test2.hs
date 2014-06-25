{-# LANGUAGE OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies #-}

import Yesod

data Links = Links

{-
 instance RenderRoute Links where
     data Route Links = HomeR | Page1R | Page2R
       deriving (Show, Eq, Read)

     renderRoute HomeR  = ([], [])
     renderRoute Page1R = (["page1"], [])
     renderRoute Page2R = (["page2"], [])
-}

mkYesod "Links" [parseRoutes|
/ HomeR GET
/page1 Page1R GET
/page2 Page2R GET
|]

instance Yesod Links

getHomeR = defaultLayout [whamlet|<a href=@{Page1R}>1|]
getPage1R = defaultLayout [whamlet|<a href=@{Page2R}>2|]
getPage2R = defaultLayout [whamlet|<a href=@{HomeR}>Home|]

main :: IO ()
main = do
 print "test1"
 warp 3000 Links
