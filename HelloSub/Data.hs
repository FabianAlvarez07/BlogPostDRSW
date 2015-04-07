module HelloSub.Data where

import Yesod

data HelloSub = HelloSub

mkYesodSubData "HelloSub" [parseRoutes]
/SubHomeR GET
|]
