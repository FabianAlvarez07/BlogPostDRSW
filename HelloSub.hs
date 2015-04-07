-- @HelloSub.hs
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
module HelloSub (
       module HelloSub.Data,
       module HelloSub
       ) where

import HelloSub.Data
import Yesod
import Prelude (IO,($))

-- And we'll spell out the handler type signature.
getSubHomeR :: Yesod master => HandlerT HelloSub (HandlerT master IO) Html
getSubHomeR = lift $ defaultLayout [whamlet|Welcome to the subsite!|]

instance Yesod master => YesodSubDispatch HelloSub (HandlerT master IO) where
    yesodSubDispatch = $(mkYesodSubDispatch resourcesHelloSub)
