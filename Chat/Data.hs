-- @Chat/Data.hs
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Chat.Data where

import           Prelude(IO,($),Bool(..),Maybe(..),Monad(..))
import           Blaze.ByteString.Builder.Char.Utf8  (fromText)
import           Control.Concurrent.Chan
import           Data.Monoid                         ((<>))
import           Data.Text                           (Text)
import           Network.Wai.EventSource
import           Network.Wai.EventSource.EventStream
import           Yesod

-- | Our subsite foundation. We keep a channel of events that all connections
-- will share.
data Chat = Chat (Chan ServerEvent)

mkYesodSubData "Chat" [parseRoutes|
/send SendR POST
/recv ReceiveR GET
|]

class (Yesod master, RenderMessage master FormMessage)
        => YesodChat master where
    getUserName :: HandlerT master IO Text
    isLoggedIn :: HandlerT master IO Bool

type ChatHandler a =
    forall master. YesodChat master =>
    HandlerT Chat (HandlerT master IO) a

postSendR :: ChatHandler ()
postSendR = do
    from <- lift getUserName
    body <- lift $ runInputGet $ ireq textField "message"
    Chat chan <- getYesod
    liftIO $ writeChan chan $ ServerEvent Nothing Nothing $ return $
        fromText from <> fromText ": " <> fromText body


getReceiveR :: ChatHandler ()
getReceiveR = do
    Chat chan0 <- getYesod
    chan <- liftIO $ dupChan chan0
    sendWaiApplication $ eventSourceAppChan chan
