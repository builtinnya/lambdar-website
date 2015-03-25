module Handler.Echo where

import Import
import Data.Text

getEchoR :: Text -> Handler Html
getEchoR theText = defaultLayout $(widgetFile "echo")
