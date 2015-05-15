module Handler.Home where

import Import
import qualified Database.Esqueleto as E
import qualified Data.Text as T

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Html
getHomeR = do
  articles <- runDB $
       E.select
       $ E.from $ \(article `E.InnerJoin` language) -> do
         E.on $ article E.^. ArticleLang E.==. language E.^. LanguageId
         E.orderBy [ E.desc (article E.^. ArticleUpdated) ]
         E.limit 5
         return (article, language)

  defaultLayout $ do
    master <- getYesod
    let settings = appSettings master
    setTitle "Lambdar"
    $(widgetFile "homepage")
