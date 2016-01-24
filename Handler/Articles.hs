module Handler.Articles where

import Import
import qualified Database.Esqueleto as E
import qualified Data.Text as T
import qualified Data.Time.Format as Time

getArticlesR :: Handler Html
getArticlesR = do
  articles <- runDB $
       E.select
       $ E.from $ \(article `E.InnerJoin` language) -> do
         E.on $ article E.^. ArticleLang E.==. language E.^. LanguageId
         E.orderBy [ E.desc (article E.^. ArticleCreated) ]
         return (article, language)

  defaultLayout $ do
    setTitle $ toHtml $ T.pack "Articles"
    $(widgetFile "articles")
