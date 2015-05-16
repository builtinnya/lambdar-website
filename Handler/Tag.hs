module Handler.Tag where

import Import
import qualified Database.Esqueleto as E
import qualified Data.Text as T


getTagR :: Text -> Handler Html
getTagR slug = do
  Entity tId tag <- runDB $ getBy404 $ UniqueTag slug
  articles <- runDB $
       E.select
       $ E.from $ \(article `E.InnerJoin` tagArticle `E.InnerJoin` language) -> do
         E.on $ article E.^. ArticleSlug E.==. tagArticle E.^. TagArticleArticleSlug
         E.on $ article E.^. ArticleLang E.==. language E.^. LanguageId
         E.where_ (tagArticle E.^. TagArticleTag E.==. E.val tId)
         E.orderBy [ E.desc (article E.^. ArticleCreated) ]
         return (article, language)

  defaultLayout $ do
    setTitle $ toHtml $ T.append ("Articles - " :: Text) (tagName tag)
    $(widgetFile "tags")
