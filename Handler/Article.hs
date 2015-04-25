module Handler.Article where

import Import
import qualified Database.Esqueleto as E

getArticleR :: Text -> Text -> Handler Html
getArticleR lang slug = do
  res <- runDB $
       E.select
       $ E.from $ \(article, language) -> do
         E.where_ (article  E.^. ArticleSlug  E.==. E.val slug
             E.&&. article  E.^. ArticleLang  E.==. language E.^. LanguageId
             E.&&. language E.^. LanguageSlug E.==. E.val lang)
         return (article, language)
  case res of
   (Entity _ article, Entity _ language) : _ ->
     defaultLayout $ do
       setTitle $ toHtml $ articleTitle article
       $(widgetFile "article")
   [] ->
     notFound
