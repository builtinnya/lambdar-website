module Handler.Article where

import Import
import qualified Database.Esqueleto as E

getArticleR :: Text -> Text -> Handler Html
getArticleR lang slug = do
  articles <- runDB $
       E.select
       $ E.from $ \(article, language) -> do
         E.where_ (article  E.^. ArticleSlug  E.==. E.val slug
             E.&&. article  E.^. ArticleLang  E.==. language E.^. LanguageId
             E.&&. language E.^. LanguageSlug E.==. E.val lang)
         return article
  tags <- runDB $
       E.select
       $ E.from $ \(tag_article, tag) -> do
         E.where_ (tag_article E.^. TagArticleTag         E.==. tag E.^. TagId
             E.&&. tag_article E.^. TagArticleArticleSlug E.==. E.val slug)
         return tag
  langs <- runDB $
       E.select
       $ E.from $ \(article, language) -> do
         E.where_ (article E.^. ArticleSlug E.==. E.val slug
             E.&&. article E.^. ArticleLang E.==. language E.^. LanguageId)
         return language
  case articles of
   Entity _ article : _ ->
     defaultLayout $ do
       setTitle $ toHtml $ articleTitle article
       $(widgetFile "article")
   [] ->
     notFound
