module Handler.Article where

import Import
import qualified Database.Esqueleto as E
import qualified Data.Text as T

githubSourcePrefix :: Text
githubSourcePrefix =
  T.pack "https://github.com/builtinnya/lambdar-website/blob/master/"

githubHistoryPrefix :: Text
githubHistoryPrefix =
  T.pack "https://github.com/builtinnya/lambdar-website/commits/master/"

githubSource :: Text -> Text
githubSource path =
  T.append githubSourcePrefix path

githubHistory :: Text -> Text
githubHistory path =
  T.append githubHistoryPrefix path

disqusShortName :: String -> Text
disqusShortName "ja" = T.pack "lambdar-ja"
disqusShortName "en" = T.pack "lambdar-en"
disqusShortName _ = T.pack "lambdar-en"

getArticleR :: Text -> Text -> Handler Html
getArticleR lang slug = do
  articles <- runDB $
       E.select
       $ E.from $ \(article, language) -> do
         E.where_ (article  E.^. ArticleSlug  E.==. E.val slug
             E.&&. article  E.^. ArticleLang  E.==. language E.^. LanguageId
             E.&&. language E.^. LanguageSlug E.==. E.val lang)
         return (article, language)
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
   (Entity _ article, Entity _ language) : _ ->
     defaultLayout $ do
       master <- getYesod
       let settings = appSettings master
           sourceFileName = articleSourcefile article
           githubSourceUrl = githubSource sourceFileName
           githubHistoryUrl = githubHistory sourceFileName
       setTitle $ toHtml $ articleTitle article
       $(widgetFile "article")
   [] ->
     notFound
