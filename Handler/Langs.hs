module Handler.Langs where

import Import
import qualified Database.Esqueleto as E
import qualified Data.Text as T
import qualified Data.Time.Format as Time


getLangsR :: Text -> Handler Html
getLangsR slug = do
  Entity _ lang <- runDB $ getBy404 $ UniqueLanguage slug
  articles <- runDB $
       E.select
       $ E.from $ \(article `E.InnerJoin` language) -> do
         E.on $ article E.^. ArticleLang E.==. language E.^. LanguageId
         E.where_ (language E.^. LanguageSlug E.==. E.val slug)
         E.orderBy [ E.desc (article E.^. ArticleCreated) ]
         return (article, language)

  defaultLayout $ do
    setTitle $ toHtml $ T.append ("Articles - " :: Text) (languageName lang)
    $(widgetFile "langs")
