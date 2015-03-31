module Handler.Article where

import Import

getArticleR :: Text -> Text -> Handler Html
getArticleR lang slug = do
    Entity _ article <- runDB $ getBy404 $ UniqueArticle lang slug
    defaultLayout $ do
        setTitle $ toHtml $ articleTitle article
        $(widgetFile "article")
