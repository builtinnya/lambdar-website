import Prelude
import System.IO
import Data.Maybe
import Control.Monad.IO.Class
import Control.Exception
import Database.Persist
import Database.Persist.Sqlite
import Database.Sqlite as Sqlite
import Database.Persist.Sql (SqlPersistM, SqlBackend, runSqlPersistMPool, rawExecute, rawSql, unSingle, connEscapeName)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Model
import Yesod.Default.Config2 (useEnv, configSettingsYml, loadAppSettings)
import qualified Database.Sqlite as Sqlite
import Control.Monad.Logger (runLoggingT)
import Settings (appDatabaseConf)
import Yesod.Core (messageLoggerSource)
import Yesod.Persist.Core (runDB)
import Options.Applicative
import Text.Pandoc
import Text.Blaze.Html
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Data.Time (getCurrentTime)
import Crypto.Hash
import qualified Database.Esqueleto as E
import Database.Esqueleto ((^.))

data Options = Options
               { optCommand :: Command }
             deriving (Show, Eq)

data Command = AddCmd     { lang :: String
                          , slug :: String
                          , title :: String
                          , sourcefile :: String }
             | ListCmd
             | UpdateCmd  { lang :: String
                          , slug :: String
                          , _title :: Maybe String
                          , _tags :: Maybe String }
             | ContentCmd { lang :: String
                          , slug :: String
                          , _sourcefile :: Maybe String }
             | DeleteCmd  { lang :: String
                          , slug :: String }
             deriving (Show, Eq)

type Database = T.Text

listArticles :: Database -> IO ()
listArticles db = do
  runSqlite db $ do
    articles <- E.select
              $ E.from $ \(article `E.InnerJoin` language) -> do
                  E.on $ article ^. ArticleLang E.==. language ^. LanguageId
                  return ( language ^. LanguageSlug
                         , article  ^. ArticleSlug
                         , article  ^. ArticleTitle
                         , article  ^. ArticleSourcefile
                         , article  ^. ArticleUpdated
                         )
    liftIO $ TIO.putStr header
    liftIO $ TIO.putStr $ T.unlines $ map showArticle articles
    return ()
  where
    colLens = [ 6, 15, 20, 20, 20 ]
    colNames = map T.pack [ "Lang", "Slug", "Title", "Source File", "Updated" ]
    header =
      let centerCol (l, t) = T.center l ' ' t
          cols = T.intercalate (T.pack "|") $ map centerCol $ zip colLens colNames
          sep = T.replicate ((sum colLens) + (length colLens) - 1) (T.pack "-") in
       T.unlines [ cols, sep ]
    showArticle ((E.Value l), (E.Value s), (E.Value t), (E.Value f), (E.Value u)) =
      let sep = T.pack "|"
          justifyCol (l, t) = T.justifyRight l ' ' (T.snoc t ' ') in
       T.intercalate (T.pack "|") $ map justifyCol $ zip colLens [ l, s, t, f, (T.pack . show) u ]

githubMarkdownToHtml :: String -> Html
githubMarkdownToHtml =
  (writeHtml def) . readMarkdown def { readerExtensions = githubMarkdownExtensions }

addArticle :: Database -> String -> String -> String -> String -> IO ()
addArticle db lang slug title sourcefile = do
  withFile sourcefile ReadMode (\handle -> do
    now <- getCurrentTime
    contents <- hGetContents handle
    let html = githubMarkdownToHtml contents
        hashVal = show $ (hashlazy (renderHtml html) :: Digest SHA1)
    runSqlite db $ do
      Just (Entity langId _) <- getBy $ UniqueLanguage (T.pack lang)
      let entity = Article { articleLang       = langId
                           , articleSlug       = T.pack slug
                           , articleTitle      = T.pack title
                           , articleSourcefile = T.pack sourcefile
                           , articleCreated    = now
                           , articleUpdated    = now
                           , articleViews      = 0
                           , articleContent    = html
                           , articleHash       = T.pack hashVal }
      articleId <- insert entity
      liftIO $ putStrLn $ "Article (" ++ lang ++ ", " ++ slug ++ ") has been added."
      return ()
    `catch` \(e :: SomeException) -> putStrLn $ show e)

updateArticle :: Database -> String -> String -> Maybe String -> Maybe String -> IO ()
updateArticle db lang slug title tags = do
  runSqlite db $ do
    Just (Entity langId _) <- getBy $ UniqueLanguage (T.pack lang)
    res <- getBy $ UniqueArticle langId (T.pack slug)
    case res of
     Just (Entity articleId _) -> do
       let updates = map fromJust $ filter isJust $
                     [ title >>= \t -> Just $ ArticleTitle =. (T.pack t) ]
       update articleId updates
       liftIO $ putStrLn $ "Article (" ++ lang ++ ", " ++ slug ++ ") has been updated."
     Nothing -> do
       liftIO $ putStrLn $ "Article (" ++ lang ++ ", " ++ slug ++ ") not found."
    return ()
  `catch` \(e :: SomeException) -> putStrLn $ show e

contentArticle :: Database -> String -> String -> Maybe String -> IO ()
contentArticle db lang slug sourcefile = do
  putStrLn $ "Not implemented"

deleteArticle :: Database -> String -> String -> IO ()
deleteArticle db lang slug = do
  runSqlite db $ do
    Just (Entity langId _) <- getBy $ UniqueLanguage (T.pack lang)
    res <- getBy $ UniqueArticle langId (T.pack slug)
    case res of
     Just (Entity articleId _) -> do
       delete articleId
       liftIO $ putStrLn $ "Article (" ++ lang ++ ", " ++ slug ++ ") has been deleted."
     Nothing -> do
       liftIO $ putStrLn $ "Article (" ++ lang ++ ", " ++ slug ++ ") not found."
    return ()
  `catch` \(e :: SomeException) -> putStrLn $ show e

main :: IO ()
main = do
  -- Get the settings from all relevant sources
  settings <- loadAppSettings
              -- @config/settings.yml@
              [configSettingsYml]

              -- no other values to use
              []

              -- allow environment variables to override
              useEnv

  -- Get the database file from the settings
  let db = sqlDatabase $ appDatabaseConf settings

  -- Run database migration
  runSqlite db $ do
    runMigration migrateAll

  -- Parse arguments
  o <- execParser optParser'
  case optCommand o of
   AddCmd     lang slug title sourcefile -> addArticle db lang slug title sourcefile
   ListCmd                               -> listArticles db
   UpdateCmd  lang slug _title _tags     -> updateArticle db lang slug _title _tags
   ContentCmd lang slug _sourcefile      -> contentArticle db lang slug _sourcefile
   DeleteCmd  lang slug                  -> deleteArticle db lang slug

addOptions :: Parser Command
addOptions = AddCmd
        <$> argument str
            ( metavar "LANG"
           <> help "The language of the article" )
        <*> argument str
            ( metavar "SLUG"
           <> help "The slug for the article" )
        <*> argument str
            ( metavar "TITLE"
           <> help "The title of the article" )
        <*> argument str
            ( metavar "SOURCEFILE"
           <> help "The source file of the article" )

listOptions :: Parser Command
listOptions = pure ListCmd

updateOptions :: Parser Command
updateOptions = UpdateCmd
        <$> argument str
            ( metavar "LANG"
           <> help "The language of the article" )
        <*> argument str
            ( metavar "SLUG"
           <> help "The slug for the article" )
        <*> ( optional $ strOption
              ( long "title"
             <> metavar "TITLE"
             <> help "The new title of the article" ) )
        <*> ( optional $ strOption
              ( long "tags"
             <> metavar "TAGS"
             <> help "The new tags of the article" ) )

contentOptions :: Parser Command
contentOptions = ContentCmd
        <$> argument str
            ( metavar "LANG"
           <> help "The language of the article" )
        <*> argument str
            ( metavar "SLUG"
           <> help "The slug for the article" )
        <*> ( optional $ strOption
              ( long "sourcefile"
             <> metavar "SOURCEFILE"
             <> help "A new source file of the article" ) )

deleteOptions :: Parser Command
deleteOptions = DeleteCmd
        <$> argument str
            ( metavar "LANG"
           <> help "The language of the article" )
        <*> argument str
            ( metavar "SLUG"
           <> help "The slug for the article" )

optParser' :: ParserInfo Options
optParser' = info (helper <*> optParser) ( fullDesc <> header "Lambdar command-line manager" )

optParser :: Parser Options
optParser = Options
        <$> subparser ( command "add"
                           (info addOptions
                            ( fullDesc
                           <> progDesc "Add a new article" ))
                     <> command "list"
                           (info listOptions
                            ( fullDesc
                           <> progDesc "List articles"))
                     <> command "update"
                           (info updateOptions
                            ( fullDesc
                           <> progDesc "Update metadata of an article" ))
                     <> command "content-update"
                           (info contentOptions
                            ( fullDesc
                           <> progDesc "Update article content" ))
                     <> command "delete"
                           (info deleteOptions
                            ( fullDesc
                           <> progDesc "Delete an article" )) )
