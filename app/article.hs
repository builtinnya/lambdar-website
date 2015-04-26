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
import Data.Time (UTCTime, getCurrentTime)
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


timestamp :: UTCTime -> [ Update Article ] -> [ Update Article ]
timestamp now [] = []
timestamp now updates = ( ArticleUpdated =. now ) : updates

githubMarkdownToHtml :: String -> Html
githubMarkdownToHtml =
  (writeHtml def) . readMarkdown def { readerExtensions = githubMarkdownExtensions }

htmlToSHA1Text :: Html -> T.Text
htmlToSHA1Text html = T.pack $ show $ (hashlazy (renderHtml html) :: Digest SHA1)

getArticle :: Database -> String -> String -> IO (Maybe (Entity Article))
getArticle db lang slug = do
  runSqlite db $ do
    res <- E.select
         $ E.from $ \(article, language) -> do
           E.where_ (article  ^. ArticleSlug  E.==. E.val (T.pack slug)
               E.&&. article  ^. ArticleLang  E.==. language ^. LanguageId
               E.&&. language ^. LanguageSlug E.==. E.val (T.pack lang))
           return article
    case res of
     []    -> return $ Nothing
     a : _ -> return $ Just a

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

addArticle :: Database -> String -> String -> String -> String -> IO ()
addArticle db lang slug title sourcefile = do
  withFile sourcefile ReadMode (\handle -> do
    now <- getCurrentTime
    contents <- hGetContents handle
    let html = githubMarkdownToHtml contents
        hashVal = htmlToSHA1Text html
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
                           , articleHash       = hashVal }
      articleId <- insert entity
      liftIO $ putStrLn $ "Article (" ++ lang ++ ", " ++ slug ++ ") has been added."
      return ()
    `catch` \(e :: SomeException) -> putStrLn $ show e)

updateArticle :: Database -> String -> String -> Maybe String -> Maybe String -> IO ()
updateArticle db lang slug title tags = do
  now <- getCurrentTime
  runSqlite db $ do
    res <- liftIO $ getArticle db lang slug
    case res of
     Just (Entity articleId _) -> do
       let updates = map fromJust $ filter isJust $
                     [ title >>= \t -> Just $ ArticleTitle   =. (T.pack t) ]
       update articleId (timestamp now updates)
       liftIO $ putStrLn $ "Article (" ++ lang ++ ", " ++ slug ++ ") has been updated."
     Nothing -> do
       liftIO $ putStrLn $ "Article (" ++ lang ++ ", " ++ slug ++ ") not found."
    return ()
  `catch` \(e :: SomeException) -> putStrLn $ show e

contentArticle :: Database -> String -> String -> Maybe String -> IO ()
contentArticle db lang slug _sourcefile = do
  now <- getCurrentTime
  res <- getArticle db lang slug
  case res of
   Just (Entity articleId article) -> do
     let (sfChanged, sourcefile) = getSourcefile article _sourcefile

     withFile (T.unpack sourcefile) ReadMode ( \handle -> do
       contents <- hGetContents handle

       let html = githubMarkdownToHtml contents
           hashVal = htmlToSHA1Text html
           contentChanged = hashVal /= articleHash article

       runSqlite db $ do
           update articleId $
             if contentChanged
               then (timestamp now [ ArticleContent    =. html
                                   , ArticleHash       =. hashVal
                                   , ArticleUpdated    =. now
                                   , ArticleSourcefile =. sourcefile ])
               else [ ArticleSourcefile =. sourcefile ]
                    -- For the case that we just want to change the source file's name.
                    -- It's internal so don't update updated.
           return ()

       printIfSourcefileNameChanged sfChanged sourcefile
       printIfContentChanged contentChanged )

     where printIfContentChanged contentChanged
             | contentChanged =
                 putStrLn $ "Article content has been updated."
             | otherwise =
                 putStrLn $ "Article content has not been changed."

           printIfSourcefileNameChanged sfChanged sourcefile
             | sfChanged  =
                 putStrLn $ "Article source file -> " ++ (T.unpack sourcefile)
             | otherwise  =
                 return ()

           getSourcefile article _sourcefile =
             let sourcefile = articleSourcefile article
             in case _sourcefile of
                 Nothing -> (False, articleSourcefile article)
                 Just f  -> (sourcefile /= (T.pack f), T.pack f)
   Nothing -> do
     putStrLn $ "Couldn't find the article"
  `catch` \(e :: SomeException) -> putStrLn $ show e

deleteArticle :: Database -> String -> String -> IO ()
deleteArticle db lang slug = do
  runSqlite db $ do
    res <- liftIO $ getArticle db lang slug
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
