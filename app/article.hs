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
import Data.Char (isSpace)
import Data.List (dropWhile, dropWhileEnd, intercalate)
import Data.List.Split (splitOn)
import qualified Data.Set as S

data Options = Options
               { _production :: Bool
               , optCommand :: Command}
             deriving (Show, Eq)

data Command = AddCmd     { lang :: String
                          , slug :: String
                          , title :: String
                          , sourcefile :: String }
             | ListCmd
             | UpdateCmd  { lang :: String
                          , slug :: String
                          , _title :: Maybe String }
             | ContentCmd { lang :: String
                          , slug :: String
                          , _sourcefile :: Maybe String }
             | DeleteCmd  { lang :: String
                          , slug :: String }
             | TagCmd     { slug :: String
                          , _tags :: Maybe String }
             | UntagCmd   { slug :: String
                          , _tags :: Maybe String }
             deriving (Show, Eq)

type Database = T.Text


timestamp :: UTCTime -> [ Update Article ] -> [ Update Article ]
timestamp now [] = []
timestamp now updates = ( ArticleUpdated =. now ) : updates

githubMarkdownToHtml :: String -> Html
githubMarkdownToHtml markdown =
  case readMarkdown def { readerExtensions = (S.insert
Ext_footnotes githubMarkdownExtensions) } markdown of
    Left err  -> error "cannot read markdown"
    Right doc -> writeHtml def doc

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

updateArticle :: Database -> String -> String -> Maybe String -> IO ()
updateArticle db lang slug title = do
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

getTagSlugs :: String -> [String]
getTagSlugs tags = map (dropWhileEnd isSpace . dropWhile isSpace) $ splitOn "," tags

listArticleTags :: Database -> String -> IO ()
listArticleTags db articleSlug = do
  runSqlite db $ do
    tags <- E.select
          $ E.from $ \(tagArticle, tag) -> do
            E.where_ (tagArticle ^. TagArticleArticleSlug E.==. E.val (T.pack articleSlug)
                E.&&. tagArticle ^. TagArticleTag         E.==. tag ^. TagId)
            return tag
    liftIO $ TIO.putStr header
    liftIO $ TIO.putStr $ T.unlines $ map showTag tags
    return ()
  where
    colLens = [ 15, 15, 20 ]
    colNames = map T.pack [ "Slug", "Name", "Description" ]
    header =
      let centerCol (l, t) = T.center l ' ' t
          cols = T.intercalate (T.pack "|") $ map centerCol $ zip colLens colNames
          sep = T.replicate ((sum colLens) + (length colLens) - 1) (T.pack "-") in
       T.unlines [ cols, sep ]
    showTag (Entity _ (Tag s n d)) =
      let sep = T.pack "|"
          justifyCol (l, t) = T.justifyRight l ' ' (T.snoc t ' ') in
       T.intercalate (T.pack "|") $ map justifyCol $ zip colLens [ s, n, d ]

tagArticle :: Database -> String -> Maybe String -> IO ()
tagArticle db slug _tags = do
  case _tags of
   Just _tags -> do
     let tagSlugs = getTagSlugs _tags
     runSqlite db $ do
       articles <- selectList [ ArticleSlug ==. (T.pack slug) ] []
       if null articles
         then liftIO $ putStrLn $ "No article (" ++ slug ++ ") exists"
         else do
           tags <- selectList [ FilterOr $ map ((==.) TagSlug) (map T.pack tagSlugs) ] []
           insertMany_ $ map (createTagArticle slug) tags
           let attachedTags = map (\(Entity _ tag) -> T.unpack $ tagSlug tag) tags
           liftIO $ printResultMsg attachedTags

     where createTagArticle articleSlug (Entity tagId _) =
             TagArticle tagId $ (T.pack articleSlug)

           printResultMsg attachedTags = case attachedTags of
             [] ->
               putStrLn $ "No tags attached (no specified article or tag exists)."
             tags ->
               putStrLn $ "Tags (" ++ (intercalate ", " tags) ++ ")" ++ " has been attached to " ++ slug ++ "."

   Nothing -> do
     listArticleTags db slug

untagArticle :: Database -> String -> Maybe String -> IO ()
untagArticle db slug _tags = do
  case _tags of
   Just _tags -> do
     let tagSlugs = getTagSlugs _tags
     runSqlite db $ do
       articles <- selectList [ ArticleSlug ==. (T.pack slug) ] []
       if null articles
          then liftIO $ putStrLn $ "No article (" ++ slug ++ ") exists"
          else do
            tags <- selectList [ FilterOr $ map ((==.) TagSlug) (map T.pack tagSlugs) ] []
            mapM_ deleteBy $ map createTagArticle tags
            let detachedTags = map (\(Entity _ tag) -> T.unpack $ tagSlug tag) tags
            liftIO $ printResultMsg detachedTags

     where createTagArticle (Entity tagId _) =
             UniqueTagArticle tagId (T.pack slug)

           printResultMsg detachedTags = case detachedTags of
             [] ->
               putStrLn $ "No tags detached (no specified tag attached to the article)"
             tags ->
               putStrLn $ "Tags (" ++ (intercalate ", " tags) ++ ")" ++ " has been detached from " ++ slug ++ "."

   Nothing -> do
     listArticleTags db slug

main :: IO ()
main = do
  -- Parse arguments
  o <- execParser optParser'

  let configSettings =
        if _production o
        then "config/production-settings.yml" : [configSettingsYml]
        else [configSettingsYml]

  -- Get the settings from all relevant sources
  settings <- loadAppSettings
              -- configuration files
              configSettings

              -- no other values to use
              []

              -- allow environment variables to override
              useEnv

  -- Get the database file from the settings
  let db = sqlDatabase $ appDatabaseConf settings

  -- Run database migration
  runSqlite db $ do
    runMigration migrateAll

  case optCommand o of
   AddCmd     lang slug title sourcefile -> addArticle db lang slug title sourcefile
   ListCmd                               -> listArticles db
   UpdateCmd  lang slug _title           -> updateArticle db lang slug _title
   ContentCmd lang slug _sourcefile      -> contentArticle db lang slug _sourcefile
   DeleteCmd  lang slug                  -> deleteArticle db lang slug
   TagCmd     slug _tags                 -> tagArticle db slug _tags
   UntagCmd   slug _tags                 -> untagArticle db slug _tags

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

tagOptions :: Parser Command
tagOptions = TagCmd
        <$> argument str
            ( metavar "SLUG"
           <> help "The slug for the article" )
        <*> ( optional $ strOption
              ( long "tags"
             <> metavar "TAGS"
             <> help "Comma-separated list of tag slugs to be attached" ) )

untagOptions :: Parser Command
untagOptions = UntagCmd
        <$> argument str
            ( metavar "SLUG"
           <> help "The slug for the article" )
        <*> ( optional $ strOption
              ( long "tags"
             <> metavar "TAGS"
             <> help "Comma-separated list of tag slugs to be detached" ) )

optParser' :: ParserInfo Options
optParser' = info (helper <*> optParser) ( fullDesc <> header "Lambdar command-line manager" )

optParser :: Parser Options
optParser = Options
        <$> switch ( long "production"
                  <> help "Whether in production mode" )
        <*> subparser ( command "add"
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
                           <> progDesc "Delete an article" ))
                     <> command "tag"
                           (info tagOptions
                            ( fullDesc
                           <> progDesc "Attach tags to an article" ))
                     <> command "untag"
                           (info untagOptions
                            ( fullDesc
                           <> progDesc "Detach tags from an article" )) )
