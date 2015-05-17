import Prelude
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

data Options = Options
               { _production :: Bool
               , optCommand :: Command }
             deriving (Show, Eq)

data Command = AddCmd    { slug :: String
                         , name :: String }
             | ListCmd
             | UpdateCmd { slug :: String
                         , name :: String }
             | DeleteCmd { slug :: String }
             deriving (Show, Eq)

type Database = T.Text

listLangs :: Database -> IO ()
listLangs db = do
  runSqlite db $ do
    langs <- selectList [] [ Asc LanguageSlug ]
    liftIO $ TIO.putStr header
    liftIO $ TIO.putStr $ T.unlines $ map showLang langs
    return ()
  where
    colLen1 = 5
    colLen2 = 15
    header =
      let h1 = T.justifyLeft colLen1 ' ' (T.pack "Slug")
          h2 = T.center colLen2 ' ' (T.pack "Name")
          colNames = T.intercalate (T.pack "|") [ h1, h2 ]
          sep = T.replicate (colLen1 + colLen2 + 1) (T.pack "-") in
       T.unlines [ colNames, sep ]
    showLang (Entity _ (Language slug name)) =
      let sep = T.pack "|"
          s = T.justifyLeft colLen1 ' ' slug
          n = T.justifyRight colLen2 ' ' name in
       T.intercalate sep [ s, n ]

addLang :: Database -> String -> String -> IO ()
addLang db slug name = do
  let entity = Language (T.pack slug) (T.pack name)
  runSqlite db $ do
    LanguageKey _ <- insert entity
    liftIO $ putStrLn $ "Language (" ++ slug ++ ") has been added."
    return ()
  `catch` \(e :: SomeException) -> putStrLn $ show e

updateLang :: Database -> String -> String -> IO ()
updateLang db slug name = do
  runSqlite db $ do
    res <- getBy $ UniqueLanguage (T.pack slug)
    case res of
     Just (Entity langId _) -> do
       update langId [ LanguageName =. (T.pack name) ]
       liftIO $ putStrLn $ "Language (" ++ slug ++ ") has been updated."
     Nothing -> do
       liftIO $ putStrLn $ "Language (" ++ slug ++ ") not found."
    return ()
  `catch` \(e :: SomeException) -> putStrLn $ show e

deleteLang :: Database -> String -> IO ()
deleteLang db slug = do
  runSqlite db $ do
    res <- getBy $ UniqueLanguage (T.pack slug)
    case res of
     Just (Entity langId _) -> do
       delete langId
       liftIO $ putStrLn $ "Language (" ++ slug ++ ") has been deleted."
     Nothing -> do
       liftIO $ putStrLn $ "Language (" ++ slug ++ ") not found."
    return ()
  `catch` \(e :: SomeException) -> putStrLn $ show e

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
   AddCmd    slug name -> addLang    db slug name
   ListCmd             -> listLangs  db
   UpdateCmd slug name -> updateLang db slug name
   DeleteCmd slug      -> deleteLang db slug

addOptions :: Parser Command
addOptions = AddCmd
        <$> argument str
            ( metavar "SLUG"
           <> help "The slug for the language" )
        <*> argument str
            ( metavar "NAME"
           <> help "The display name of the language" )

listOptions :: Parser Command
listOptions = pure ListCmd

updateOptions :: Parser Command
updateOptions = UpdateCmd
        <$> argument str
            ( metavar "SLUG"
           <> help "The slug for the language" )
        <*> argument str
            ( metavar "NAME"
           <> help "The display name of the language" )

deleteOptions :: Parser Command
deleteOptions = DeleteCmd
        <$> argument str
            ( metavar "SLUG"
           <> help "The slug for the language" )

optParser' :: ParserInfo Options
optParser' = info (helper <*> optParser) ( fullDesc <> header "Lambdar command-line manager" )

optParser :: Parser Options
optParser = Options
        <$> switch ( long "production"
                  <> help "Whether in production mode" )
        <*> subparser ( command "add"
                           (info addOptions
                            ( fullDesc
                           <> progDesc "Add a new language named NAME for SLUG" ))
                     <> command "list"
                           (info listOptions
                            ( fullDesc
                           <> progDesc "List languages"))
                     <> command "update"
                           (info updateOptions
                            ( fullDesc
                           <> progDesc "Update the name of the language SLUG" ))
                     <> command "delete"
                           (info deleteOptions
                            ( fullDesc
                           <> progDesc "Delete the language SLUG" )) )
