import Prelude
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

data Options = Options
               { optCommand :: Command }
             deriving (Show, Eq)

data Command = AddCmd    { slug :: String
                         , name :: String
                         , _description :: Maybe String }
             | ListCmd
             | UpdateCmd { slug :: String
                         , _name :: Maybe String
                         , _description :: Maybe String }
             | DeleteCmd { slug :: String }
             deriving (Show, Eq)

type Database = T.Text

listTags :: Database -> IO ()
listTags db = do
  runSqlite db $ do
    tags <- selectList [] [ Asc TagSlug ]
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

addTag :: Database -> String -> String -> Maybe String -> IO ()
addTag db slug name Nothing = addTag db slug name (Just "")
addTag db slug name (Just description) = do
  let entity = Tag (T.pack slug) (T.pack name) (T.pack description)
  runSqlite db $ do
    TagKey slug <- insert entity
    liftIO $ putStrLn $ "Tag (" ++ (T.unpack slug) ++ ") has been added."
    return ()
  `catch` \(e :: SomeException) -> putStrLn $ show e

updateTag :: Database -> String -> Maybe String -> Maybe String -> IO ()
updateTag db slug name description = do
  let tagId = TagKey (T.pack slug)
  runSqlite db $ do
    res <- get tagId
    case res of
     Just _ -> do
       let updates = map fromJust $ filter isJust $
                     [ name >>= \n -> Just $ TagName =. (T.pack n)
                     , description >>= \d -> Just $ TagDescription =. (T.pack d) ]
       update tagId updates
       liftIO $ putStrLn $ "Tag (" ++ slug ++ ") has been updated."
     Nothing -> do
       liftIO $ putStrLn $ "Tag (" ++ slug ++ ") not found."
    return ()
  `catch` \(e :: SomeException) -> putStrLn $ show e

deleteTag :: Database -> String -> IO ()
deleteTag db slug = do
  let tagId = TagKey (T.pack slug)
  runSqlite db $ do
    res <- get tagId
    case res of
     Just _ -> do
       delete tagId
       liftIO $ putStrLn $ "Tag (" ++ slug ++ ") has been deleted."
     Nothing -> do
       liftIO $ putStrLn $ "Tag (" ++ slug ++ ") not found."
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

  -- Parse arguments
  o <- execParser optParser'
  case optCommand o of
   AddCmd    slug name _description -> addTag    db slug name _description
   ListCmd                          -> listTags  db
   UpdateCmd slug name _description -> updateTag db slug name _description
   DeleteCmd slug                   -> deleteTag db slug

addOptions :: Parser Command
addOptions = AddCmd
        <$> argument str
            ( metavar "SLUG"
           <> help "The slug for the tag" )
        <*> argument str
            ( metavar "NAME"
           <> help "The display name of the tag" )
        <*> ( optional $ argument str
              ( metavar "DESCRIPTION"
             <> help "The description for the tag" ) )

listOptions :: Parser Command
listOptions = pure ListCmd

updateOptions :: Parser Command
updateOptions = UpdateCmd
        <$> argument str
            ( metavar "SLUG"
           <> help "The slug for the tag" )
        <*> ( optional $ strOption
              ( long "name"
             <> metavar "NAME"
             <> help "The display name of the tag" ) )
        <*> ( optional $ strOption
              ( long "description"
             <> metavar "DESCRIPTION"
             <> help "The description for the tag" ) )

deleteOptions :: Parser Command
deleteOptions = DeleteCmd
        <$> argument str
            ( metavar "SLUG"
           <> help "The slug for the tag" )

optParser' :: ParserInfo Options
optParser' = info (helper <*> optParser) ( fullDesc <> header "Lambdar command-line manager" )

optParser :: Parser Options
optParser = Options
        <$> subparser ( command "add"
                           (info addOptions
                            ( fullDesc
                           <> progDesc "Add a new tag named NAME for SLUG with DESCRIPTION" ))
                     <> command "list"
                           (info listOptions
                            ( fullDesc
                           <> progDesc "List tags"))
                     <> command "update"
                           (info updateOptions
                            ( fullDesc
                           <> progDesc "Update the name/description of the tag TAG" ))
                     <> command "delete"
                           (info deleteOptions
                            ( fullDesc
                           <> progDesc "Delete the tag TAG" )) )
