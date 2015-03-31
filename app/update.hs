import ClassyPrelude
import Application (makeFoundation)
import Foundation
import Database.Persist hiding (get)
import Database.Persist.Sql (SqlPersistM, SqlBackend, runSqlPersistMPool, rawExecute, rawSql, unSingle, connEscapeName)
import Model
import Yesod.Default.Config2 (ignoreEnv, loadAppSettings)
import Database.Persist.Sqlite (sqlDatabase, wrapConnection, createSqlPool)
import qualified Database.Sqlite as Sqlite
import Control.Monad.Logger (runLoggingT)
import Settings (appDatabaseConf)
import Yesod.Core (messageLoggerSource)
import Yesod.Persist.Core (runDB)
import Options.Applicative


main :: IO ()
main = do
  putStrLn "Hello!"
