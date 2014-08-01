

module Run where


import           Data.Function
import           Network.Mail.Mime
import           Pipes
import           Pipes.Prelude      as P
import           Pipes.Safe
import           Prelude            hiding (lookup)
import           System.Environment
import           System.Exit.Compat
import           System.IO
import           Systemd.Journal

import           Process


run :: IO ()
run = do
  receivers <- getArgs
  case receivers of
    [] -> do
      progName <- getProgName
      die ("usage: " ++ progName ++ " EMAIL_ADDRESS...")
    _ -> runEffect $ runSafeP $
      (journal >-> process receivers >-> for cat (liftIO . notify))


journal :: MonadSafe m => Producer JournalFields m ()
journal =
  openJournal [] FromEnd Nothing Nothing >->
  P.map journalEntryFields

notify :: Mail -> IO ()
notify mail = do
  hPutStrLn stderr "sending mail notification"
  renderSendMail mail
