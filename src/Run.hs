

module Run where


import           Data.Maybe (fromMaybe)
import           Network.Mail.Mime
import           Pipes
import           Pipes.Prelude      as P
import           Pipes.Safe
import           System.Environment
import           System.Exit.Compat
import           System.IO
import           Systemd.Journal

import           Options
import           Process


run :: IO ()
run = do
  options <- getConfiguration
  case receivers options of
    [] -> do
      progName <- getProgName
      die ("usage: " ++ progName ++ " EMAIL_ADDRESS...")
    _ -> runEffect $ runSafeP $
      (journal >-> process options ioJournal >-> for cat (liftIO . notify options))


journal :: MonadSafe m => Producer JournalFields m ()
journal =
  openJournal [] FromEnd Nothing Nothing >->
  P.map journalEntryFields

notify :: Configuration String -> Mail -> IO ()
notify options mail = do
  hPutStrLn stderr "sending mail notification"
  renderSendMailCustom sendmailPath' sendmailOpts' mail
 where
  sendmailPath' = fromMaybe "/usr/sbin/sendmail" $ sendmailPath options
  sendmailOpts' = fromMaybe [] $ sendmailOpts options
