{-# LANGUAGE DeriveDataTypeable #-}

module Options (
  Options(..),
  getOptions,
 ) where


import           System.Console.GetOpt
import           System.Environment
import           System.Exit
import           System.IO


data Options = Options {
  sender :: String,
  receivers :: [String]
 }
  deriving (Show, Eq, Ord)


data Flag
  = Help
  | Sender String
 deriving (Show, Eq, Ord)

isSender :: Flag -> Bool
isSender Sender{} = True
isSender _ = False

getOptions :: IO Options
getOptions = do
  args <- getArgs
  let help = Option ['h'] ["help"] (NoArg Help) "print this help"
      sender = Option [] ["sender"] (ReqArg Sender "ADDRESS")
        "email address that will be used as the sender"
      options = [help, sender]
      result = getOpt Permute options args
  case result of
    (flags, arguments, []) -> if Help `elem` flags
      then do
        progName <- getProgName
        putStr (usageInfo (header progName) options)
        exitWith ExitSuccess
      else case filter isSender flags of
        [Sender sender] -> return $ Options sender arguments
        [] -> do
          hPutStrLn stderr "no --sender given"
          exitWith $ ExitFailure 1
        _ -> do
          hPutStrLn stderr "multiple --sender flags not allowed"
          exitWith $ ExitFailure 1
    (_, _, errors) -> do
      hPutStr stderr $ concat errors
      exitWith $ ExitFailure 1

header :: String -> String
header progName = unlines $
  (progName ++ " 0.1.0.0") :
  "Sends out emails for every severe message logged to systemd's journal." :
  []
