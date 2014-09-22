{-# LANGUAGE DeriveDataTypeable #-}

module Options (
  Configuration(..),
  getConfiguration,
 ) where


import           Data.List
import           System.Console.GetOpt
import           System.Environment
import           System.Exit
import           System.IO


data Configuration sender = Configuration {
  showHelp :: Bool,
  sender :: sender,
  receivers :: [String]
 }
  deriving (Show, Eq, Ord)

setShowHelp :: Configuration a -> Configuration a
setShowHelp c = c{showHelp = True}

addSender :: String -> (Configuration (Maybe String)) -> (Configuration (Maybe String))
addSender sender c = c{sender = Just sender}

defaultConfiguration :: Configuration (Maybe String)
defaultConfiguration = Configuration False Nothing []

type Flag = Configuration (Maybe String) -> Configuration (Maybe String)

getConfiguration :: IO (Configuration String)
getConfiguration = do
  args <- getArgs
  let help = Option ['h'] ["help"] (NoArg setShowHelp) "print this help"
      senderOption = Option [] ["sender"] (ReqArg addSender "ADDRESS")
        "email address that will be used as the sender"
      options :: [OptDescr Flag]
      options = [help, senderOption]
      result = getOpt Permute options args
  case result of
    (optionMod, arguments, []) ->
      let config :: Configuration (Maybe String)
          config = foldl' (.) id optionMod defaultConfiguration
      in if showHelp config
        then do
          progName <- getProgName
          putStr (usageInfo (header progName) options)
          exitWith ExitSuccess
        else case sender config of
          Just sender -> return $ Configuration False sender arguments
          Nothing -> do
            hPutStrLn stderr "no --sender given"
            exitWith $ ExitFailure 1
    (_, _, errors) -> do
      hPutStr stderr $ concat errors
      exitWith $ ExitFailure 1

header :: String -> String
header progName = unlines $
  (progName ++ " 0.1.0.0") :
  "Sends out emails for every severe message logged to systemd's journal." :
  []
