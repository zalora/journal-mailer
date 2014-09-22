{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, OverloadedStrings,
             ScopedTypeVariables #-}

module Options (
  Configuration(..),
  getConfiguration,
 ) where


import           Control.Applicative
import           Control.Monad
import           Data.HashMap.Strict     (keys)
import           Data.List
import           Data.String.Conversions
import           Data.Yaml
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

addReceiver :: String -> Configuration a -> Configuration a
addReceiver r c = c{receivers = receivers c ++ [r]}

defaultConfiguration :: Configuration (Maybe String)
defaultConfiguration = Configuration False Nothing []

instance FromJSON (Configuration (Maybe String)) where
  parseJSON (Object o) = do
    forM_ (keys o) $ \ key ->
      when (not (key `elem` ["sender", "receivers"])) $
        fail ("unknown key: " ++ cs key)
    Configuration False <$>
      o .:? "sender" <*>
      o .:? "receivers" .!= []
  parseJSON _ = mzero

type Flag = Configuration (Maybe String) -> IO (Configuration (Maybe String))

getConfiguration :: IO (Configuration String)
getConfiguration = do
  args <- getArgs
  let help = Option ['h'] ["help"] (NoArg setShowHelp) "print this help"
      senderOption = Option [] ["sender"] (ReqArg addSender "ADDRESS")
        "email address that will be used as the sender"
      receiverOption = Option ['r'] ["receiver"] (ReqArg addReceiver "ADDRESS")
        "email address that notifications will be sent to"
      options :: [OptDescr Flag]
      options = map (fmap (return .)) [help, senderOption, receiverOption] ++ [configFileOption]
      result = getOpt Permute options args
  case result of
    (optionMods, [], []) -> do
      config :: Configuration (Maybe String)
        <- foldl' (>=>) return optionMods defaultConfiguration
      if showHelp config
        then do
          progName <- getProgName
          putStr (usageInfo (header progName) options)
          exitWith ExitSuccess
        else case sender config of
          Just sender -> return $ Configuration False sender (receivers config)
          Nothing -> do
            hPutStrLn stderr "no --sender given"
            exitWith $ ExitFailure 1
    (_, arguments, errors) -> do
      hPutStr stderr $ concat (errors ++
        map (\ arg -> "unused argument: " ++ arg ++ "\n") arguments)
      exitWith $ ExitFailure 1

header :: String -> String
header progName = unlines $
  (progName ++ " 0.1.0.0") :
  "Sends out emails for every severe message logged to systemd's journal." :
  []

configFileOption :: OptDescr Flag
configFileOption = Option ['h'] ["config"] (ReqArg addConfig "FILE")
  "configuration file"

addConfig :: FilePath -> Flag
addConfig configFile input = do
  fileConfig :: Configuration (Maybe String) <- abortOnError =<< decodeFileEither configFile
  return $ Configuration {
    showHelp = showHelp input || showHelp fileConfig,
    sender = sender fileConfig <|> sender input,
    receivers = receivers input ++ receivers fileConfig
   }
 where
  abortOnError :: Either ParseException a -> IO a
  abortOnError x = case x of
    Left e -> do
      hPutStrLn stderr (show e)
      exitWith $ ExitFailure 1
    Right x -> return x
