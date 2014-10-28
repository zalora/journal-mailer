{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, OverloadedStrings,
             ScopedTypeVariables #-}

module Options (
  Configuration(..),
  lookupReceivers,
  getConfiguration,
 ) where


import           Control.Applicative     ((<$>), (<*>), (<|>))
import           Control.Monad
import           Data.HashMap.Strict     (HashMap, empty, keys, lookup, union)
import           Data.List               (foldl', isSuffixOf)
import           Data.Maybe
import           Data.String.Conversions
import           Data.Yaml
import           Prelude                 hiding (lookup)
import           System.Console.GetOpt
import           System.Environment
import           System.Exit
import           System.IO


data Configuration sender = Configuration {
  showHelp :: Bool,
  sender :: sender,
  receivers :: [String],
  receiverMap :: HashMap String [String], -- mapping from units to receivers' email addresses
  contextInterval :: Maybe Integer    -- Number of seconds before event for which to include info
 }
  deriving (Show, Eq)

setShowHelp :: Configuration a -> Configuration a
setShowHelp c = c{showHelp = True}

addSender :: String -> (Configuration (Maybe String)) -> (Configuration (Maybe String))
addSender sender c = c{sender = Just sender}

addReceiver :: String -> Configuration a -> Configuration a
addReceiver r c = c{receivers = receivers c ++ [r]}

defaultConfiguration :: Configuration (Maybe String)
defaultConfiguration = Configuration False Nothing [] empty (Just 5)

instance FromJSON (Configuration (Maybe String)) where
  parseJSON (Object o) = do
    forM_ (keys o) $ \ key ->
      when (not (key `elem` ["sender", "receivers", "receiver_map", "context_interval"])) $
        fail ("unknown key: " ++ cs key)
    Configuration False <$>
      o .:? "sender" <*>
      o .:? "receivers" .!= [] <*>
      o .:? "receiver_map" .!= empty <*>
      o .:? "context_interval"
  parseJSON _ = mzero

lookupReceivers :: Maybe String -> Configuration a -> [String]
lookupReceivers mMessageSource configuration = receivers configuration ++ interested
 where
  interested = case mMessageSource of
    Nothing -> []
    Just messageSource ->
      fromMaybe [] (lookup messageSource (receiverMap configuration)) ++
      if ".service" `isSuffixOf` messageSource
        then fromMaybe []
          (lookup
            (dropAtEnd (length (".service" :: String)) messageSource)
            (receiverMap configuration))
        else []

dropAtEnd :: Int -> [a] -> [a]
dropAtEnd n = reverse . drop n . reverse


-- * configuration loading

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
          Just sender -> return $
            Configuration False sender (receivers config)
                          (receiverMap config) (contextInterval config)
          Nothing -> do
            hPutStrLn stderr "no --sender given"
            exitWith $ ExitFailure 1
    (_, arguments, errors) -> do
      hPutStr stderr $ concat (errors ++
        map (\ arg -> "unused argument: " ++ arg ++ "\n") arguments)
      exitWith $ ExitFailure 1

header :: String -> String
header progName = unlines $
  (progName ++ " 0.1.1.0") :
  "Sends out emails for every severe message logged to systemd's journal." :
  []

configFileOption :: OptDescr Flag
configFileOption = Option ['c'] ["config"] (ReqArg addConfig "FILE")
  "configuration file"

addConfig :: FilePath -> Flag
addConfig configFile input = do
  fileConfig :: Configuration (Maybe String) <- abortOnError =<< decodeFileEither configFile
  return $ Configuration {
    showHelp = showHelp input || showHelp fileConfig,
    sender = sender fileConfig <|> sender input,
    receivers = receivers input ++ receivers fileConfig,
    receiverMap = receiverMap fileConfig `union` receiverMap input,
    contextInterval = contextInterval input <|> contextInterval fileConfig
   }
 where
  abortOnError :: Either ParseException a -> IO a
  abortOnError x = case x of
    Left e -> do
      hPutStrLn stderr (show e)
      exitWith $ ExitFailure 1
    Right x -> return x
