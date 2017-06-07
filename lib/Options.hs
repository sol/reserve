module Options (
  Options (..)
, withOptions
, parseOptions
, defaultOptions

-- exported to silence warnings
, Arg (..)
) where

import           Prelude.Compat

import           Data.Maybe
import           Data.List
import           Text.Read.Compat
import           System.Console.GetOpt
import           System.Environment
import           System.IO
import           System.Exit

import           Network (PortNumber)

withOptions :: (Options -> IO ()) -> IO ()
withOptions action = do
  args <- getArgs
  case parseOptions args of
    Left err -> uncurry exitWithMessage err
    Right opts -> action opts

exitWithMessage :: ExitCode -> String -> IO ()
exitWithMessage err msg = case err of
  ExitSuccess -> hPutStr stdout msg
  _           -> hPutStr stderr msg >> exitWith err

data Options = Options {
  optionsPort :: PortNumber
, optionsReservePort :: PortNumber
, optionsMainIs :: FilePath
, optionsAppArgs :: [String]
} deriving (Eq, Show)

setPort :: Integer -> Options -> Options
setPort p c = c {optionsPort = fromInteger p}

setReservePort :: Integer -> Options -> Options
setReservePort p c = c {optionsReservePort = fromInteger p}

defaultOptions :: Options
defaultOptions = Options 3000 12000 "src/Main.hs" []

type Result = Either NoOptions Options

data NoOptions = Help | InvalidArgument String String

data Arg a = Arg {
  argumentName   :: String
, argumentParser :: String -> Maybe a
, argumentSetter :: a -> Options -> Options
}

mkOption :: [Char] -> String -> Arg a -> String -> OptDescr (Result -> Result)
mkOption shortcut name (Arg argName parser setter) help = Option shortcut [name] (ReqArg arg argName) help
  where
    arg :: String -> Result -> Result
    arg input x = x >>= \c -> case parser input of
      Just n -> Right (setter n c)
      Nothing -> Left (InvalidArgument name input)

options :: [OptDescr (Result -> Result)]
options = [
    Option   []  ["help"]             (NoArg (const $ Left Help))            "display this help and exit"
  , mkOption "p"  "port"              (Arg "PORT" readMaybe setPort)        ("port of the web application (default: " ++ show (optionsPort defaultOptions) ++ ")")
  , mkOption ""   "reserve-port"      (Arg "PORT" readMaybe setReservePort) ("port reserve listens on (default: " ++ show (optionsReservePort defaultOptions) ++ ")")
  ]

parseOptions :: [String] -> Either (ExitCode, String) Options
parseOptions allArgs = case getOpt Permute options args of
    (_, _, err:_)  -> tryHelp err
    (_, _:arg:_, _)  -> tryHelp ("unexpected argument `" ++ arg ++ "'\n")
    (opts, mainIs, []) -> case foldl' (flip id) (Right defaultOptions) opts of
        Left Help                         -> Left (ExitSuccess, usage)
        Left (InvalidArgument flag value) -> tryHelp ("invalid argument `" ++ value ++ "' for `--" ++ flag ++ "'\n")
        Right x -> Right x {optionsMainIs = fromMaybe (optionsMainIs defaultOptions) $ listToMaybe mainIs, optionsAppArgs = appArgs}
  where
    tryHelp msg = Left (ExitFailure 1, "reserve: " ++ msg ++ "Try `reserve --help' for more information.\n")
    usage = usageInfo ("Usage: reserve [OPTION]... [MAIN] [-- ARG...]\n\nOPTIONS") options ++ helpForMain ++ helpForAppArgs
    helpForMain = "\nThe optional MAIN argument is a path to a module that exports a `main' function.  (default: " ++ optionsMainIs defaultOptions ++ ")\n"
    helpForAppArgs = "\nAll arguments following the optional `--' are passed to the web application.\n"

    (args, appArgs) = drop 1 <$> span (/= "--") allArgs
