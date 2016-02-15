module Network.Soil.Options
  ( Options(..)
  , options
  , parseArgs
  ) where

import System.Console.GetOpt

data Options = Options { optHelp :: Bool
                       , optList :: Bool
                       , optPlugins :: Bool
                       , optVersion :: Bool
                       , optApi :: String
                       , optEnv :: String
                       }

defaultOptions :: Options
defaultOptions = Options { optHelp = False
                         , optList = False
                         , optPlugins = False
                         , optVersion = False
                         , optApi = ""
                         , optEnv = ""
                         }

options :: [OptDescr (Options -> Options)]
options = [ Option ['h'] ["help"]     (NoArg (\opts -> opts { optHelp = True }))
              "Print this help message"
          , Option []    ["list"]     (NoArg (\opts -> opts { optList = True }))
              "List available APIs and quit.  If an API is specified, lists the environments."
          , Option []    ["plugins"]  (NoArg (\opts -> opts { optPlugins = True }))
              "List installed plugins and quit"
          , Option ['v'] ["version"]  (NoArg (\opts -> opts { optVersion = True }))
              "Show the version number and quit"
          ]

setOptions :: Options -> [String] -> Either String (Options, [String])
setOptions opts args
  | optHelp opts || optPlugins opts || optVersion opts = Right (opts, [])
  | optList opts = Right ((if null args then opts else opts { optApi = head args }), [])
setOptions opts (api:env:args) = Right (opts { optApi = api, optEnv = env}, args)
setOptions _ _ = Left "Not enough args"

parseArgs :: [String] -> Either String (Options, [String])
parseArgs argv = case getOpt RequireOrder options argv of
  (opts, args, []) -> setOptions (foldl (flip id) defaultOptions opts) args
  (_, _, errs) -> Left $ unwords errs
