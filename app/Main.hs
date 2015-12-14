module Main where

import Data.Version (showVersion)
import Network.HTTP.Conduit (parseUrl)
import Network.Soil
import Paths_soil (version)
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO

-- TODO: Interactive
data Flag
  = Help
  | Plugins
  | List
  | Version
  deriving (Eq, Ord, Enum, Show, Bounded)

flags =
  [ Option ['h'] ["help"]     (NoArg Help)        "Print this help message"
  , Option []    ["list"]     (NoArg List)        "List available APIs and quit"
  , Option []    ["plugins"]  (NoArg Plugins)     "List installed plugins and quit"
  , Option ['v'] ["version"]  (NoArg Version)     "Show the version number and quit"
  ]

header = "Usage: soil [flags ...] api environment method path [body]"

run :: ([Flag], [String]) -> IO (Either String String)
run (fs, args)
  | hasFlag Help = return . Right $ usageInfo header flags
  | hasFlag Version = return . Right . showVersion $ version
  | hasFlag List = (fmap . fmap) unlines (if null args then apis else envs . head $ args)
  | hasFlag Plugins = return . Left $ "Not available yet."
  | otherwise = makeRequest args >>= return . (curl <$>)
  where hasFlag = flip elem fs

makeRequest :: [String] -> IO (Either String Request)
makeRequest [api, env, m, path] = (fmap . fmap) (withMethod m) req
  where req = getDomain api env >>= mapRightToM ((Right <$>) . parseUrl . flip (++) path)
makeRequest [api, env, m, path, body] = makeRequest [api, env, m, path] >>= return . (withBody body <$>)
makeRequest _ = return . Left $ "Not enough args"

parse :: [String] -> Either String ([Flag], [String])
parse argv = case getOpt RequireOrder flags argv of
  (fs, args, []) -> Right (fs, args)
  (_, _, errs) -> Left $ unwords errs

finish :: (Either String String) -> IO a
finish (Left message) = hPutStrLn stderr message >> exitWith (ExitFailure 1)
finish (Right result) = hPutStrLn stdout result >> exitWith ExitSuccess

main :: IO ()
main = getArgs >>= return . parse >>= mapRightToM run >>= finish
