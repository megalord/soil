module Main where

import Data.Version (showVersion)
import Network.HTTP.Conduit (parseUrl)
import Network.Soil
import Paths_soil (version)
import System.Console.GetOpt (usageInfo)
import System.Environment (getArgs)
import System.Exit (ExitCode(ExitFailure, ExitSuccess), exitWith)
import System.IO (hPutStrLn, stderr, stdout)

import qualified Plugins

header = "Usage: soil [flags ...] api environment method path [body]"

run :: (Options, [String]) -> IO (Either String String)
run (opts, args)
  | optHelp opts = return . Right $ usageInfo header options
  | optVersion opts = return . Right . showVersion $ version
  | optList opts = (fmap . fmap) unlines (if null (optApi opts) then apis else envs (optApi opts))
  | optPlugins opts = Right . unlines <$> Plugins.list
  | otherwise = makeRequest opts args >>= return . (curl <$>)

makeRequest :: Options -> [String] -> IO (Either String Request)
makeRequest opts [m, path] = (fmap . fmap) (withMethod m) req >>= either (return . Left) Plugins.apply
  where req = getDomain (optApi opts) (optEnv opts) >>=
          mapRightToM ((Right <$>) . parseUrl . flip (++) path)
makeRequest opts [m, path, body] = makeRequest opts [m, path] >>= return . (withBody body <$>)
makeRequest _ _ = return . Left $ "Not enough args"

finish :: (Either String String) -> IO a
finish (Left message) = hPutStrLn stderr message >> exitWith (ExitFailure 1)
finish (Right result) = hPutStrLn stdout result >> exitWith ExitSuccess

main :: IO ()
main = getArgs >>= return . parseArgs >>= mapRightToM run >>= finish
