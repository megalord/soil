module Plugins
  ( apply
  , list
  ) where

import Control.Monad (join)
import Data.Either (rights)
import Language.Haskell.Interpreter
import Network.Soil (Request, configDir, split, (>=>=>))
import System.Directory (getDirectoryContents)

type Plugin = Request -> IO (Either String Request)

listDirectory :: FilePath -> IO [FilePath]
listDirectory = (filter f <$>) . getDirectoryContents
  where f filename = filename /= "." && filename /= ".."

pluginDir :: IO FilePath
pluginDir = (++ "plugins/") <$> configDir

pluginFiles :: IO [FilePath]
pluginFiles = pluginDir >>= listDirectory

interpreter :: String -> Interpreter Plugin
interpreter plugin = do
  (++ plugin) <$> liftIO pluginDir >>= \fp -> loadModules [fp]
  setImportsQ [(name, Just name), ("Network.Soil", Nothing)]
  interpret (name ++ ".mapReq") (as :: Plugin)
  where name = head . split '.' $ plugin

importPlugin :: String -> IO (Either String Plugin)
importPlugin plugin = either (Left . show) Right <$> runInterpreter (interpreter plugin) 

plugins :: IO [Plugin]
plugins = rights <$> (pluginFiles >>= sequence . map importPlugin)

apply :: Plugin
apply = join . (foldCompose <$> plugins <*>) . return
  where foldCompose = foldl (>=>=>) (return . Right)

list :: IO [String]
list = pluginFiles >>= sequence . map testImport
  where testImport p = either (prepend p) (\_ -> prepend p "installed") <$> importPlugin p
        prepend p = ((p ++ ": ") ++)
