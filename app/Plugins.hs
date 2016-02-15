module Plugins
  ( apply
  , list
  ) where

import Control.Monad (join)
import Data.Either (rights)
import Language.Haskell.Interpreter
import Network.Soil (Options, Request, configDir, split, (>=>=>))
import System.Directory (getDirectoryContents)

type Plugin = Options -> Request -> IO (Either String Request)

listDirectory :: FilePath -> IO [FilePath]
listDirectory = (filter f <$>) . getDirectoryContents
  where f filename = filename /= "." && filename /= ".."

pluginDir :: IO FilePath
pluginDir = (++ "plugins/") <$> configDir

pluginFiles :: IO [FilePath]
pluginFiles = pluginDir >>= listDirectory

interpreter :: FilePath -> Interpreter Plugin
interpreter file = do
  (++ file) <$> liftIO pluginDir >>= \fp -> loadModules [fp]
  setImportsQ [(name, Just name), ("Network.Soil", Nothing)]
  interpret (name ++ ".mapReq") (as :: Plugin)
  where name = head . split '.' $ file

importPlugin :: FilePath -> IO (Either String Plugin)
importPlugin = (either (Left . show) Right <$>) . runInterpreter . interpreter

plugins :: IO [Plugin]
plugins = rights <$> (pluginFiles >>= sequence . map importPlugin)

apply :: Plugin
apply opts = join . (foldCompose . map (\f -> f opts) <$> plugins <*>) . return
  where foldCompose = foldl (>=>=>) (return . Right)

list :: IO [String]
list = pluginFiles >>= sequence . map testImport
  where testImport p = either (prepend p) (\_ -> prepend p "installed") <$> importPlugin p
        prepend p = ((p ++ ": ") ++)
