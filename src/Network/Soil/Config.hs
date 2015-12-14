module Network.Soil.Config
  ( apis
  , configDir
  , envs
  , getDomain
  ) where

import Control.Error.Util (note)
import Data.Hashable (Hashable)
import qualified Data.HashMap.Lazy as HM
import Data.Yaml (decodeFileEither)
import System.Directory (getHomeDirectory)
import System.Environment (lookupEnv)

type Config = HM.HashMap String Envs
type Envs = HM.HashMap String String

configDir :: IO FilePath
configDir = (++ "soil/") <$> base
  where base = lookupEnv "XDG_CONFIG_HOME" >>= maybe ((++ "/.config/") <$> getHomeDirectory) return

config :: IO (Either String Config)
config = configDir >>= decodeFileEither . (++ "apis.yaml") >>= return . mapLeft show
  where mapLeft = (flip either Right) . (Left .)

apis :: IO (Either String [String])
apis = fmap HM.keys <$> config

envs :: String -> IO (Either String [String])
envs name = fmap HM.keys . (>>= eitherLookup name) <$> config

eitherLookup :: (Show k, Eq k, Hashable k) => k -> HM.HashMap k v -> Either String v
eitherLookup k = note ("Key " ++ (show k) ++ " not found") . HM.lookup k

getDomain :: String -> String -> IO (Either String String)
getDomain name env = (>>= eitherLookup env) . (>>= eitherLookup name) <$> config
