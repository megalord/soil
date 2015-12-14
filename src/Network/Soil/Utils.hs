module Network.Soil.Utils
  ( mapRightToM
  ) where

mapRightToM :: Monad m => (b -> m (Either a c)) -> Either a b -> m (Either a c)
mapRightToM = either (return . Left)
