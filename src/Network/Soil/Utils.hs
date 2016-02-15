module Network.Soil.Utils
  ( mapRightToM
  , split
  , (>=>=>)
  ) where

mapRightToM :: Monad m => (b -> m (Either a c)) -> Either a b -> m (Either a c)
mapRightToM = either (return . Left)

(>=>=>) :: Monad m => (a -> m (Either s b)) -> (b -> m (Either s c)) -> (a -> m (Either s c))
f >=>=> g = (>>= either (return . Left) g) . f

split :: Char -> String -> [String]
split del s = (takeWhile notDel s):(split del . tail . dropWhile notDel $ s)
  where notDel = (/= del)
