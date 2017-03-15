import Control.Monad.Random
import Learning
import Data.Set
import Numeric.LinearAlgebra

uniformInitializer :: Initializer (Rand StdGen) Double
uniformInitializer 0 set = pure empty
uniformInitializer k set = do
  i <- getRandomR (0, Data.Set.size set - 1)
  let a = elemAt i set
  rest <- uniformInitializer (k - 1) (deleteAt i set)
  return (singleton a `union` rest)

main :: IO ()
main = do
  stuff <- rand 1000000 100
  [lin] <- toRows <$> rand 1 100
  [noise] <- toRows <$> randn 1 1000000
  let res = stuff #> lin
  putStrLn "\nPrinting least squares fit, then real model which was learned with some noise:\n"
  print $ (ols stuff (res + noise) - lin)

--  stuff <- (Data.Set.fromList . toRows) <$> randn 1000 3
--  (centroids, clusters) <- evalRandIO $ kmeans uniformInitializer 3 stuff
--  print centroids
