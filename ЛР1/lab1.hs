import Data.List
import System.IO

f1 :: (Double, Double, Double) -> Bool
f1 (x, y, r)
 | (sqrt(x + y) <= r) = True 
 | otherwise = False

f2 :: Double -> Double -> Double -> Bool
f2 x y r
 | (sqrt(x + y) <= r) = True 
 | otherwise = False