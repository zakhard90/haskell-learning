module MaxAdjProd where

main :: IO ()
main = do
  let ls = [1,5,6,2,7,9,1,2,8,5,9,8,4,23,1,4,15]
  print $ genMaxAdjProd 2 ls
  
genMaxAdjProd :: (Num a, Ord a) => Int -> [a] -> a
genMaxAdjProd n list@(x:xs)
  | length list < n = 0
  | otherwise = go n (product (take n list)) (tail list)
  where 
    go :: (Num a, Ord a) => Int -> a -> [a] -> a
    go m maxVal [_] = maxVal
    go m maxVal sublist@(y:ys) = go m (max (product (take m sublist)) maxVal) (tail sublist)