import Data.List

fermatChain :: Int -> Int -> Int -> Int
fermatChain p a n = (n * a) `mod` p

target = fermatChain 5 3

rmap :: (a -> a) -> [a] -> [a]
rmap f [] = []
rmap f (x:xs) = (f x) : (rmap f (map f xs))

get p a = rmap (fermatChain p a) (replicate 15 a)

main = do
  print $ get 5 3
