import Data.Char
data Number = Number Char Int deriving Show

val :: Number -> Int
val (Number c v) = v

sym :: Int -> Char
sym v = chr ((ord '@') + v)

s :: Number -> Number
s (Number c v) = (Number (chr ((ord c) + 1)) (v + 1))

-- Spread!
(|||) :: Int -> (Number -> Number) -> (Number -> Int -> Number)
(|||) e f = (g e)
  where
    g :: Int -> Number -> Int -> Number
    g e num v
         | v == e         = num
         | (val num) == e = (Number (sym v) v)
         | otherwise      = g e (f num) (v - 1)

-- Shrink!
(><) :: (Number -> Int -> Number) -> Int -> (Number -> Number)
(><) f v = g
  where g num = f num v

add = (|||) 0 s
mul n v = ((|||) 1 ((><) add (val n))) n v
-- xp n v  = ((|||) 1 ((><) mul (val n))) n v
-- xp' n v = ((|||) 1 ((><) xp (val n))) n v

main = do
  print $ mul (Number 'E' 5) 2
--  print $ mul (Number 'E' 5) 3
