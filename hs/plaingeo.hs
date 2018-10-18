data Line = Line Float deriving (Show, Eq)
data Angle = Angle Float deriving (Show, Eq)
data Trigono = Trigono Float deriving (Show, Eq)

trigono :: Line -> Line -> Trigono
trigono (Line l1) (Line l2) = (Trigono $ l1 * l2)

trigono :: Angle -> Trigono
trigono (Angle l1) = (Trigono l1)

arc_trigono :: Trigono -> [Line]
arc_trigono (Trigono f) = [(Line f), (Line 1.0)]

arc_trigono :: Trigono -> [Line]
arc_trigono (Trigono f) = [(Line f), (Line 1.0)]



sine_rule :: 


main = do
  let a = trigono (Line 3.0) (Line 5.0)
  print $ a
  print $ arc_trigono a
