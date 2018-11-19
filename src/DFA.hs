
data DFA = DFA
  { _size :: Int                 -- Number of states
  , _trap :: Int                 -- The trap state for non-matching strings
  , _trans :: [(Int, Char, Int)] -- list of transitions
  , _final :: [Int]              -- Accepting states
  } deriving (Show)
