-- Based on http://semantic-domain.blogspot.com/2013/11/antimirov-derivatives-for-regular.html
-- See also: https://ac.els-cdn.com/0304397595001824/1-s2.0-0304397595001824-main.pdf?_tid=726e60ef-03e9-46a5-b8c1-015e155bc691&acdnat=1542129989_c946c1e56e665c1251f575f8e462fa79
-- http://lambda-the-ultimate.org/node/2293/
{-# LANGUAGE FlexibleInstances #-}

import qualified Data.Set as Set
import Data.Set (Set)

data RE = C Char
  | Nil
  | Con RE RE
  | Bot
  | Alt RE RE
  | Kle RE
  deriving (Show, Eq, Ord)

acceptsNil :: RE -> Bool
acceptsNil (C _)   = False
acceptsNil Bot     = False
acceptsNil Nil     = True
acceptsNil (Kle _) = True
acceptsNil (Alt r1 r2) = acceptsNil r1 || acceptsNil r2
acceptsNil (Con r1 r2) = acceptsNil r1 && acceptsNil r2

class Brzozowski a where
  brzozowski :: a -> RE -> RE

-- 1 character derivative
instance Brzozowski Char where
  brzozowski c = d
    where d Nil = Bot
          d (C y)
              | c == y    = Nil
              | otherwise = Bot
          d (Con r1 r2)
              | acceptsNil r1 = Alt (Con (d r1) r2) (d r2)
              | otherwise     = Con (d r1) r2
          d Bot         = Bot
          d (Alt r1 r2) = Alt (d r1) (d r2)
          d (Kle r)     = Con (d r) (Kle r)

-- word derivative
instance Brzozowski [Char] where
  brzozowski [] = id
  brzozowski (x:xs) = brzozowski xs . brzozowski x

class Antimirov a where
  antimirov :: a -> RE -> Set RE

-- 1 character partial derivative
instance Antimirov Char where
  antimirov c = d
    where d :: RE -> Set RE
          d (C y)
              | c == y    = Set.singleton Nil
              | otherwise = Set.empty
          d (Con r1 r2)
              | acceptsNil r1 =
                Set.map (\r -> Con r r2) (d r1) `Set.union` (d r2)
              | otherwise     = Set.map (\r -> Con r r2) (d r1)
          d Bot         = Set.empty
          d (Alt r1 r2) = (d r1) `Set.union` (d r2)
          d (Kle r)     = Set.map (\r' -> Con r' (Kle r)) (d r)

-- word partial derivative
-- instance Antimirov [Char] where
  -- antimirov []     = Set.singleton
  -- antimirov (x:xs) = antimirov xs . antimirov x

data DFA = DFA { _size :: Int -- Number of states
               , _trap :: Int -- The trap state for non-matching strings
               , _trans :: [(Int, Char, Int)] -- list of transitions
               , _final :: [Int] -- Accepting states
               }
  deriving (Show)
