{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Brzozowski
-- Copyright   : (c) 2018 Cristian Adrián Ontivero
-- License     : BSD3
-- Stability   : experimental
-- Portability : unknown
--
-- Brzozowski regular expression derivative, with its correspoding concise and
-- elegant matching algorithm, and his DFA construction.
--
-----------------------------------------------------------------------------
module Brzozowski where

import Data.Foldable (null)
import Data.Maybe (fromMaybe)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Regex

-- We overload brzozowski to denote both the one character derivative, and its
-- extension to words.
class (Ord a) => Brzozowski a b where
  brzozowski :: Regex a -> b -> Regex a

-- 1 symbol derivative
instance Ord a => Brzozowski a a where
  brzozowski r c = 𝛿 r
    where
      𝛿 Nil       = Bot
      𝛿 Bot       = Bot
      𝛿 (Kle s)   = con (𝛿 s) (kle s)
      𝛿 (Alt s t) = alt (𝛿 s) (𝛿 t)
      𝛿 (Con s t)
          | acceptsEmptyStr s = alt (con (𝛿 s) t) (𝛿 t)
          | otherwise         = con (𝛿 s) t
      𝛿 (C a)
          | c == a    = Nil
          | otherwise = Bot

-- Word derivative, generalized to foldable types instead of just [a]
instance (Foldable f, Ord a) => Brzozowski a (f a) where
  brzozowski = foldl brzozowski

-- A sequence s is contained in a regular expression R iff epsilon is contained
-- in DsR (see Theorem 4.2 from Brzozowski's paper).
--
-- Brzozowski's simple algorithm computes the DFA "on-the-fly". The current
-- regex is the current state of the DFA, and every time we take another
-- derivative, we transition to a new state, represented by the new regex. This
-- is less time efficient, but mostly space efficient.
-- match :: Brzozowski a => Regex a -> f a -> Bool
match r x = acceptsEmptyStr $ brzozowski r x

data DFA a = DFA (Regex a) (Map (Regex a, a) (Regex a)) (Set (Regex a))
  deriving (Show)

-- Brzozowski's algorithm to build a DFA based on derivatives.
mkDFA :: Regex Char -> DFA Char
mkDFA r = go initial Map.empty initial Set.empty
  where
    initial = Set.singleton r
    alph = alphabet r
    go seen table toBeSeen accepting
        | null toBeSeen = DFA r table accepting
        | otherwise     =
          let currentState = Set.elemAt 0 toBeSeen
              newSeen      = Set.insert currentState seen
              t'           = Set.delete currentState toBeSeen
              newAccepting
                  | acceptsEmptyStr currentState = Set.insert currentState accepting
                  | otherwise                    = accepting
              (newTable, _, _, newToBeSeen) = foldr updateTable (table, currentState, newSeen, t') alph
          in go newSeen newTable newToBeSeen newAccepting
    updateTable :: Char -> (Map (RE, Char) RE, RE, Set RE, Set RE) -> (Map (RE, Char) RE, RE, Set RE, Set RE)
    updateTable i (m, q, seen, t) =
        let newMap = Map.insert (q, i) der m
            toSee = t `Set.union` Set.singleton der
        in if not (der `Set.member` seen) && not (der `Set.member` t)
              then (newMap, q, seen, toSee)
              else (newMap, q, seen, t)
      where der = brzozowski q i

match' :: Regex Char -> [Char] -> Bool
match' = runDFA <$> mkDFA
  where
    runDFA :: DFA Char -> [Char] -> Bool
    runDFA (DFA initialState transitionFunction accepting) xs =
        foldl (flip f) initialState xs `Set.member` accepting
      where f c r = fromMaybe Bot $ Map.lookup (r, c) transitionFunction
