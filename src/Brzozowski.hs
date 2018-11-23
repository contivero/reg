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

import Data.Foldable (null, foldl')
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
  brzozowski = foldl' brzozowski

-- A sequence s is contained in a regular expression R iff epsilon is contained
-- in DsR (see Theorem 4.2 from Brzozowski's paper).
--
-- Brzozowski's simple algorithm computes the DFA "on-the-fly". The current
-- regex is the current state of the DFA, and every time we take another
-- derivative, we transition to a new state, represented by the new regex. This
-- is less time efficient, but mostly space efficient.
matchDeriving :: Brzozowski a b => Regex a -> b -> Bool
matchDeriving r x = acceptsEmptyStr $ brzozowski r x

data DFA a = DFA (Regex a) (Map (Regex a, a) (Regex a)) (Set (Regex a))
  deriving (Show)

-- | Brzozowski construction to build a DFA out of regular expression derivatives.
-- Although the set of derivatives of regular expressions might be infinite,
-- Brzozowski proved it is finite modulo the ACI-axioms (associativity,
-- commutativity, and idempotence of the sum).
mkDFA :: Ord a => Regex a -> DFA a
mkDFA r = go initial Map.empty initial Set.empty
  where
    initial = Set.singleton r
    alph = alphabet r

    -- Loop updating the set of dissimilar derivatives, the transition function,
    -- the queue of states (regexes) to process, and the set of accepting
    -- states.
    go diss table toBeSeen accepting
        | null toBeSeen = DFA r table accepting
        | otherwise     =
          let currentState = Set.elemAt 0 toBeSeen
              diss'        = Set.insert currentState diss
              toBeSeen'    = Set.delete currentState toBeSeen
              accepting'
                  | acceptsEmptyStr currentState = Set.insert currentState accepting
                  | otherwise                    = accepting
              (table', _, _, newToBeSeen) = foldr updateTable (table, currentState, diss', toBeSeen') alph
          in go diss' table' newToBeSeen accepting'

    updateTable i (m, q, diss, toBeSeen) =
        if not (der `Set.member` diss || der `Set.member` toBeSeen)
           then (newMap, q, diss, toBeSeen `Set.union` Set.singleton der)
           else (newMap, q, diss, toBeSeen)
      where der    = brzozowski q i
            newMap = Map.insert (q, i) der m

-- Build a DFA from a regular expression, and use it to match a string.
matchWithDFA :: (Foldable f, Ord a) => Regex a -> f a -> Bool
matchWithDFA = runDFA <$> mkDFA
  where
    runDFA :: (Foldable f, Ord a) => DFA a -> f a -> Bool
    runDFA (DFA initialState table accepting) xs =
        foldl' (flip f) initialState xs `Set.member` accepting
      where f c r = fromMaybe Bot $ Map.lookup (r, c) table
