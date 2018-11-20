{-# LANGUAGE DeriveFunctor #-}
-----------------------------------------------------------------------------
-- |
-- Module      : DFA
-- Copyright   : (c) 2018 Cristian Adrián Ontivero
-- License     : BSD3
-- Stability   : experimental
-- Portability : unknown
--
-- Deterministic Finite Automaton
--
-----------------------------------------------------------------------------

module DFA where

-- | Deterministic Finite Automaton where the states are of type 'a'.
data DFA a = DFA
  { _size :: Int             -- Number of states
  , _trap :: a               -- The trap state for non-matching strings
  , _trans :: (a, Char) -> a -- Transition function
  , _final :: [a]            -- Accepting states
  } deriving (Show, Functor)
