{-# LANGUAGE FlexibleInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Brzozowski
-- Copyright   : (c) 2018 Cristian Adrián Ontivero
-- License     : BSD3
-- Stability   : experimental
-- Portability : unknown
--
-- Brzozowski regular expression derivative, with its correspoding concise and
-- elegant matching algorithm.
--
-----------------------------------------------------------------------------

module Brzozowski where

import Regexp

-- We overload brzozowski to denote both the one character derivative, and its
-- extension to words.
class Brzozowski a where
  brzozowski :: a -> RE -> RE

-- Single character derivative
instance Brzozowski Char where
  brzozowski c = 𝛿
    where
      𝛿 Nil       = Bot
      𝛿 Bot       = Bot
      𝛿 (Kle r)   = Con (𝛿 r) (Kle r)
      𝛿 (Alt r s) = Alt (𝛿 r) (𝛿 s)
      𝛿 (Con r s)
          | acceptsEmptyStr r = Alt (Con (𝛿 r) s) (𝛿 s)
          | otherwise         = Con (𝛿 r) s
      𝛿 (C a)
          | c == a    = Nil
          | otherwise = Bot

-- Word derivative
instance Brzozowski [Char] where
  brzozowski []     = id
  brzozowski (x:xs) = brzozowski xs . brzozowski x

match :: RE -> String -> Bool
match r = acceptsEmptyStr . foldr brzozowski r
