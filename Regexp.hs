module Regexp where

import Data.Semigroup
import Data.Monoid (Monoid)

data RE = Nil       -- ^ Empty string
        | Bot       -- ^ Bottom
        | C Char    -- ^ A character literal
        | Con RE RE -- ^ Concatenation: rs
        | Alt RE RE -- ^ Alternation: r | s
        | Kle RE    -- ^ Kleene closure: r*
        -- | Fun2 (RE -> RE -> Bool) RE RE
        -- | Fun1 (RE -> Bool) RE
  deriving (Show, Eq, Ord)

instance Semigroup RE where
  Nil <> r = r
  r <> Nil = r
  r <> s   = Con r s

instance Monoid RE where
  mempty  = Nil

acceptsEmptyStr :: RE -> Bool
acceptsEmptyStr (C _)     = False
acceptsEmptyStr Bot       = False
acceptsEmptyStr Nil       = True
acceptsEmptyStr (Kle _)   = True
acceptsEmptyStr (Alt r s) = acceptsEmptyStr r || acceptsEmptyStr s
acceptsEmptyStr (Con r s) = acceptsEmptyStr r && acceptsEmptyStr s
