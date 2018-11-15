module Regexp where

data RE = Nil
        | Bot
        | C Char
        | Con RE RE
        | Alt RE RE
        | Kle RE
        -- | Fun2 (RE -> RE -> Bool) RE RE
        -- | Fun1 (RE -> Bool) RE
  deriving (Show, Eq, Ord)

acceptsEmptyStr :: RE -> Bool
acceptsEmptyStr (C _)       = False
acceptsEmptyStr Bot         = False
acceptsEmptyStr Nil         = True
acceptsEmptyStr (Kle _)     = True
acceptsEmptyStr (Alt r s) = acceptsEmptyStr r || acceptsEmptyStr s
acceptsEmptyStr (Con r s) = acceptsEmptyStr r && acceptsEmptyStr s
