{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE PatternSynonyms #-}

module Data.Key where

import           Data.Bifunctor
import           Data.Function (on)
import           Prelude



-- | A datatype describing a key-value pair, with 'Eq', 'Ord' and 'Functor' only
--   working over the value part.
data Key k i = Key k i
               deriving (Show, Functor)

instance Eq i => Eq (Key k i) where
  (==) = (==) `on` unkey

instance Ord i => Ord (Key k i) where
  compare = compare `on` unkey

instance Bifunctor Key where
  bimap f g (Key k i) = Key (f k) (g i)



{-# COMPLETE (:@) #-}
infixr 7 :@
pattern (:@) :: k -> i -> Key k i
pattern (:@) k i = Key k i

getKey :: Key k i -> k
getKey (k :@ _) = k

unkey :: Key k i -> i
unkey (_ :@ i) = i
