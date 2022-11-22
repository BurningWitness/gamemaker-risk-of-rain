module GameMaker.RiskOfRain.Stream where

import           Prelude



data Stream c e a = Next c (Stream c e a)
                  | Error e
                  | Bottom a
                    deriving Show

instance Functor (Stream c e) where   
  fmap f (Next c s) = Next c $ fmap f s
  fmap f (Bottom a) = Bottom $ f a
  fmap _ (Error e)  = Error e

stream :: Applicative f => (c -> f b) -> Stream c e a -> f (Either e a)
stream f (Next c s) = f c *> stream f s
stream _ (Bottom a) = pure $ Right a
stream _ (Error e)  = pure $ Left e

dump :: Stream c e a -> Either e a
dump (Next _ s) = dump s
dump (Error e)  = Left e
dump (Bottom a) = Right a
