module Clash.Core.Evaluator.Delay
  ( Delay
  , delay
  , bot
  , runDelay
  ) where

import Control.Monad.Fix

-- Represent a computation that either yields a value now, or gives a
-- computation that yields a value later.
--
data Delay a = Now a | Later (Delay a)
  deriving (Functor)

instance Applicative Delay where
  pure = Now

  Now f   <*> Now x   = Now (f x)
  Now f   <*> Later x = fmap f x
  Later f <*> x       = f <*> x

instance Monad Delay where
  return = pure

  Now x   >>= f = f x
  Later g >>= f = Later (g >>= f)

instance MonadFix Delay where
  mfix f = let a = f (runDelay a) in a

instance (Show a) => Show (Delay a) where
  show (Now a)   = show a
  show (Later _) = "delayed"

runDelay :: Delay a -> a
runDelay (Now x)   = x
runDelay (Later f) = runDelay f

delay :: Delay a -> Delay a
delay = Later

bot :: Delay a
bot = delay bot

