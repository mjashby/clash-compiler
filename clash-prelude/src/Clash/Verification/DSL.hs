module Clash.Verification.DSL where

import qualified Clash.Verification           as Cv
import           Clash.Verification.Internal

(&&) :: (BasicValue dom a, BasicValue dom b) => a -> b -> BasicAssertion dom
a && b = Cv.and a b
{-# INLINE (&&) #-}

(||) :: (BasicValue dom a, BasicValue dom b) => a -> b -> BasicAssertion dom
a || b = Cv.or a b
{-# INLINE (||) #-}

(~>) :: (BasicValue dom a, BasicValue dom b) => a -> b -> BasicAssertion dom
a ~> b = Cv.implies a b
{-# INLINE (~>) #-}

(|=>) :: (ModalValue dom a, ModalValue dom b) => a -> b -> ModalAssertion dom
a |=> b = Cv.timplies a b
{-# INLINE (|=>) #-}

(|->) :: (ModalValue dom a, ModalValue dom b) => a -> b -> ModalAssertion dom
a |-> b = Cv.timpliesOverlapping a b
{-# INLINE (|->) #-}

(##) :: (ModalValue dom a, ModalValue dom b) => a -> b -> ModalAssertion dom
a ## b = Cv.before a b
{-# INLINE (##) #-}
