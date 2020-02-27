{-|
Copyright  :  (C) 2019, Myrtle Software Ltd
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Verification
-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

#if __GLASGOW_HASKELL__ < 806
{-# LANGUAGE TypeInType #-}
#endif

module Clash.Verification.Internal
 ( BasicAssertion(..)
 , ModalAssertion(..)
 , Property(..)
 , Assertion(..)
 , BasicValue
 , ModalValue
 , RenderAs(..)

 -- * Internal
 , BasicAssertion'(..)
 , ModalAssertion'(..)
 , Property'(..)
 , toBasicAssertion
 , toModalAssertion
 )
 where

import           Data.Coerce                    (coerce)
import           Data.Text                      (Text)

import Clash.Annotations.BitRepresentation
  (ConstrRepr(..), DataReprAnn(..), liftQ)
import           Clash.Signal.Internal          (Domain, Signal)

-- | Render target for HDL
data RenderAs
  = PSL
  -- ^ Property Specification Language
  | SVA
  -- ^ SystemVerilog Assertions
  | AutoRenderAs
  -- ^ Use SVA for SystemVerilog, PSL for others
  deriving (Show)

-- | Internal version of "BasicAssertion". All user facing will instantiate @a@
-- with @(Maybe Text, Signal dom Bool)@. Blackboxes will instantiate it with
-- @(Maybe Text, Term)@ instead.
data BasicAssertion' a
  = CvPure a
  | CvLit Bool

  | CvNot (BasicAssertion' a)
  | CvAnd (BasicAssertion' a) (BasicAssertion' a)
  | CvOr (BasicAssertion' a) (BasicAssertion' a)
  | CvImplies (BasicAssertion' a) (BasicAssertion' a)
  deriving (Show, Functor, Foldable, Traversable)

-- | Internal version of "ModelAssertion". All user facing will instantiate @a@
-- with @(Maybe Text, Signal dom Bool)@. Blackboxes will instantiate it with
-- @(Maybe Text, Term)@ instead.
data ModalAssertion' a
  = CvTempPure (BasicAssertion' a)
  | CvNext Int (ModalAssertion' a)
  | CvBefore (ModalAssertion' a) (ModalAssertion' a)
  | CvTempImplies Int (ModalAssertion' a) (ModalAssertion' a)
  | CvAlways (ModalAssertion' a)
  | CvNever (ModalAssertion' a)
  deriving (Show, Functor, Foldable, Traversable)

-- | Internal version of "Property". All user facing will instantiate @a@
-- with @(Maybe Text, Signal dom Bool)@. Blackboxes will instantiate it with
-- @(Maybe Text, Term)@ instead.
data Property' a
  = CvAssert (ModalAssertion' a)
  | CvCover (ModalAssertion' a)
  deriving (Show, Functor, Foldable, Traversable)

-- | A basic, non-temporal assertion. I.e., a "BasicAssertion" will only deal
-- with single cycles.
newtype BasicAssertion (dom :: Domain) =
  BasicAssertion (BasicAssertion' (Maybe Text, Signal dom Bool))

-- | A modal (temporal) assertion. I.e., a "ModelAssertion" deals with assertions
-- defined over one or more cycles.
newtype ModalAssertion (dom :: Domain) =
  ModalAssertion (ModalAssertion' (Maybe Text, Signal dom Bool))

-- | A property is a temporal or basic assertion that's specified to either
-- used as an _assert_ or _cover_ statement. See
-- 'Clash.Explicit.Verification.assert' and 'Clash.Explicit.Verification.cover'.
newtype Property (dom :: Domain) =
  Property (Property' (Maybe Text, Signal dom Bool))

-- | A result of some property. Besides carrying the actual boolean result, it
-- carries some properties used to make reports.
data Assertion = Assertion
  { cvPropName :: !String  -- I'd like text, but Clash complains :[
  -- ^ Name of property belonging to this result
  , cvPass :: !Bool
  -- ^ False whenever property is violated, True otherwise
  }
  deriving (Eq)
{-# ANN module (
  DataReprAnn
    $(liftQ [t| Assertion |])
    0
    [ ConstrRepr 'Assertion 0 0 [0b0, 0b0]
    ]) #-}
{- Marked as zero-width so Clash won't stumble on the fact it's unrepresentable. ^ -}

-- | A BasicValue is a bool-like value or stream that can be used in property
-- specifications. Clash implements two: a stream of booleans (Signal dom Bool),
-- and the result of a property expression (BasicAssertion dom).
class BasicValue dom a | a -> dom where
  -- | Convert given type into a BasicAssertion.
  toBasicAssertion :: a -> BasicAssertion' (Maybe Text, Signal dom Bool)

-- | Boolean literal; breaks functional dependency
--instance BasicValue dom Bool where
--  toBasicAssertion = CvLit
--  {-# INLINE toBasicAssertion #-}

-- | Stream of booleans, originating from a circuit
instance BasicValue dom (Signal dom Bool) where
  toBasicAssertion s = CvPure (Nothing, s)
  {-# INLINE toBasicAssertion #-}

-- | Result of a property specification
instance BasicValue dom (BasicAssertion dom) where
  toBasicAssertion = coerce
  {-# INLINE toBasicAssertion #-}

-- | Same as 'BasicValue' but for modal/temporal values.
class ModalValue dom a | a -> dom where
  -- | Convert given type into a ModalAssertion.
  toModalAssertion :: a -> ModalAssertion' (Maybe Text, Signal dom Bool)

instance ModalValue dom (Signal dom Bool) where
  toModalAssertion = toModalAssertion . BasicAssertion . toBasicAssertion
  {-# INLINE toModalAssertion #-}

instance ModalValue dom (BasicAssertion dom) where
  toModalAssertion b = CvTempPure (coerce b)
  {-# INLINE toModalAssertion #-}

instance ModalValue dom (ModalAssertion dom) where
  toModalAssertion = coerce
  {-# INLINE toModalAssertion #-}
