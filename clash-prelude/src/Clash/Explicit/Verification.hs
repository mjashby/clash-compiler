{-|
Copyright  :  (C) 2019, Myrtle Software Ltd
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Verification primitives for Clash. Currently implements PSL (Property
Specification Language) and SVA (SystemVerilog Assertions). For a good overview
of PSL and an introduction to the concepts of property checking, read
<https://standards.ieee.org/standard/62531-2012.html>.

The verification API is currently experimental and subject to change.
-}

{-# LANGUAGE NoImplicitPrelude #-}

module Clash.Explicit.Verification
  ( -- * Types
    BasicAssertion
  , ModalAssertion
  , Property
  , Assertion
  , BasicValue
  , ModalValue
  , RenderAs(..)

    -- * Bootstrapping functions
  , name
  , lit

    -- * Functions to build a PSL/SVA expressions
  , not
  , and
  , or
  , implies
  , next
  , nextN
  , before
  , timplies
  , timpliesOverlapping
  , always
  , never

  -- * Asserts
  , assert
  , cover

  -- * Assertion checking
  , check
  , checkI

  -- * Functions to deal with assertion results
  , hideAssertion
  , mergeAssertions
  )
 where

import           Prelude                           (Bool, Int, (.), pure)

import           Data.Text                         (Text)
import           Data.Maybe                        (Maybe(Just))

import           Clash.Annotations.Primitive
  (Primitive(InlinePrimitive), HDL(..))
import           Clash.Signal.Internal
  (KnownDomain, Signal, Clock, Reset)
import           Clash.XException                  (errorX, hwSeqX)

import           Clash.Verification.Internal

-- | Convert a signal to a cv expression with a name hint. Clash will try its
-- best to use this name in the rendered assertion, but might run into
-- collisions. You can skip using 'name' altogether. Clash will then try its
-- best to get a readable name from context.
name :: Text -> Signal dom Bool -> BasicAssertion dom
name nm signal = BasicAssertion (CvPure (Just nm, signal))
{-# INLINE name #-}

-- | For using a literal (either True or False) in assertions
lit :: Bool -> BasicAssertion dom
lit = BasicAssertion . CvLit
{-# INLINE lit #-}

-- |
-- a     | not a
-- ------------
-- True  | False
-- False | True
not :: BasicValue dom a => a -> BasicAssertion dom
not = BasicAssertion . CvNot . toBasicAssertion
{-# INLINE not #-}

-- |
-- a     | b     | a `and` b
-- --------------|----------
-- False | False | False
-- False | True  | False
-- True  | False | False
-- True  | True  | True
and :: (BasicValue dom a, BasicValue dom b) => a -> b -> BasicAssertion dom
and a b = BasicAssertion (CvAnd (toBasicAssertion a) (toBasicAssertion b))
{-# INLINE and #-}

-- |
-- a     | b     | a `or` b
-- --------------|----------
-- False | False | False
-- False | True  | True
-- True  | False | True
-- True  | True  | True
or :: (BasicValue dom a, BasicValue dom b) => a -> b -> BasicAssertion dom
or a b = BasicAssertion (CvOr (toBasicAssertion a) (toBasicAssertion b))
{-# INLINE or #-}

-- |
-- a     | b     | a `implies` b
-- --------------|----------
-- False | False | True
-- False | True  | True
-- True  | False | False
-- True  | True  | True
implies :: (BasicValue dom a, BasicValue dom b) => a -> b -> BasicAssertion dom
implies a b = BasicAssertion (CvImplies (toBasicAssertion a) (toBasicAssertion b))
{-# INLINE implies #-}

next :: ModalValue dom a => a -> ModalAssertion dom
next = ModalAssertion . CvNext 1 . toModalAssertion
{-# INLINE next #-}

nextN :: ModalValue dom a => Int -> a -> ModalAssertion dom
nextN n = ModalAssertion . CvNext n . toModalAssertion
{-# INLINE nextN #-}

before :: (ModalValue dom a, ModalValue dom b) => a -> b -> ModalAssertion dom
before a b = ModalAssertion (CvBefore (toModalAssertion a) (toModalAssertion b))
{-# INLINE before #-}

timplies :: (ModalValue dom a, ModalValue dom b) => a -> b -> ModalAssertion dom
timplies a b = ModalAssertion (CvTempImplies 1 (toModalAssertion a) (toModalAssertion b))
{-# INLINE timplies #-}

timpliesOverlapping
  :: (ModalValue dom a, ModalValue dom b)
  => a
  -> b
  -> ModalAssertion dom
timpliesOverlapping a b =
  ModalAssertion (CvTempImplies 0 (toModalAssertion a) (toModalAssertion b))
{-# INLINE timpliesOverlapping #-}

always :: ModalValue dom a => a -> ModalAssertion dom
always = ModalAssertion . CvAlways . toModalAssertion
{-# INLINE always #-}

never :: ModalValue dom a => a -> ModalAssertion dom
never = ModalAssertion . CvNever . toModalAssertion
{-# INLINE never #-}

assert :: ModalValue dom a => a -> Property dom
assert = Property . CvAssert . toModalAssertion
{-# INLINE assert #-}

cover :: ModalValue dom a => a -> Property dom
cover = Property . CvCover . toModalAssertion
{-# INLINE cover #-}

check
  :: KnownDomain dom
  => Clock dom
  -> Reset dom
  -> Text
  -- ^ Property name (used in reports and error messages)
  -> RenderAs
  -- ^ Assertion language to use in HDL
  -> Property dom
  -> Signal dom Assertion
check !_clk !_rst !_propName !_renderAs !_prop =
  pure (errorX "Simulation for Clash.Verification not yet implemented")
{-# NOINLINE check #-}
{-# ANN check (InlinePrimitive [Verilog, SystemVerilog, VHDL] "[ { \"BlackBoxHaskell\" : { \"name\" : \"Clash.Explicit.Verification.check\", \"templateFunction\" : \"Clash.Primitives.Verification.checkBBF\"}} ]") #-}

-- | Same as 'assert',
checkI
  :: KnownDomain dom
  => Clock dom
  -> Reset dom
  -> Text
  -- ^ Property name (used in reports and error messages)
  -> RenderAs
  -- ^ Assertion language to use in HDL
  -> Property dom
  -> Signal dom a
  -> Signal dom a
checkI clk rst propName renderAs prop =
  hideAssertion (check clk rst propName renderAs prop)

hideAssertion :: Signal dom Assertion -> Signal dom a -> Signal dom a
hideAssertion = hwSeqX

mergeAssertions
  :: Signal dom Assertion
  -> Signal dom Assertion
  -> Signal dom Assertion
mergeAssertions !_a !_b =
  pure (errorX "Simulation for Clash.Verification not yet implemented")
