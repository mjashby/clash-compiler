{-|
Copyright  :  (C) 2019, Myrtle Software Ltd
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

See "Clash.Explicit.Verification" for an introduction.

The verification API is currently experimental and subject to change.

-}

module Clash.Verification
  ( -- * Types
    BasicAssertion
  , ModalAssertion
  , Property
  , Assertion
  , BasicValue
  , ModalValue
  , RenderAs(..)


    -- * Bootstrapping functions
  , EV.name
  , EV.lit

    -- * Functions to build a PSL/SVA expressions
  , EV.not
  , EV.and
  , EV.or
  , EV.implies
  , EV.next
  , EV.nextN
  , EV.before
  , EV.timplies
  , EV.timpliesOverlapping
  , EV.always
  , EV.never

  -- * Asserts
  , EV.assert
  , EV.cover

  -- * Assertion checking
  , check
  , checkI

  -- * Functions to deal with assertion results
  , EV.hideAssertion
  , EV.mergeAssertions
  ) where


import qualified Clash.Explicit.Verification     as EV
import           Clash.Signal
  (KnownDomain, HiddenClock, HiddenReset, Signal, hasClock, hasReset)
import           Clash.Verification.Internal
import           Data.Text                       (Text)

check
  :: ( KnownDomain dom
     , HiddenClock dom
     , HiddenReset dom
     )
  => Text
  -- ^ Property name (used in reports and error messages)
  -> RenderAs
  -- ^ Assertion language to use in HDL
  -> Property dom
  -> Signal dom Assertion
check = EV.check hasClock hasReset

checkI
  :: ( KnownDomain dom
     , HiddenClock dom
     , HiddenReset dom
     )
  => Text
  -- ^ Property name (used in reports and error messages)
  -> RenderAs
  -- ^ Assertion language to use in HDL
  -> Property dom
  -> Signal dom a
  -> Signal dom a
checkI = EV.checkI hasClock hasReset
