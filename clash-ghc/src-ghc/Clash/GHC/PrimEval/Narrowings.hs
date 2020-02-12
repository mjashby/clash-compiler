{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}

module Clash.GHC.PrimEval.Narrowings
  ( narrowingPrims
  ) where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)
import GHC.Prim
import GHC.Types

import Clash.Core.Evaluator.Models (EvalPrim)

import Clash.GHC.PrimEval.Common

-- | Primitive Operations to narrow native sized Int / Word.
--
narrowingPrims :: HashMap Text EvalPrim
narrowingPrims = HashMap.fromList
  [ ("GHC.Prim.narrow8Int#", evalNarrowI# narrow8Int#)
  , ("GHC.Prim.narrow16Int#", evalNarrowI# narrow16Int#)
  , ("GHC.Prim.narrow32Int#", evalNarrowI# narrow32Int#)
  , ("GHC.Prim.narrow8Word#", evalNarrowW# narrow8Word#)
  , ("GHC.Prim.narrow16Word#", evalNarrowW# narrow16Word#)
  , ("GHC.Prim.narrow32Word#", evalNarrowW# narrow32Word#)
  ]

evalNarrowI# :: (Int# -> Int#) -> EvalPrim
evalNarrowI# op = evalUnaryOp $ \i ->
  let !(I# a) = i in I# (op a)

evalNarrowW# :: (Word# -> Word#) -> EvalPrim
evalNarrowW# op = evalUnaryOp $ \i ->
  let !(W# a) = i in W# (op a)

