{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}

module Clash.GHC.PrimEval.Bit
  ( bitPrims
  ) where

import Prelude hiding (pi)

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)

import Clash.Sized.Internal.BitVector

import Clash.Core.Evaluator.Models
import Clash.Core.Term

import Clash.GHC.PrimEval.Common
import Clash.GHC.PrimEval.Convert

bitPrims :: HashMap Text EvalPrim
bitPrims = HashMap.fromList
  [ -- Constructor
    ("Clash.Sized.Internal.BitVector.Bit", evalBinaryOp Bit)

    -- Construction
  , ("Clash.Sized.Internal.BitVector.high", primHigh)
  , ("Clash.Sized.Internal.BitVector.low", primLow)

    -- Comparison
  , ("Clash.Sized.Internal.BitVector.eq##", evalBinaryOp eq##)
  , ("Clash.Sized.Internal.BitVector.neq##", evalBinaryOp neq##)
  , ("Clash.Sized.Internal.BitVector.lt##", evalBinaryOp lt##)
  , ("Clash.Sized.Internal.BitVector.ge##", evalBinaryOp ge##)
  , ("Clash.Sized.Internal.BitVector.gt##", evalBinaryOp gt##)
  , ("Clash.Sized.Internal.BitVector.le##", evalBinaryOp le##)

    -- Bit operations
  , ("Clash.Sized.Internal.BitVector.and##", evalBinaryOp and##)
  , ("Clash.Sized.Internal.BitVector.or##", evalBinaryOp or##)
  , ("Clash.Sized.Internal.BitVector.xor##", evalBinaryOp xor##)
  , ("Clash.Sized.Internal.BitVector.complement##", evalUnaryOp complement##)
  ]

primHigh :: EvalPrim
primHigh env pi _ =
  return . Just $ toValue (envTcMap env) (primType pi) (Bit 0 1)

primLow :: EvalPrim
primLow env pi _ =
  return . Just $ toValue (envTcMap env) (primType pi) (Bit 0 0)

