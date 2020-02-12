{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}

module Clash.GHC.PrimEval.BitVector
  ( bitVectorPrims
  ) where

import qualified Data.Either as Either
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)

import Clash.Core.Evaluator.Models
import Clash.Core.Util (tyNatSize)

import Clash.GHC.PrimEval.Common

bitVectorPrims :: HashMap Text EvalPrim
bitVectorPrims = HashMap.fromList
  [ -- Constructors
    ("Clash.Sized.Internal.BitVector.BV", evalMissing)

    -- BitPack
  , ("Clash.Sized.Internal.BitVector.pack#", evalMissing)
  , ("Clash.Sized.Internal.BitVector.unpack#", evalMissing)


    -- Accessors
  , ("Clash.Sized.Internal.BitVector.size#", evalMissing)
  , ("Clash.Sized.Internal.BitVector.maxIndex#", evalMissing)

    -- Concatenation
  , ("Clash.Sized.Internal.BitVector.++#", evalMissing)

    -- Reduction
  , ("Clash.Sized.Internal.BitVector.reduceAnd#", evalMissing)
  , ("Clash.Sized.Internal.BitVector.reduceOr#", evalMissing)
  , ("Clash.Sized.Internal.BitVector.reduceXor#", evalMissing)

    -- Indexing
  , ("Clash.Sized.Internal.BitVector.index#", evalMissing)
  , ("Clash.Sized.Internal.BitVector.replaceBit#", evalMissing)
  , ("Clash.Sized.Internal.BitVector.setSlice#", evalMissing)
  , ("Clash.Sized.Internal.BitVector.slice#", evalMissing)
  , ("Clash.Sized.Internal.BitVector.split#", evalMissing)
  , ("Clash.Sized.Internal.BitVector.msb#", evalMissing)
  , ("Clash.Sized.Internal.BitVector.lsb#", evalMissing)

    -- Comparison
  , ("Clash.Sized.Internal.BitVector.eq#", evalMissing)
  , ("Clash.Sized.Internal.BitVector.neq#", evalMissing)
  , ("Clash.Sized.Internal.BitVector.lt#", evalMissing)
  , ("Clash.Sized.Internal.BitVector.ge#", evalMissing)
  , ("Clash.Sized.Internal.BitVector.gt#", evalMissing)
  , ("Clash.Sized.Internal.BitVector.le#", evalMissing)

    -- Bounds
  , ("Clash.Sized.Internal.BitVector.minBound#", evalMissing)
  , ("Clash.Sized.Internal.BitVector.maxBound#", evalMissing)

    -- Arithmetic
  , ("Clash.Sized.Internal.BitVector.+#", evalMissing)
  , ("Clash.Sized.Internal.BitVector.-#", evalMissing)
  , ("Clash.Sized.Internal.BitVector.*#", evalMissing)
  , ("Clash.Sized.Internal.BitVector.negate#", evalMissing)
  , ("Clash.Sized.Internal.BitVector.quot#", evalMissing)
  , ("Clash.Sized.Internal.BitVector.rem#", evalMissing)
  , ("Clash.Sized.Internal.BitVector.toInteger#", evalMissing)

    -- ExtendingNum
  , ("Clash.Sized.Internal.BitVector.plus#", evalMissing)
  , ("Clash.Sized.Internal.BitVector.minus#", evalMissing)
  , ("Clash.Sized.Internal.BitVector.times#", evalMissing)

    -- Bit operations
  , ("Clash.Sized.Internal.BitVector.and#", evalMissing)
  , ("Clash.Sized.Internal.BitVector.or#", evalMissing)
  , ("Clash.Sized.Internal.BitVector.xor#", evalMissing)
  , ("Clash.Sized.Internal.BitVector.complement#", evalMissing)
  , ("Clash.Sized.Internal.BitVector.shiftL#", evalMissing)
  , ("Clash.Sized.Internal.BitVector.shiftR#", evalMissing)
  , ("Clash.Sized.Internal.BitVector.rotateL#", evalMissing)
  , ("Clash.Sized.Internal.BitVector.rotateR#", evalMissing)

    -- Resize
  , ("Clash.Sized.Internal.BitVector.truncateB#", evalMissing)
  ]

