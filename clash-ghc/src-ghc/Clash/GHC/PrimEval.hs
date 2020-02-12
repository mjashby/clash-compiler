module Clash.GHC.PrimEval where

import qualified Data.HashMap.Strict as HashMap

import Clash.Core.Evaluator.Models
import Clash.Core.Term

import Clash.GHC.PrimEval.Bit
import Clash.GHC.PrimEval.BitVector
import Clash.GHC.PrimEval.ByteArray
import Clash.GHC.PrimEval.Char
import Clash.GHC.PrimEval.Double
import Clash.GHC.PrimEval.EnumTag
import Clash.GHC.PrimEval.Float
import Clash.GHC.PrimEval.Int
import Clash.GHC.PrimEval.Integer
import Clash.GHC.PrimEval.Narrowings
import Clash.GHC.PrimEval.Word

-- If we try to evaluate a prim we haven't added an implementation for in the
-- primsMap, we simply return it back without trying to evaluate it. The only
-- time 'Nothing' is returned is when an attempt to evaluate a prim fails.
--
primEval :: EvalPrim
primEval env pi args =
  case HashMap.lookup (primName pi) primsMap of
    Just f  -> f env pi args
    Nothing -> return $ Just (VPrim pi args)
 where
  primsMap = mconcat
    [ bitPrims
    , bitVectorPrims
    , byteArrayPrims
    , charPrims
    , doublePrims
    , enumTagPrims
    , floatPrims
    , intPrims
    , integerPrims
    , narrowingPrims
    , wordPrims
    ]

