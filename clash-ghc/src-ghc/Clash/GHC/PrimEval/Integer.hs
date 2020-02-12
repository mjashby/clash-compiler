{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnboxedTuples #-}

module Clash.GHC.PrimEval.Integer
  ( integerPrims
  ) where

import qualified Data.Either as Either
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)
import GHC.Integer
import GHC.Integer.Logarithms
import GHC.Types
import GHC.Prim

import Clash.Core.Evaluator.Models
import Clash.Core.Term
import Clash.Core.TyCon
import Clash.Core.Type
import Clash.Unique

import Clash.GHC.PrimEval.Common
import Clash.GHC.PrimEval.Convert

integerPrims :: HashMap Text EvalPrim
integerPrims = HashMap.fromList
  [ ("GHC.Integer.Logarithms.integerLogBase#", primIntegerLogBase)

    -- Construct Integers
  , ("GHC.Integer.Type.smallInteger", primSmallInteger)
  , ("GHC.Integer.Type.wordToInteger", primWordToInteger)

    -- Conversion to other integral types
  , ("GHC.Integer.Type.integerToWord", primIntegerToWord)
  , ("GHC.Integer.Type.integerToInt", primIntegerToInt)

    -- Helpers for RealFloat type-class operations
  , ("GHC.Integer.Type.encodeFloatInteger", primEncodeFloatInteger)
  , ("GHC.Integer.Type.floatFromInteger", primFloatFromInteger)
  , ("GHC.Integer.Type.encodeDoubleInteger", primEncodeDoubleInteger)
  , ("GHC.Integer.Type.decodeDoubleInteger", primDecodeDoubleInteger)
  , ("GHC.Integer.Type.doubleFromInteger", primDoubleFromInteger)

    -- Arithmetic operations
  , ("GHC.Integer.Type.plusInteger", evalBinaryOp plusInteger)
  , ("GHC.Integer.Type.minusInteger", evalBinaryOp minusInteger)
  , ("GHC.Integer.Type.timesInteger", evalBinaryOp timesInteger)
  , ("GHC.Integer.Type.negateInteger", evalUnaryOp negateInteger)
  , ("GHC.Integer.Type.absInteger", evalUnaryOp absInteger)
  , ("GHC.Integer.Type.signumInteger", evalUnaryOp signumInteger)
  , ("GHC.Integer.Type.divModInteger", evalMissing)
  , ("GHC.Integer.Type.divInteger", evalMissing)
  , ("GHC.Integer.Type.modInteger", evalMissing)
  , ("GHC.Integer.Type.quotRemInteger", evalMissing)
  , ("GHC.Integer.Type.quotInteger", evalMissing)
  , ("GHC.Integer.Type.remInteger", evalMissing)

    -- Comparison predicates
  , ("GHC.Integer.Type.eqInteger", evalBinaryOp eqInteger)
  , ("GHC.Integer.Type.neqInteger", evalBinaryOp neqInteger)
  , ("GHC.Integer.Type.leInteger", evalBinaryOp leInteger)
  , ("GHC.Integer.Type.gtInteger", evalBinaryOp gtInteger)
  , ("GHC.Integer.Type.ltInteger", evalBinaryOp ltInteger)
  , ("GHC.Integer.Type.geInteger", evalBinaryOp geInteger)
  , ("GHC.Integer.Type.compareInteger", evalBinaryOp compareInteger)

    -- Int#-boolean valued verisons of comparison predicates
  , ("GHC.Integer.Type.eqInteger#", evalComparison# eqInteger#)
  , ("GHC.Integer.Type.neqInteger#", evalComparison# neqInteger#)
  , ("GHC.Integer.Type.leInteger#", evalComparison# leInteger#)
  , ("GHC.Integer.Type.gtInteger#", evalComparison# gtInteger#)
  , ("GHC.Integer.Type.ltInteger#", evalComparison# ltInteger#)
  , ("GHC.Integer.Type.geInteger#", evalComparison# geInteger#)

    -- Bit-operations
  , ("GHC.Integer.Type.andInteger", evalBinaryOp andInteger)
  , ("GHC.Integer.Type.orInteger", evalBinaryOp orInteger)
  , ("GHC.Integer.Type.xorInteger", evalBinaryOp xorInteger)
  , ("GHC.Integer.Type.complementInteger", evalUnaryOp complementInteger)
  , ("GHC.Integer.Type.shiftLInteger", primShiftLInteger)
  , ("GHC.Integer.Type.shiftRInteger", primShiftRInteger)
  , ("GHC.Integer.Type.testBitInteger", primTestBitInteger)

    -- Hashing
  , ("GHC.Integer.Type.hashInteger", primHashInteger)
  ]

primIntegerLogBase :: EvalPrim
primIntegerLogBase = evalBinaryOp $ \i j ->
  toInteger $ I# (integerLogBase# i j)

primSmallInteger :: EvalPrim
primSmallInteger = evalUnaryOp $ \i ->
  let !(I# a) = i in smallInteger a

primWordToInteger :: EvalPrim
primWordToInteger = evalUnaryOp $ \i ->
  let !(W# a) = i in wordToInteger a

primIntegerToWord :: EvalPrim
primIntegerToWord = evalUnaryOp $ \i ->
  W# (integerToWord i)

primIntegerToInt :: EvalPrim
primIntegerToInt = evalUnaryOp $ \i ->
  I# (integerToInt i)

primEncodeFloatInteger :: EvalPrim
primEncodeFloatInteger = evalBinaryOp $ \i j ->
  let !(I# a) = j in F# (encodeFloatInteger i a)

primFloatFromInteger :: EvalPrim
primFloatFromInteger = evalUnaryOp $ \i ->
  F# (floatFromInteger i)

primEncodeDoubleInteger :: EvalPrim
primEncodeDoubleInteger = evalBinaryOp $ \i j ->
  let !(I# a) = j in D# (encodeDoubleInteger i a)

primDecodeDoubleInteger :: EvalPrim
primDecodeDoubleInteger env pi args
  | Just [i] <- traverse fromValue (Either.lefts args)
  , TyConApp tupTcNm tyArgs <- tyView . snd $ splitFunForallTy (primType pi)
  , Just tupTc <- lookupUniqMap tupTcNm (envTcMap env)
  , [tupDc] <- tyConDataCons tupTc
  = let !(D# a) = i
        !(# b, c #) = decodeDoubleInteger a
     in return . Just . VData tupDc $ mappend (fmap Right tyArgs)
          [ Left $ toValue tcm ty b
          , Left $ toValue tcm ty (I# c)
          ]
  | otherwise
  = return Nothing
 where
  tcm = envTcMap env
  ty  = primType pi

primDoubleFromInteger :: EvalPrim
primDoubleFromInteger = evalUnaryOp $ \i ->
  D# (doubleFromInteger i)

primShiftLInteger :: EvalPrim
primShiftLInteger = evalBinaryOp $ \i j ->
  let !(I# a) = j in shiftLInteger i a

primShiftRInteger :: EvalPrim
primShiftRInteger = evalBinaryOp $ \i j ->
  let !(I# a) = j in shiftRInteger i a

primTestBitInteger :: EvalPrim
primTestBitInteger = evalBinaryOp $ \i j ->
  let !(I# a) = j in testBitInteger i a

primHashInteger :: EvalPrim
primHashInteger = evalUnaryOp $ \i ->
  I# (hashInteger i)

evalComparison# :: (Integer -> Integer -> Int#) -> EvalPrim
evalComparison# op = evalBinaryOp (\i j -> I# (i `op` j))

