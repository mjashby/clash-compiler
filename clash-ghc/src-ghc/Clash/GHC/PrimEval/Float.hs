{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnboxedTuples #-}

module Clash.GHC.PrimEval.Float
  ( floatPrims
  ) where

import Prelude hiding (pi)

import qualified Data.Either as Either
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)
import GHC.Prim
import GHC.Types

import Clash.Core.Evaluator.Models
import Clash.Core.Term

import Clash.GHC.PrimEval.Common
import Clash.GHC.PrimEval.Convert

floatPrims :: HashMap Text EvalPrim
floatPrims = HashMap.fromList
  [ ("GHC.Prim.gtFloat#", evalComparison# gtFloat#)
  , ("GHC.Prim.geFloat#", evalComparison# geFloat#)
  , ("GHC.Prim.eqFloat#", evalComparison# eqFloat#)
  , ("GHC.Prim.neFloat#", evalComparison# neFloat#)
  , ("GHC.Prim.ltFloat#", evalComparison# ltFloat#)
  , ("GHC.Prim.leFloat#", evalComparison# leFloat#)
  , ("GHC.Prim.plusFloat#", evalBinaryOp# plusFloat#)
  , ("GHC.Prim.minusFloat#", evalBinaryOp# minusFloat#)
  , ("GHC.Prim.timesFloat#", evalBinaryOp# timesFloat#)
  , ("GHC.Prim.divideFloat#", evalBinaryOp# divideFloat#)
  , ("GHC.Prim.negateFloat#", evalUnaryOp# negateFloat#)
  , ("GHC.Prim.fabsFloat#", evalUnaryOp# fabsFloat#)
  , ("GHC.Prim.float2Int#", primFloat2Int)
  , ("GHC.Prim.expFloat#", evalUnaryOp# expFloat#)
  , ("GHC.Prim.logFloat#", evalUnaryOp# logFloat#)
  , ("GHC.Prim.sqrtFloat#", evalUnaryOp# sqrtFloat#)
  , ("GHC.Prim.sinFloat#", evalUnaryOp# sinFloat#)
  , ("GHC.Prim.cosFloat#", evalUnaryOp# cosFloat#)
  , ("GHC.Prim.tanFloat#", evalUnaryOp# tanFloat#)
  , ("GHC.Prim.asinFloat#", evalUnaryOp# asinFloat#)
  , ("GHC.Prim.acosFloat#", evalUnaryOp# acosFloat#)
  , ("GHC.Prim.atanFloat#", evalUnaryOp# atanFloat#)
  , ("GHC.Prim.sinhFloat#", evalUnaryOp# sinhFloat#)
  , ("GHC.Prim.coshFloat#", evalUnaryOp# coshFloat#)
  , ("GHC.Prim.tanhFloat#", evalUnaryOp# tanhFloat#)

#if MIN_VERSION_ghc(8,7,0)
  , ("GHC.Prim.asinhFloat#", evalUnaryOp# asinhFloat#)
  , ("GHC.Prim.acoshFloat#", evalUnaryOp# acoshFloat#)
  , ("GHC.Prim.atanhFloat#", evalUnaryOp# atanhFloat#)
#endif

#if MIN_VERSION_base(4,12,0)
  , ("GHC.Float.$w$casinh1", primAsinhSpecialized)
#endif

  , ("GHC.Prim.powerFloat#", evalBinaryOp# powerFloat#)
  , ("GHC.Prim.float2Double#", primFloat2Double)
  , ("GHC.Prim.decodeFloat_Int#", primDecodeFloat_Int)
  ]

primFloat2Int :: EvalPrim
primFloat2Int = evalUnaryOp $ \i
  -> let !(F# a) = i in I# (float2Int# a)

#if MIN_VERSION_base(4,12,0)
primAsinhSpecialized :: EvalPrim
primAsinhSpecialized = evalUnaryOp# $ \i ->
  let !(F# a) = asinh (F# i) in a
#endif

primFloat2Double :: EvalPrim
primFloat2Double = evalUnaryOp $ \i ->
  let !(F# a) = i in D# (float2Double# a)

primDecodeFloat_Int :: EvalPrim
primDecodeFloat_Int env pi args
  | Just [i] <- traverse fromValue (Either.lefts args)
  , (tyArgs, [tupDc]) <- typeInfo tcm ty
  = let !(F# a) = i
        !(# p, q #) = decodeFloat_Int# a
     in return . Just . VData tupDc $ mappend (fmap Right tyArgs)
          [ Left $ toValue tcm ty (I# p)
          , Left $ toValue tcm ty (I# q)
          ]
  | otherwise
  = return Nothing
 where
  tcm = envTcMap env
  ty  = primType pi

evalUnaryOp# :: (Float# -> Float#) -> EvalPrim
evalUnaryOp# op = evalUnaryOp $ \i ->
  let !(F# a) = i
   in F# (op a)

evalBinaryOp# :: (Float# -> Float# -> Float#) -> EvalPrim
evalBinaryOp# op = evalBinaryOp $ \i j ->
  let !(F# a) = i
      !(F# b) = j
   in F# (a `op` b)

evalComparison# :: (Float# -> Float# -> Int#) -> EvalPrim
evalComparison# op = evalBinaryOp $ \i j ->
  let !(F# a) = i
      !(F# b) = j
   in I# (a `op` b)

