{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnboxedTuples #-}

module Clash.GHC.PrimEval.Int
  ( intPrims
  ) where

import qualified Data.Either as Either
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap (fromList)
import Data.Text (Text)
import GHC.Float
import GHC.Prim
import GHC.Types

import Clash.Core.Evaluator.Models
import Clash.Core.Term
import Clash.Core.TyCon
import Clash.Core.Type
import Clash.Unique

import Clash.GHC.PrimEval.Common
import Clash.GHC.PrimEval.Convert

-- | Primitive Operations defined on Int#
--
intPrims :: HashMap Text EvalPrim
intPrims = HashMap.fromList
  [ ("GHC.Prim.+#", evalBinaryOp# (+#))
  , ("GHC.Prim.-#", evalBinaryOp# (-#))
  , ("GHC.Prim.*#", evalBinaryOp# (*#))
  , ("GHC.Prim.mulIntMayOflo#", evalBinaryOp# mulIntMayOflo#)

    -- TODO These need catchDivByZero
  , ("GHC.Prim.quotInt#", evalMissing)
  , ("GHC.Prim.remInt#", evalMissing)
  , ("GHC.Prim.quotRemInt#", evalMissing)

  , ("GHC.Prim.andI#", evalBinaryOp# andI#)
  , ("GHC.Prim.orI#", evalBinaryOp# orI#)
  , ("GHC.Prim.xorI#", evalBinaryOp# xorI#)
  , ("GHC.Prim.notI#", evalUnaryOp# notI#)
  , ("GHC.Prim.negateInt#", evalUnaryOp# negateInt#)
  , ("GHC.Prim.addIntC#", evalBinaryOpIntC addIntC#)
  , ("GHC.Prim.subIntC#", evalBinaryOpIntC subIntC#)
  , ("GHC.Prim.>#", evalBinaryOp# (>#))
  , ("GHC.Prim.>=#", evalBinaryOp# (>=#))
  , ("GHC.Prim.==#", evalBinaryOp# (==#))
  , ("GHC.Prim./=#", evalBinaryOp# (/=#))
  , ("GHC.Prim.<#", evalBinaryOp# (<#))
  , ("GHC.Prim.<=#", evalBinaryOp# (<=#))
  , ("GHC.Prim.chr#", primChr)
  , ("GHC.Prim.int2Word#", primInt2Word)
  , ("GHC.Prim.int2Float#", primInt2Float)
  , ("GHC.Prim.int2Double#", primInt2Double)
  , ("GHC.Prim.word2Float#", primWord2Float)
  , ("GHC.Prim.word2Double#", primWord2Double)
  , ("GHC.Prim.uncheckedIShiftL#", evalBinaryOp# uncheckedIShiftL#)
  , ("GHC.Prim.uncheckedIShiftRA#", evalBinaryOp# uncheckedIShiftRA#)
  , ("GHC.Prim.uncheckedIShiftRL#", evalBinaryOp# uncheckedIShiftRL#)
  ]

primChr :: EvalPrim
primChr = evalUnaryOp $ \i ->
  let !(I# a) = i in C# (chr# a)

primInt2Word :: EvalPrim
primInt2Word = evalUnaryOp $ \i ->
  let !(I# a) = i in W# (int2Word# a)

primInt2Float :: EvalPrim
primInt2Float = evalUnaryOp $ \i ->
  let !(I# a) = i in F# (int2Float# a)

primInt2Double :: EvalPrim
primInt2Double = evalUnaryOp $ \i ->
  let !(I# a) = i in D# (int2Double# a)

primWord2Float :: EvalPrim
primWord2Float = evalUnaryOp $ \i ->
  let !(W# a) = i in F# (word2Float# a)

primWord2Double :: EvalPrim
primWord2Double = evalUnaryOp $ \i ->
  let !(W# a) = i in D# (word2Double# a)

evalBinaryOpIntC :: (Int# -> Int# -> (# Int#, Int# #)) -> EvalPrim
evalBinaryOpIntC op env pi args
  | Just [i, j] <- traverse fromValue (Either.lefts args)
  , TyConApp tupTcNm tyArgs <- tyView . snd $ splitFunForallTy (primType pi)
  , Just tupTc <- lookupUniqMap tupTcNm (envTcMap env)
  , [tupDc] <- tyConDataCons tupTc
  = let !(I# a) = i
        !(I# b) = j
        !(# d, c #) = a `op` b
     in return . Just . VData tupDc $ mappend (fmap Right tyArgs)
          [ Left $ toValue tcm ty (I# d)
          , Left $ toValue tcm ty (I# c)
          ]
  | otherwise
  = return Nothing
 where
  tcm = envTcMap env
  ty  = primType pi

evalUnaryOp# :: (Int# -> Int#) -> EvalPrim
evalUnaryOp# op = evalUnaryOp $ \i ->
  let !(I# a) = i in I# (op a)

evalBinaryOp# :: (Int# -> Int# -> Int#) -> EvalPrim
evalBinaryOp# op = evalBinaryOp $ \i j ->
  let !(I# a) = i
      !(I# b) = j
   in I# (a `op` b)

