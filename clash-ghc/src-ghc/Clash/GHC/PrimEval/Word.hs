{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnboxedTuples #-}

module Clash.GHC.PrimEval.Word
  ( wordPrims
  ) where

import qualified Data.Either as Either
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)
import GHC.Prim
import GHC.Types

import Clash.Core.Evaluator.Models
import Clash.Core.Term
import Clash.Core.TyCon
import Clash.Core.Type
import Clash.Unique

import Clash.GHC.PrimEval.Common
import Clash.GHC.PrimEval.Convert

-- | Primitive Operatations defined on Word# and related
-- fixed-width types (Word{8,16,32,64})
--
wordPrims :: HashMap Text EvalPrim
wordPrims = HashMap.fromList
  [ ("GHC.Prim.plusWord#", evalBinaryOp# plusWord#)
  , ("GHC.Prim.addWordC#", evalBinaryOpIntC addWordC#)
  , ("GHC.Prim.subWordC#", evalBinaryOpIntC subWordC#)
  , ("GHC.Prim.plusWord2#", evalBinaryOpWord2 plusWord2#)
  , ("GHC.Prim.minusWord#", evalBinaryOp# minusWord#)
  , ("GHC.Prim.timesWord#", evalBinaryOp# timesWord#)
  , ("GHC.Prim.timesWord2#", evalBinaryOpWord2 timesWord2#)

    -- TODO These need catchDivByZero
  , ("GHC.Prim.quotWord#", evalMissing)
  , ("GHC.Prim.remWord#", evalMissing)
  , ("GHC.Prim.quotRemWord#", evalMissing)
  , ("GHC.Prim.quotRemWord2#", evalMissing)

  , ("GHC.Prim.and#", evalBinaryOp# and#)
  , ("GHC.Prim.or#", evalBinaryOp# or#)
  , ("GHC.Prim.xor#", evalBinaryOp# xor#)
  , ("GHC.Prim.not#", evalUnaryOp# not#)
  , ("GHC.Prim.uncheckedShiftL#", primUncheckedShiftL)
  , ("GHC.Prim.uncheckedShiftRL#", primUncheckedShiftRL)
  , ("GHC.Prim.word2Int#", primWord2Int)
  , ("GHC.Prim.gtWord#", evalComparison# gtWord#)
  , ("GHC.Prim.geWord#", evalComparison# geWord#)
  , ("GHC.Prim.eqWord#", evalComparison# eqWord#)
  , ("GHC.Prim.neWord#", evalComparison# neWord#)
  , ("GHC.Prim.ltWord#", evalComparison# ltWord#)
  , ("GHC.Prim.leWord#", evalComparison# leWord#)
  , ("GHC.Prim.popCnt8#", evalUnaryOp# popCnt8#)
  , ("GHC.Prim.popCnt16#", evalUnaryOp# popCnt16#)
  , ("GHC.Prim.popCnt32#", evalUnaryOp# popCnt32#)
  , ("GHC.Prim.popCnt64#", evalUnaryOp# popCnt64#)
  , ("GHC.Prim.popCnt#", evalUnaryOp# popCnt#)
  , ("GHC.Prim.pdep8#", evalBinaryOp# pdep8#)
  , ("GHC.Prim.pdep16#", evalBinaryOp# pdep16#)
  , ("GHC.Prim.pdep32#", evalBinaryOp# pdep32#)
  , ("GHC.Prim.pdep64#", evalBinaryOp# pdep64#)
  , ("GHC.Prim.pdep", evalBinaryOp# pdep#)
  , ("GHC.Prim.pext8#", evalBinaryOp# pext8#)
  , ("GHC.Prim.pext16#", evalBinaryOp# pext16#)
  , ("GHC.Prim.pext32#", evalBinaryOp# pext32#)
  , ("GHC.Prim.pext64#", evalBinaryOp# pext64#)
  , ("GHC.Prim.pext", evalBinaryOp# pext#)
  , ("GHC.Prim.clz8#", evalUnaryOp# clz8#)
  , ("GHC.Prim.clz16#", evalUnaryOp# clz16#)
  , ("GHC.Prim.clz32#", evalUnaryOp# clz32#)
  , ("GHC.Prim.clz64#", evalUnaryOp# clz64#)
  , ("GHC.Prim.clz#", evalUnaryOp# clz#)
  , ("GHC.Prim.ctz8#", evalUnaryOp# ctz8#)
  , ("GHC.Prim.ctz16#", evalUnaryOp# ctz16#)
  , ("GHC.Prim.ctz32#", evalUnaryOp# ctz32#)
  , ("GHC.Prim.ctz64#", evalUnaryOp# ctz64#)
  , ("GHC.Prim.ctz#", evalUnaryOp# ctz#)
  , ("GHC.Prim.byteSwap16#", evalUnaryOp# byteSwap16#)
  , ("GHC.Prim.byteSwap32#", evalUnaryOp# byteSwap32#)
  , ("GHC.Prim.byteSwap64#", evalUnaryOp# byteSwap64#)
  , ("GHC.Prim.byteSwap#", evalUnaryOp# byteSwap#)
  ]

primUncheckedShiftL :: EvalPrim
primUncheckedShiftL = evalBinaryOp $ \i j ->
  let !(W# a) = i
      !(I# b) = j
   in W# (uncheckedShiftL# a b)

primUncheckedShiftRL :: EvalPrim
primUncheckedShiftRL = evalBinaryOp $ \i j ->
  let !(W# a) = i
      !(I# b) = j
   in W# (uncheckedShiftRL# a b)

primWord2Int :: EvalPrim
primWord2Int = evalUnaryOp $ \i ->
  let !(W# a) = i in I# (word2Int# a)

-- TODO There must be a nice way to generalise evalBinaryOp#Word2 and evalBinaryOp#IntC

evalBinaryOpWord2 :: (Word# -> Word# -> (# Word#, Word# #)) -> EvalPrim
evalBinaryOpWord2 op env pi args
  | Just [i, j] <- traverse fromValue (Either.lefts args)
  , TyConApp tupTcNm tyArgs <- tyView . snd $ splitFunForallTy (primType pi)
  , Just tupTc <- lookupUniqMap tupTcNm (envTcMap env)
  , [tupDc] <- tyConDataCons tupTc
  = let !(W# a) = i
        !(W# b) = j
        !(# d, c #) = a `op` b
     in return . Just . VData tupDc $ mappend (fmap Right tyArgs)
          [ Left $ toValue tcm ty (W# d)
          , Left $ toValue tcm ty (W# c)
          ]
  | otherwise
  = return Nothing
 where
  tcm = envTcMap env
  ty  = primType pi

evalBinaryOpIntC :: (Word# -> Word# -> (# Word#, Int# #)) -> EvalPrim
evalBinaryOpIntC op env pi args
  | Just [i, j] <- traverse fromValue (Either.lefts args)
  , TyConApp tupTcNm tyArgs <- tyView . snd $ splitFunForallTy (primType pi)
  , Just tupTc <- lookupUniqMap tupTcNm (envTcMap env)
  , [tupDc] <- tyConDataCons tupTc
  = let !(W# a) = i
        !(W# b) = j
        !(# d, c #) = a `op` b
     in return . Just . VData tupDc $ mappend (fmap Right tyArgs)
          [ Left $ toValue tcm ty (W# d)
          , Left $ toValue tcm ty (I# c)
          ]
  | otherwise
  = return Nothing
 where
  tcm = envTcMap env
  ty  = primType pi

evalUnaryOp# :: (Word# -> Word#) -> EvalPrim
evalUnaryOp# op = evalUnaryOp $ \i ->
  let !(W# a) = i in W# (op a)

evalBinaryOp# :: (Word# -> Word# -> Word#) -> EvalPrim
evalBinaryOp# op = evalBinaryOp $ \i j ->
  let !(W# a) = i
      !(W# b) = j
   in W# (a `op` b)

evalComparison# :: (Word# -> Word# -> Int#) -> EvalPrim
evalComparison# op = evalBinaryOp $ \i j ->
  let !(W# a) = i
      !(W# b) = j
   in I# (a `op` b)

