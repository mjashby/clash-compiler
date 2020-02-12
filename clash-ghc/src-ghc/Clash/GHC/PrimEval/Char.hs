{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}

module Clash.GHC.PrimEval.Char
  ( charPrims
  ) where

import qualified Data.Either as Either
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap (fromList)
import Data.Text (Text)
import GHC.Prim
import GHC.Types

import Clash.Core.Evaluator.Models
import Clash.Core.Term

import Clash.GHC.PrimEval.Common
import Clash.GHC.PrimEval.Convert

-- | Primitive Operations defined on Char / Char#
--
charPrims :: HashMap Text EvalPrim
charPrims = HashMap.fromList
  [ ("GHC.Prim.gtChar#", evalComparison# gtChar#)
  , ("GHC.Prim.geChar#", evalComparison# geChar#)
  , ("GHC.Prim.eqChar#", evalComparison# eqChar#)
  , ("GHC.Prim.neChar#", evalComparison# neChar#)
  , ("GHC.Prim.ltChar#", evalComparison# ltChar#)
  , ("GHC.Prim.leChar#", evalComparison# leChar#)
  , ("GHC.Prim.ord#", primOrd)
  , ("GHC.Types.C#", primC)
  ]

primOrd :: EvalPrim
primOrd = evalUnaryOp $ \i ->
  let !(C# a) = i in I# (ord# a)

primC :: EvalPrim
primC env pi args
  | ([], [charDc]) <- typeInfo (envTcMap env) (primType pi)
  , Just [i] <- traverse fromValue (Either.lefts args)
  = let !(C# a) = i
     in return . Just $ VData charDc
          [Left $ toValue (envTcMap env) (primType pi) (C# a)]

  | otherwise
  = return Nothing

evalComparison# :: (Char# -> Char# -> Int#) -> EvalPrim
evalComparison# op = evalBinaryOp $ \i j ->
  let !(C# a) = i
      !(C# b) = j
   in toEnum @Bool $ I# (a `op` b)

