{-# LANGUAGE OverloadedStrings #-}

module Clash.GHC.PrimEval.EnumTag
  ( enumTagPrims
  ) where

import qualified Data.Either as Either
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.List (find)
import Data.Text (Text)

import Clash.Core.DataCon
import Clash.Core.Evaluator.Models
import Clash.Core.TyCon
import Clash.Core.Type
import Clash.Unique

import Clash.GHC.PrimEval.Convert

enumTagPrims :: HashMap Text EvalPrim
enumTagPrims = HashMap.fromList
  [ ("GHC.Prim.tagToEnum#", primTagToEnum)
  ]

primTagToEnum :: EvalPrim
primTagToEnum env _ args
  | [ConstTy (TyCon tcN)] <- Either.rights args
  , Just [i] <- traverse fromValue (Either.lefts args)
  , Just tc <- lookupUniqMap tcN (envTcMap env)
  , Just dc <- find (\x -> dcTag x == i + 1) (tyConDataCons tc)
  = return . Just $ VData dc []

  | otherwise
  = return Nothing

