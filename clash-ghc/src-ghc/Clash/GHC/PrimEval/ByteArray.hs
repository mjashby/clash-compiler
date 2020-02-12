{-# LANGUAGE OverloadedStrings #-}

module Clash.GHC.PrimEval.ByteArray
  ( byteArrayPrims
  ) where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)

import Clash.Core.Evaluator.Models

import Clash.GHC.PrimEval.Common
import Clash.GHC.PrimEval.Convert

byteArrayPrims :: HashMap Text EvalPrim
byteArrayPrims = HashMap.fromList
  [
  ]

