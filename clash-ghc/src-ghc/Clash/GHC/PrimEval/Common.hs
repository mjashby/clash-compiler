module Clash.GHC.PrimEval.Common where

import qualified Control.Monad.Except as Except
import qualified Data.Either as Either

import Clash.Core.DataCon
import Clash.Core.Evaluator.Models
import Clash.Core.Term
import Clash.Core.TyCon
import Clash.Core.Type
import Clash.Core.Util (piResultTys, tyNatSize)
import Clash.Unique (lookupUniqMap)

import Clash.GHC.PrimEval.Convert

-- | For primitives which are themselves values, simply rewrap the PrimInfo
-- and arguments back into VPrim.
--
evalId :: EvalPrim
evalId _ pi args = return $ Just (VPrim pi args)


-- TODO Rebase and use the nice interpolation errors here.
evalMissing :: EvalPrim
evalMissing _ pi _ =
  error ("No implementation for prim: " <> show (primName pi))

-- Evaluate a generic unary op, provided a means to extract a literal
-- from the input value, and convert the output to a value.
--
-- TODO: Does primType pi need to be changed to only the RHS of the last ->
--
evalUnaryOp
  :: (FromValue a, ToValue b)
  => (a -> b)
  -> EvalPrim
evalUnaryOp op env pi args
  | Just [x] <- traverse fromValue (Either.lefts args)
  = return . Just $ toValue (envTcMap env) (primType pi) (op x)

  | otherwise
  = return Nothing

-- Evaluate a generic binary op, provided a means to extract literals
-- from the input values, and convert the output to a value.
--
-- TODO: Does primType pi need to be changed to only the RHS of the last ->
--
evalBinaryOp
  :: (FromValue a, FromValue b, ToValue c)
  => (a -> b -> c)
  -> EvalPrim
evalBinaryOp op env pi args
  | [xV, yV] <- Either.lefts args
  , Just x <- fromValue xV
  , Just y <- fromValue yV
  = return . Just $ toValue (envTcMap env) (primType pi) (x `op` y)

  | otherwise
  = return Nothing

-- Given a type, extract information about the type parameters and data
-- constructors availble, e.g.
--
--   e.g. tyConsInfo boolTy == ([], [trueDc, falseDc])
--
typeInfo :: TyConMap -> Type -> ([Type], [DataCon])
typeInfo tcm ty
  | TyConApp tcNm tyArgs <- tyView . snd $ splitFunForallTy ty
  , Just tc <- lookupUniqMap tcNm tcm
  = (tyArgs, tyConDataCons tc)

  | otherwise
  = error ("tyConsInfo: No TyCons for " <> show ty)

-- Given a type which starts with some size, e.g.
-- 
--   forall (n :: Nat). (KnownNat n) => A n
--
-- extract a triple of the base type, size type, and applied size, e.g.
--
--   typeSizeInfo tcm A [Nat, 3] == (A, Nat, 3)
--
typeSizeInfo :: TyConMap -> Type -> [Type] -> Maybe (Type, Type, Integer)
typeSizeInfo tcm ty tys =
  case knownNatInfo tcm tys of
    Just (sizeTy, size) -> Just (resTy, sizeTy, size)
    _ -> Nothing
 where
  resTy = resultType tcm ty tys

-- Given a type which starts with a KnownNat constraint, e.g.
--
--   (KnownNat n) => A n
--
-- extract a tuple of the size type and applied size, e.g.
--
--   knownNatInfo tcm [Nat, 3] == Just (Nat, 3)
--
knownNatInfo :: TyConMap -> [Type] -> Maybe (Type, Integer)
knownNatInfo tcm (sizeTy:_)
  | Right size <- Except.runExcept (tyNatSize tcm sizeTy) = Just (sizeTy, size)
  | otherwise = Nothing
knownNatInfo _ _ = Nothing

resultType :: TyConMap -> Type -> [Type] -> Type
resultType tcm ty =
  snd . splitFunForallTy . piResultTys tcm ty

