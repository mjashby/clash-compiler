{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Clash.GHC.PrimEval.Convert where

import Data.Bits
import qualified Data.Either as Either
import Data.Primitive.ByteArray (ByteArray(..))
import Data.Proxy
import Data.Vector.Primitive (Vector(..))
import GHC.Integer.GMP.Internals
import GHC.Natural
import GHC.TypeNats (KnownNat, natVal)

import Clash.Sized.Internal.BitVector

import Clash.Core.DataCon
import Clash.Core.Evaluator.Models
import Clash.Core.Literal
import Clash.Core.Name
import Clash.Core.Term
import Clash.Core.TyCon
import Clash.Core.Type
import Clash.Core.TysPrim
import Clash.Core.Var
import Clash.Unique

-- Types which can be obtained from a value.
-- Values returned from this function *must* be WHNF.
--
class ToValue a where
  toValue :: TyConMap -> Type -> a -> Value

instance ToValue Bit where
  toValue tcm ty (Bit m v)
    | TyConApp bTcNm _ <- tyView ty
    = let funTy = foldr1 mkFunTy [integerPrimTy, integerPrimTy, mkTyConApp bTcNm []]
          pi = PrimInfo "Clash.Sized.Internal.BitVector.fromInteger##" funTy WorkNever
       in VPrim pi
            [ Left $ toValue tcm integerPrimTy (m .&. 1)
            , Left $ toValue tcm integerPrimTy (v .&. 1)
            ]

    | otherwise
    = error ("toValue: Expected Bit but given: " <> show ty)

instance (KnownNat n) => ToValue (BitVector n) where
  toValue tcm ty (BV m i)
    | TyConApp bvTcNm _ <- tyView ty
    = let pi = mkPrimInfo bvTcNm
       in VPrim pi
            [ Right undefined -- The type n
            , Left $ toValue tcm naturalPrimTy bvSize
            , Left $ toValue tcm integerPrimTy m
            , Left $ toValue tcm integerPrimTy i
            ]

    | otherwise
    = error ("toValue: Expected BitVector " <> show bvSize <> " but given " <> show ty)
   where
    bvSize = natVal (Proxy @n)

    mkPrimInfo tcNm =
      let nTv  = mkTyVar typeNatKind (mkUnsafeSystemName "n" 0)
          nVar = VarTy nTv
          pTy  = ForAllTy nTv $ foldr1 mkFunTy
                   [ naturalPrimTy
                   , integerPrimTy
                   , integerPrimTy
                   , mkTyConApp tcNm [nVar]
                   ]
      in PrimInfo "Clash.Sized.Internal.BitVector.fromInteger#" pTy WorkNever

instance ToValue Bool where
  toValue tcm ty
    | TyConApp boolTcNm [] <- tyView . snd $ splitFunForallTy ty
    , Just boolTc <- lookupUniqMap boolTcNm tcm
    , [falseDc, trueDc] <- tyConDataCons boolTc
    = \case
        True -> VData trueDc []
        False -> VData falseDc []

    | otherwise
    = error ("toValue: Expected Bool but given " <> show ty)

instance ToValue Char where
  toValue _ _ = VLit . CharLiteral

instance ToValue Double where
  toValue _ _ = VLit . DoubleLiteral . toRational

instance ToValue Float where
  toValue _ _ = VLit . FloatLiteral . toRational

instance ToValue Int where
  toValue _ _ = VLit . IntLiteral . toInteger

instance ToValue Integer where
  toValue _ _ = VLit . IntegerLiteral

instance ToValue Natural where
  toValue _ _ = VLit . NaturalLiteral . toInteger

instance ToValue Ordering where
  toValue tcm ty
    | TyConApp ordTcNm [] <- tyView . snd $ splitFunForallTy ty
    , Just ordTc <- lookupUniqMap ordTcNm tcm
    , [ltDc, eqDc, gtDc] <- tyConDataCons ordTc
    = \case
        LT -> VData ltDc []
        EQ -> VData eqDc []
        GT -> VData gtDc []

    | otherwise
    = error "toValue: Couldn't find 'Ordering' in TyConMap"

instance ToValue Word where
  toValue _ _ = VLit . WordLiteral . toInteger

-- Types which can be converted to a value.
--
class FromValue a where
  fromValue :: Value -> Maybe a

-- Bit literals are only valid if the mask is set to 0 (otherwise they are
-- undefined). If a value refers to an undefined bit Nothing is returned, as
-- this prevents the primitives from calling error on evaluation.
--
instance FromValue Bit where
  fromValue = \case
    VPrim pi args
      | primName pi == "Clash.Sized.Internal.BitVector.fromInteger##"
      , Just [0, v] <- traverse fromValue (Either.lefts args)
      -> Just (Bit 0 v)

    _ -> Nothing

{-
instance FromValue BitVector where
  fromValue = \case
    VPrim pi args
      | primName pi == "Clash.Sized.Internal.BitVector.fromInteger#"
      , Just 
-}

instance FromValue ByteArray where
  fromValue = \case
    VLit (ByteArrayLiteral (Vector _ _ ba)) -> Just ba
    _ -> Nothing

instance FromValue Char where
  fromValue = \case
    VLit (CharLiteral x) -> Just x
    _ -> Nothing

instance FromValue Double where
  fromValue = \case
    VLit (DoubleLiteral x) -> Just (fromRational x)
    _ -> Nothing

instance FromValue Float where
  fromValue = \case
    VLit (FloatLiteral x) -> Just (fromRational x)
    _ -> Nothing

instance FromValue Int where
  fromValue = \case
    VLit (IntLiteral x) -> Just (fromInteger x)
    _ -> Nothing

instance FromValue Integer where
  fromValue = \case
    VLit (IntegerLiteral x) -> Just x
    VData dc args ->
      case (dcTag dc, Either.lefts args) of
        (1, [v]) -> toInteger <$> fromValue @Int v
        (2, [v]) -> flip fmap (fromValue @ByteArray v)
          (\x -> let !(ByteArray ba) = x in Jp# (BN# ba))

        (3, [v]) -> flip fmap (fromValue @ByteArray v)
          (\x -> let !(ByteArray ba) = x in Jn# (BN# ba))

        _ -> Nothing
    _ -> Nothing

instance FromValue Natural where
  fromValue = \case
    VLit (NaturalLiteral x) -> Just (fromInteger x)
    VData dc args ->
      case (dcTag dc, Either.lefts args) of
        (1, [v]) -> wordToNatural <$> fromValue @Word v
        (2, [v]) -> flip fmap (fromValue @ByteArray v)
          (\x -> let !(ByteArray ba) = x in naturalFromInteger $ Jp# (BN# ba))

        _ -> Nothing
    _ -> Nothing

instance FromValue Rational where
  fromValue = \case
    VLit (DoubleLiteral x) -> Just x
    VLit (FloatLiteral x) -> Just x
    _ -> Nothing

instance FromValue Word where
  fromValue = \case
    VLit (WordLiteral x) -> Just (fromInteger x)
    _ -> Nothing

