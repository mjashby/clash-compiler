{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Clash.Core.Evaluator.Semantics
  ( partialEval
  , evaluate
  , quote
  ) where

import Prelude hiding (pi)

import Control.Concurrent.Supply (Supply)
import Data.Bitraversable (bitraverse)
import qualified Data.Either as Either

import BasicTypes (InlineSpec(..))

import Clash.Core.DataCon
import Clash.Core.Evaluator.Delay
import Clash.Core.Evaluator.Models
import Clash.Core.Term
import Clash.Core.TyCon
import Clash.Core.Type
import Clash.Core.Var
import Clash.Core.VarEnv
import Clash.Driver.Types

-- TODO: In practice this may not terminate. If this ends up being a problem
-- we should impose a limit on how many times we are acceptable with being
-- delayed - returning the original subterm if we exceed this.
--
partialEval
  :: EvalPrim
  -> VarEnv Term
  -> BindingMap
  -> TyConMap
  -> InScopeSet
  -> Supply
  -> Term
  -> Nf
partialEval eval ps bm tcm is ids x =
  runDelay (evaluate (mkEnv eval ps bm tcm is ids) x >>= quote)
{-# SCC partialEval #-}

-- TODO Currently, a globally bound term which is not WHNF is re-evaluated
-- every time it is looked up in the environment. We should keep this result
-- so we only evaluate each global once.
--
-- Does this mean changing Delay to StateT Env Delay ?
--
evaluate :: Env -> Term -> Delay Value
evaluate env = \case
  Var v -> evaluateVar env v
  Data dc -> return (VData dc [])
  Literal l -> return (VLit l)
  Prim pi -> return (VPrim pi [])
  Lam x e -> return (VLam x e env)
  TyLam x e -> return (VTyLam x e env)
  App x y -> evaluateApp env x y
  TyApp x ty -> evaluateTyApp env x ty
  Letrec bs e -> evaluateLetrec env bs e
  Case e ty xs -> evaluateCase env e ty xs
  Cast x a b -> evaluateCast env x a b
  Tick ti x -> evaluateTick env x ti
{-# SCC evaluate #-}

evaluateVar :: Env -> Id -> Delay Value
evaluateVar e i
  | Just etv <- lookupVarEnv i (envLocals e)
  = go LocalId etv

  | Just (s, etv) <- lookupVarEnv i (envGlobals e)
  , s == Inline || s == Inlinable
  = go GlobalId etv

  | otherwise
  = return $ VNeu (NeVar i)
 where
  go s = either (evaluate (deleteEnv s i e)) return
{-# SCC evaluateVar #-}

evaluateApp :: Env -> Term -> Term -> Delay Value
evaluateApp env x y = do
  evalX <- evaluate env x
  evalY <- evaluate env y
  evalApplication Left apply env evalX evalY
{-# SCC evaluateApp #-}

evaluateTyApp :: Env -> Term -> Type -> Delay Value
evaluateTyApp env x ty = do
  evalX <- evaluate env x
  evalApplication Right applyTy env evalX ty
{-# SCC evaluateTyApp #-}

evalApplication
  :: (r -> Either Value Type)
  -> (Value -> r -> Delay Value)
  -> Env
  -> Value
  -> r
  -> Delay Value
evalApplication toArg f env x y = do
  case x of
    VData dc args -> applyToData dc args
    VPrim pi args -> applyToPrim pi args
    _ -> f x y
 where
  -- TODO applyToData and applyToPrim are basically the same function.
  -- Abstract this further?

  applyToData dc args =
    case compare (length tys) (length args') of
      LT -> error "applyToData: Overapplied DC"
      EQ -> return (VData dc args')
      GT -> return (VData dc args')
   where
    args' = args <> [toArg y]
    tys = fst $ splitFunForallTy (dcType dc)

  applyToPrim pi args =
    case compare (length tys) (length args') of
      LT -> error "applyToPrim: Overapplied prim"
      EQ -> envPrimEval env env pi args >>= \case
        Nothing -> error "applyToPrim: Could not evaluate prim"
        Just v  -> return v
      GT -> return (VPrim pi args')
   where
    args' = args <> [toArg y]
    tys = fst $ splitFunForallTy (primType pi)
{-# SCC evalApplication #-}

evaluateLetrec :: Env -> [LetBinding] -> Term -> Delay Value
evaluateLetrec env bs x = do
  evalBs <- traverse (bitraverse return (fmap Right . evaluate env)) bs
  let env' = foldr (uncurry $ extendEnv LocalId) env evalBs

  evaluate env' x
{-# SCC evaluateLetrec #-}

evaluateCase :: Env -> Term -> Type -> [Alt] -> Delay Value
evaluateCase env x ty xs = do
  evalX <- evaluate env x

  case evalX of
    VLit l -> litCase l
    VData dc args -> dataCase dc args
    VPrim pi args -> primCase pi args
    v -> neuCase v
 where
  neuCase s =
    fmap (VNeu . NeCase s ty) (traverse (evaluateAlt env) xs)

  litCase l =
    let matches = \case
          -- TODO Some DataPat are really literals (Integer, Natural)
          DataPat{} -> False
          _ -> True

        eval (pat, e) = case pat of
          LitPat a
            | l == a -> evaluate env e
          DefaultPat -> evaluate env e
          -- TODO: We hit this now
          _ -> error ("litCase: Cannot match on " <> show pat)

     in evalAlts env matches eval xs

  evalDataPat args tvs ids e =
    let tys  = zip tvs (Either.rights args)
        tms  = zip ids (Either.lefts args)
        env' = env
                { envLocals = extendVarEnvList (envLocals env) (fmap Right <$> tms)
                , envTypes  = extendVarEnvList (envTypes env) tys
                -- TODO Extend InScopeSet ?
                }
     in evaluate env' e

  dataCase dc args =
    let matches = \case
          DataPat c _ _ -> dc == c
          LitPat _ -> False
          DefaultPat -> True

        eval (pat, e) = case pat of
          DataPat _ tvs ids -> evalDataPat args tvs ids e
          DefaultPat -> evaluate env e
          _ -> error ("dataCase: Cannot match on pattern " <> show pat)

     in evalAlts env matches eval xs

  primCase pi args =
    let eval (pat, e) = case pat of
          DataPat _ tvs ids -> evalDataPat args tvs ids e
          _ -> evaluate env e

     in evalAlts env (const True) eval xs
{-# SCC evaluateCase #-}

-- Evalaute an alternative without selecting it.
-- This is used when a case expression is neutral.
--
evaluateAlt :: Env -> Alt -> Delay (Pat, Value)
evaluateAlt env (pat, x)
  | DataPat dc tvs ids <- pat
  = let tys  = fmap (\tv -> (tv, VarTy tv)) tvs
        tms  = fmap (\i  -> (i, Right . VNeu $ NeVar i)) ids
        env' = env
                { envLocals = extendVarEnvList (envLocals env) tms
                , envTypes  = extendVarEnvList (envTypes env) tys
                -- TODO InScopeSet ?
                }
     in (pat,) <$> evaluate env' x

  | otherwise
  = (pat,) <$> evaluate env x
{-# SCC evaluateAlt #-}

findBestMatch :: (Pat -> Bool) -> [Alt] -> Alt
findBestMatch isMatch alts =
  case filter (isMatch . fst) alts of
    (x:xs) -> bestMatch x xs
    [] -> error "findBestMatch: No matching patterns for case"
 where
  bestMatch (DefaultPat, e) [] = (DefaultPat, e)
  bestMatch (DefaultPat, _) xs = head xs
  bestMatch alt _ = alt

evalAlts :: Env -> (Pat -> Bool) -> (Alt -> Delay Value) -> [Alt] -> Delay Value
evalAlts env isMatch eval alts =
  case filter (isMatch . fst) alts of
    [] -> error "evalAlts: No matching alternatives for case"
    (x:xs) -> eval (bestMatch x xs)
 where
  bestMatch (DefaultPat, e) [] = (DefaultPat, e)
  bestMatch (DefaultPat, _) xs = head xs
  bestMatch alt _ = alt
{-# SCC evalAlts #-}

evaluateCast :: Env -> Term -> Type -> Type -> Delay Value
evaluateCast env x a b = do
  evalX <- evaluate env x
  return (VCast evalX a b)
{-# SCC evaluateCast #-}

evaluateTick :: Env -> Term -> TickInfo -> Delay Value
evaluateTick env x ti = do
  evalX <- evaluate env x
  return (VTick evalX ti)
{-# SCC evaluateTick #-}

apply :: Value -> Value -> Delay Value
apply (collectValueTicks -> (v1, ts)) v2 =
  case v1 of
    VNeu n -> return (addTicks (VNeu (NeApp n v2)) ts)

    VLam x e env ->
      let val = evaluate (extendEnv LocalId x (Right v2) env) e
       in delay (fmap (`addTicks` ts) val)

    _ -> error ("apply: Cannot apply value to " <> show v1)
{-# SCC apply #-}

applyTy :: Value -> Type -> Delay Value
applyTy (collectValueTicks -> (v, ts)) ty =
  case v of
    VNeu n ->
      return (addTicks (VNeu (NeTyApp n ty)) ts)

    VTyLam x e env ->
      let val = evaluate (extendEnvTy x ty env) e
       in delay (fmap (`addTicks` ts) val)

    _ -> error ("applyTy: Cannot apply type to " <> show v)
{-# SCC applyTy #-}

quote :: Value -> Delay Nf
quote = \case
  VData dc args -> quoteData dc args
  VLit l -> return (NLit l)
  VPrim pi args -> quotePrim pi args
  VLam x e env -> quoteLam x e env
  VTyLam x e env -> quoteTyLam x e env
  VCast x a b -> quoteCast x a b
  VTick x ti -> quoteTick x ti
  VNeu n -> NNeu <$> quoteNeutral n
{-# SCC quote #-}

quoteData :: DataCon -> [Either Value Type] -> Delay Nf
quoteData dc args = do
  quoteArgs <- traverse (bitraverse quote return) args
  return (NData dc quoteArgs)
{-# SCC quoteData #-}

quotePrim :: PrimInfo -> [Either Value Type] -> Delay Nf
quotePrim pi args = do
  quoteArgs <- traverse (bitraverse quote return) args
  return (NPrim pi quoteArgs)
{-# SCC quotePrim #-}

quoteLam :: Id -> Term -> Env -> Delay Nf
quoteLam x e env = do
  evalE  <- apply (VLam x e env) (VNeu (NeVar x))
  quoteE <- quote evalE
  return (NLam x quoteE)
{-# SCC quoteLam #-}

-- TODO This presumably is also where it hangs
quoteTyLam :: TyVar -> Term -> Env -> Delay Nf
quoteTyLam x e env = do
  evalE  <- applyTy (VTyLam x e env) (VarTy x)
  quoteE <- quote evalE
  return (NTyLam x quoteE)
{-# SCC quoteTyLam #-}

quoteCast :: Value -> Type -> Type -> Delay Nf
quoteCast x a b = do
  quoteX <- quote x
  return (NCast quoteX a b)
{-# SCC quoteCast #-}

quoteTick :: Value -> TickInfo -> Delay Nf
quoteTick x ti = do
  quoteX <- quote x
  return (NTick quoteX ti)
{-# SCC quoteTick #-}

quoteNeutral :: Neutral Value -> Delay (Neutral Nf)
quoteNeutral = \case
  NeVar v -> return (NeVar v)
  NeApp x y -> quoteApp x y
  NeTyApp x ty -> quoteTyApp x ty
  NeCase x ty xs -> quoteCase x ty xs
{-# SCC quoteNeutral #-}

quoteApp :: Neutral Value -> Value -> Delay (Neutral Nf)
quoteApp x y = do
  quoteX <- quoteNeutral x
  quoteY <- quote y
  return (NeApp quoteX quoteY)
{-# SCC quoteApp #-}

quoteTyApp :: Neutral Value -> Type -> Delay (Neutral Nf)
quoteTyApp x ty = do
  quoteX <- quoteNeutral x
  return (NeTyApp quoteX ty)
{-# SCC quoteTyApp #-}

quoteCase :: Value -> Type -> [(Pat, Value)] -> Delay (Neutral Nf)
quoteCase x ty xs = do
  quoteX  <- quote x
  quoteXs <- traverse (bitraverse return quote) xs
  return (NeCase quoteX ty quoteXs)
{-# SCC quoteCase #-}

