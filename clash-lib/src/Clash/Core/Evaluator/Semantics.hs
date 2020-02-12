{-# LANGUAGE LambdaCase #-}

module Clash.Core.Evaluator.Semantics
  ( partialEval
  , evaluate
  , quote
  ) where

import Prelude hiding (lookup, pi)

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

-- TODO: In practice this may not terminate. If this ends up being the
-- case we should impose a limit on how many times we are acceptable with
-- being delayed - returning the original subterm if we exceed this.
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

-- TODO Currently, a globally bound term which is not WHNF is re-evalauted
-- every time it is looked up in the environment. We should keep this result
-- so we only evaluate each global once.
--
-- Does this mean changing Delay to StateT Env Delay ?
--
evaluate :: Env -> Term -> Delay Value
evaluate env = \case
  Var v -> lookup v env
  Data dc -> return (VData dc [])
  Literal l -> return (VLit l)
  Prim pi -> return (VPrim pi [])
  Lam x e -> return (VLam x e env)
  TyLam x e -> return (VTyLam x e env)
  App x y -> evaluateApp env x y
  TyApp x ty -> evaluateTyApp env x ty
  Letrec bs e -> evaluateLetrec env bs e
  Case e _ xs -> evaluateCase env e xs
  Cast x a b -> evaluateCast env x a b
  Tick ti x -> evaluateTick env x ti

lookup :: Id -> Env -> Delay Value
lookup i e
  | Just etv <- lookupVarEnv i (envLocals e)
  = go etv

  | Just (s, etv) <- lookupVarEnv i (envGlobals e)
  , s == Inline || s == Inlinable
  = go etv

  | otherwise
  = return (VNeu (NeVar i))
 where
  go = either (evaluate e) return

evaluateApp :: Env -> Term -> Term -> Delay Value
evaluateApp env x y = do
  evalX <- evaluate env x
  evalY <- evaluate env y

  case evalX of
    VData dc args -> dataApp dc args evalY
    VPrim pi args -> primApp pi args evalY
    _ -> apply evalX evalY
 where
  dataApp dc args v
    | tys <- fst $ splitFunForallTy (dcType dc)
    , length tys == length args
    = error "evaluateApp.dataApp: Overapplied DC"

    | otherwise
    = return (VData dc (args <> [Left v]))

  primApp pi args v
    | length tys == length args
    = error "evaluateApp.primApp: Overapplied prim"

    | length tys == length args + 1
    = envPrimEval env env pi (args <> [Left v]) >>= \case
        Just r -> return r
        Nothing -> error ("evaluateApp: Could not evaluate prim " <> show (primName pi))

    | otherwise
    = return (VPrim pi (args <> [Left v])) 
   where
    tys = fst $ splitFunForallTy (primType pi)

evaluateTyApp :: Env -> Term -> Type -> Delay Value
evaluateTyApp env x ty = do
  evalX <- evaluate env x

  case evalX of
    VData dc args -> dataTyApp dc args
    VPrim pi args -> primTyApp pi args
    _ -> applyTy evalX ty
 where
  dataTyApp dc args
    | tys <- fst $ splitFunForallTy (dcType dc)
    , length tys == length args
    = error "evaluateTyApp.dataTyApp: Overapplied DC"

    | otherwise
    = return (VData dc (args <> [Right ty]))

  primTyApp pi args
    | length tys == length args
    = error "evaluateTyApp.primTyApp: Overapplied prim"

    | length tys == length args + 1
    = envPrimEval env env pi (args <> [Right ty]) >>= \case
        Just r -> return r
        Nothing -> error ("evaluateTyApp: Could not evaluate prim " <> show (primName pi))

    | otherwise
    = return (VPrim pi (args <> [Right ty]))
   where
    tys = fst $ splitFunForallTy (primType pi)

evaluateLetrec :: Env -> [LetBinding] -> Term -> Delay Value
evaluateLetrec env bs x = do
  let evalBs = fmap Left <$> bs
  let env' = foldr (uncurry $ extendEnv LocalId) env evalBs

  evaluate env' x

evaluateCase :: Env -> Term -> [Alt] -> Delay Value
evaluateCase env x xs = do
  evalX <- evaluate env x

  case evalX of
    VLit l -> litCase l
    VData dc args -> dataCase dc args
    VPrim pi args -> primCase pi args
    v -> error ("evaluateCase: Cannot scrutinise " <> show v)
 where
  litCase l = undefined

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

  primCase _ args =
    let eval (pat, e) = case pat of
          DataPat _ tvs ids -> evalDataPat args tvs ids e
          LitPat _ -> error "primCase.LitPat"
          DefaultPat -> evaluate env e

     in evalAlts env (const True) eval xs

evalAlts :: Env -> (Pat -> Bool) -> (Alt -> Delay Value) -> [Alt] -> Delay Value
evalAlts env isMatch eval alts =
  case filter (isMatch . fst) alts of
    [] -> error "evalAlts: No matching alternatives for case"
    (x:xs) -> eval (bestMatch x xs)
 where
  bestMatch (DefaultPat, e) [] = (DefaultPat, e)
  bestMatch (DefaultPat, _) xs = head xs
  bestMatch alt _ = alt

evaluateCast :: Env -> Term -> Type -> Type -> Delay Value
evaluateCast env x a b = do
  evalX <- evaluate env x
  return (VCast evalX a b)

evaluateTick :: Env -> Term -> TickInfo -> Delay Value
evaluateTick env x ti = do
  evalX <- evaluate env x
  return (VTick evalX ti)

apply :: Value -> Value -> Delay Value
apply (collectValueTicks -> (v1, ts)) v2 =
  case v1 of
    VNeu n -> return (addTicks (VNeu (NeApp n v2)) ts)

    VLam x e env ->
      let val = evaluate (extendEnv LocalId x (Right v2) env) e
       in delay (fmap (`addTicks` ts) val)

    _ -> error ("apply: Cannot apply value to " <> show v1)

applyTy :: Value -> Type -> Delay Value
applyTy (collectValueTicks -> (v, ts)) ty =
  case v of
    VNeu n ->
      return (addTicks (VNeu (NeTyApp n ty)) ts)

    VTyLam x e env ->
      let val = evaluate (extendEnvTy x ty env) e
       in delay (fmap (`addTicks` ts) val)

    _ -> error ("applyTy: Cannot apply type to " <> show v)

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

quoteData :: DataCon -> [Either Value Type] -> Delay Nf
quoteData dc args = do
  quoteArgs <- traverse (bitraverse quote return) args

  return (NData dc quoteArgs)

quotePrim :: PrimInfo -> [Either Value Type] -> Delay Nf
quotePrim pi args = do
  quoteArgs <- traverse (bitraverse quote return) args

  return (NPrim pi quoteArgs)

quoteLam :: Id -> Term -> Env -> Delay Nf
quoteLam x e env =
  fmap (NLam x) . delay $ apply (VLam x e env) (VNeu (NeVar x)) >>= quote

quoteTyLam :: TyVar -> Term -> Env -> Delay Nf
quoteTyLam x e env =
  fmap (NTyLam x) . delay $ applyTy (VTyLam x e env) (VarTy x) >>= quote

quoteCast :: Value -> Type -> Type -> Delay Nf
quoteCast x a b = do
  quoteX <- quote x

  return (NCast quoteX a b)

quoteTick :: Value -> TickInfo -> Delay Nf
quoteTick x ti = do
  quoteX <- quote x

  return (NTick quoteX ti)

quoteNeutral :: Neutral Value -> Delay (Neutral Nf)
quoteNeutral = \case
  NeVar v -> return (NeVar v)
  NeApp x y -> quoteApp x y
  NeTyApp x ty -> quoteTyApp x ty

quoteApp :: Neutral Value -> Value -> Delay (Neutral Nf)
quoteApp x y = do
  quoteX <- quoteNeutral x
  quoteY <- quote y

  return (NeApp quoteX quoteY)

quoteTyApp :: Neutral Value -> Type -> Delay (Neutral Nf)
quoteTyApp x ty = do
  quoteX <- quoteNeutral x

  return (NeTyApp quoteX ty)

