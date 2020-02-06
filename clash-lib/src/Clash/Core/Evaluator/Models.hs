{-# LANGUAGE OverloadedStrings #-}

module Clash.Core.Evaluator.Models where

import Prelude hiding (lookup, pi)

import Control.Concurrent.Supply (Supply)
import Data.Bifunctor (first)
import Data.Foldable (foldl')

import Clash.Core.DataCon
import Clash.Core.Literal
import Clash.Core.Subst
import Clash.Core.Term
import Clash.Core.TyCon
import Clash.Core.Type
import Clash.Core.Util
import Clash.Core.Var
import Clash.Core.VarEnv
import Clash.Driver.Types

-- TODO This environment should contain any local terms / types needed by 
-- the evaluator, as well as any globals which are known (primitives and
-- previously evaluated terms).
--
-- It should be embeddable back into a substitution, as it is effectively a
-- closure that lambas / tylambas are applied under.
--
data Env = Env
  { envLocals  :: VarEnv Value               -- ^ Local Expressions
  , envTypes   :: VarEnv Type                -- ^ Local Types
  , envGlobals :: VarEnv (Either Term Value) -- ^ Global Expressions
  , envPrims   :: VarEnv (Either Term Value) -- ^ Primitives
  , envTcMap   :: TyConMap
  , envInScope :: InScopeSet
  , envSupply  :: Supply
  }

instance Show Env where
 show e = show (eltsVarEnv (envLocals e), eltsVarEnv (envTypes e))

mkEnv :: VarEnv Term -> BindingMap -> TyConMap -> InScopeSet -> Supply -> Env
mkEnv ts bm = Env emptyVarEnv emptyVarEnv gs ps
 where
  gs = mapVarEnv (\(_, _, _, t) -> Left t) bm
  ps = mapVarEnv Left ts

asSubst :: Env -> Subst
asSubst = error "asSubst"

extendEnv :: IdScope -> Id -> Value -> Env -> Env
extendEnv LocalId  i v e =
  e { envLocals = extendVarEnv i v (envLocals e) }
extendEnv GlobalId i v e =
  e { envGlobals = extendVarEnv i (Right v) (envGlobals e) }

extendEnvTy :: TyVar -> Type -> Env -> Env
extendEnvTy i ty e =
  e { envTypes = extendVarEnv i ty (envTypes e) }

-- Neutral terms cannot be reduced, as they represent things like variables
-- which are unknown, partially applied functions, or case expressions where
-- the scrutinee is not yet given.
--
data Neutral a
  = NeVar   (Var Term)
  | NeApp   (Neutral a) a
  | NeTyApp (Neutral a) Type
  | NeCase  a Type [(Pat, a)]
  deriving (Show)

-- A term which has been normalised to weak head normal form (WHNF). This has
-- no redexes at the head of the term, but subterms may still contain redexes.
--
data Value
  = VNeu    (Neutral Value)
  | VData   DataCon  [Either Value Type]
  | VLit    Literal
  | VPrim   PrimInfo [Either Value Type]
  | VLam    Id Term Env
  | VTyLam  TyVar Term Env
  | VLetrec [(Id, Value)] Value
  | VCast   Value Type Type
  | VTick   Value TickInfo
  deriving (Show)

collectValueTicks :: Value -> (Value, [TickInfo])
collectValueTicks = go []
 where
  go acc (VTick v ti) = go (ti:acc) v
  go acc v = (v, acc)

addTicks :: Value -> [TickInfo] -> Value
addTicks = foldl' VTick

-- | A term which is in head normal form (HNF). This has no redexes, and all
-- partially applied functions in subterms are eta-expanded.
--
data Nf
  = NNeu    (Neutral Nf)
  | NData   DataCon [Either Nf Type]
  | NLit    Literal
  | NPrim   PrimInfo [Either Nf Type]
  | NLam    Id Nf
  | NTyLam  TyVar Nf
  | NLetrec [(Id, Nf)] Nf
  | NCast   Nf Type Type
  | NTick   Nf TickInfo
  deriving (Show)

-- Embedding WHNF and HNF values back into Term.
--
class AsTerm a where
  asTerm :: a -> Term
 
instance (AsTerm a) => AsTerm (Neutral a) where
  asTerm (NeVar v)        = Var v
  asTerm (NeApp x y)      = App (asTerm x) (asTerm y)
  asTerm (NeTyApp x ty)   = TyApp (asTerm x) ty
  asTerm (NeCase x ty xs) = Case (asTerm x) ty (fmap asTerm <$> xs)

instance AsTerm Value where
  asTerm (VNeu n)         = asTerm n
  asTerm (VData dc args)  = mkApps (Data dc) (first asTerm <$> args)
  asTerm (VLit l)         = Literal l
  asTerm (VPrim pi args)  = mkApps (Prim pi) (first asTerm <$> args)
  asTerm (VLam x e env)   = substTm "asTerm.VLam" (asSubst env) (Lam x e)
  asTerm (VTyLam x e env) = substTm "asTerm.VTyLam" (asSubst env) (TyLam x e)
  asTerm (VLetrec bs x)   = Letrec (fmap asTerm <$> bs) (asTerm x)
  asTerm (VCast x a b)    = Cast (asTerm x) a b
  asTerm (VTick x ti)     = Tick ti (asTerm x)

instance AsTerm Nf where
  asTerm (NNeu n)         = asTerm n
  asTerm (NData dc args)  = mkApps (Data dc) (first asTerm <$> args)
  asTerm (NLit l)         = Literal l
  asTerm (NPrim pi args)  = mkApps (Prim pi) (first asTerm <$> args)
  asTerm (NLam x e)       = Lam x (asTerm e)
  asTerm (NTyLam x e)     = TyLam x (asTerm e)
  asTerm (NLetrec bs x)   = Letrec (fmap asTerm <$> bs) (asTerm x)
  asTerm (NCast x a b)    = Cast (asTerm x) a b
  asTerm (NTick x ti)     = Tick ti (asTerm x)

