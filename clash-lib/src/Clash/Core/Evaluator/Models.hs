{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Clash.Core.Evaluator.Models where

import Prelude hiding (lookup, pi)

import Control.Concurrent.Supply (Supply)
import Control.DeepSeq (NFData(..))
import Data.Bifunctor (first, second)
import Data.Foldable (foldl')
import GHC.Generics (Generic)

import BasicTypes (InlineSpec(..))

import Clash.Core.DataCon
import Clash.Core.Evaluator.Delay (Delay)
import Clash.Core.Literal
import Clash.Core.Subst
import Clash.Core.Term
import Clash.Core.TyCon
import Clash.Core.Type
import Clash.Core.Util
import Clash.Core.Var
import Clash.Core.VarEnv
import Clash.Driver.Types

type EvalPrim = Env -> PrimInfo -> [Either Value Type] -> Delay (Maybe Value)

-- TODO This environment should contain any local terms / types needed by 
-- the evaluator, as well as any globals which are known (primitives and
-- previously evaluated terms).
--
-- It should be embeddable back into a substitution, as it is effectively a
-- closure that lambas / tylambas are applied under.
--
data Env = Env
  { envPrimEval :: EvalPrim
    -- ^ Primitive Evaluation Function
  , envTypes    :: VarEnv Type
    -- ^ Local Types
  , envLocals   :: VarEnv (Either Term Value)
    -- ^ Local Expressions
  , envGlobals  :: VarEnv (InlineSpec, Either Term Value)
    -- ^ Global Expressions
  , envPrims    :: VarEnv (Either Term Value)
    -- ^ Primitive Expressions
  , envTcMap    :: TyConMap
  , envInScope  :: InScopeSet
  , envSupply   :: Supply
  }

instance Show Env where
 show e = show (eltsVarEnv (envLocals e), eltsVarEnv (envTypes e))

instance NFData Env where
  rnf (Env _ ls ts gs ps tcm _ _) =
    rnf ls `seq` rnf ts `seq` rnf gs `seq` rnf ps `seq` rnf tcm

mkEnv
  :: EvalPrim
  -> VarEnv Term
  -> BindingMap
  -> TyConMap
  -> InScopeSet
  -> Supply
  -> Env
mkEnv eval ts bm = Env eval emptyVarEnv emptyVarEnv gs ps
 where
  gs = mapVarEnv (\b -> (bindingSpec b, Left $ bindingTerm b)) bm
  ps = mapVarEnv Left ts

asSubst :: Env -> Subst
asSubst = error "asSubst"

extendEnv :: IdScope -> Id -> Either Term Value -> Env -> Env
extendEnv LocalId i v e =
  e { envLocals = extendVarEnv i v (envLocals e) }
extendEnv GlobalId i v e =
  e { envGlobals = extendVarEnv i (Inline, v) (envGlobals e) }

extendEnvTy :: TyVar -> Type -> Env -> Env
extendEnvTy i ty e =
  e { envTypes = extendVarEnv i ty (envTypes e) }

deleteEnv :: IdScope -> Id -> Env -> Env
deleteEnv LocalId i e =
  e { envLocals = delVarEnv (envLocals e) i }
deleteEnv GlobalId i e =
  e { envGlobals = delVarEnv (envGlobals e) i }

-- Neutral terms cannot be reduced, as they represent things like variables
-- which are unknown, partially applied functions, or case expressions where
-- the scrutinee is not yet an inspectable value. Consider:
--
-- v              Stuck if "v" is a free variable
-- x $ y          Stuck if "x" is not known to be a lambda
-- x @ A          Stuck if "x" is not known to be a type lambda
-- case x of ...  Stuck if "x" is neutral (cannot choose an alternative)
--
data Neutral a
  = NeVar   (Var Term)
  | NeApp   (Neutral a) a
  | NeTyApp (Neutral a) Type
  | NeCase  a Type [(Pat, a)]
  deriving (Show, Generic, NFData)

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
  | VCast   Value Type Type
  | VTick   Value TickInfo
  deriving (Show, Generic, NFData)

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
  | NCast   Nf Type Type
  | NTick   Nf TickInfo
  deriving (Show, Generic, NFData)

-- Embedding WHNF and HNF values back into Term.
--
class AsTerm a where
  asTerm :: a -> Term
 
instance (AsTerm a) => AsTerm (Neutral a) where
  asTerm (NeVar v)        = Var v
  asTerm (NeApp x y)      = App (asTerm x) (asTerm y)
  asTerm (NeTyApp x ty)   = TyApp (asTerm x) ty
  asTerm (NeCase x ty as) = Case (asTerm x) ty (second asTerm <$> as)

instance AsTerm Value where
  asTerm (VNeu n)         = asTerm n
  asTerm (VData dc args)  = mkApps (Data dc) (first asTerm <$> args)
  asTerm (VLit l)         = Literal l
  asTerm (VPrim pi args)  = mkApps (Prim pi) (first asTerm <$> args)
  asTerm (VLam x e env)   = substTm "asTerm.VLam" (asSubst env) (Lam x e)
  asTerm (VTyLam x e env) = substTm "asTerm.VTyLam" (asSubst env) (TyLam x e)
  asTerm (VCast x a b)    = Cast (asTerm x) a b
  asTerm (VTick x ti)     = Tick ti (asTerm x)

instance AsTerm Nf where
  asTerm (NNeu n)         = asTerm n
  asTerm (NData dc args)  = mkApps (Data dc) (first asTerm <$> args)
  asTerm (NLit l)         = Literal l
  asTerm (NPrim pi args)  = mkApps (Prim pi) (first asTerm <$> args)
  asTerm (NLam x e)       = Lam x (asTerm e)
  asTerm (NTyLam x e)     = TyLam x (asTerm e)
  asTerm (NCast x a b)    = Cast (asTerm x) a b
  asTerm (NTick x ti)     = Tick ti (asTerm x)

