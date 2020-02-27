{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ViewPatterns          #-}

module Clash.Primitives.Verification (checkBBF) where

import Data.Either


import qualified Control.Lens                    as Lens
import           Control.Monad.State             (State)
import           Data.Text.Prettyprint.Doc.Extra (Doc)
import qualified Data.Text                       as Text
import           Data.Semigroup.Monad            (getMon)
import           GHC.Stack                       (HasCallStack)

import           Clash.Annotations.Primitive     (HDL(..))
import qualified Clash.Backend
import           Clash.Backend
  (Backend, blockDecl, hdlKind)
import           Clash.Core.Term                 (Term(Var))
import           Clash.Core.TermLiteral
  (uncheckedTermToData, termToDataError)
import           Clash.Core.Util                 (termType)
import           Clash.Util                      (indexNote)
import           Clash.Netlist                   (mkExpr)
import           Clash.Netlist.Util              (stripVoid)
import qualified Clash.Netlist.Util
import           Clash.Netlist.Util              (id2identifier)
import           Clash.Netlist.Id                (IdType(Basic))
import           Clash.Netlist.Types
  (BlackBox(BBFunction), TemplateFunction(..), BlackBoxContext, Identifier,
   NetlistMonad, Declaration(Assignment, NetDecl', TickDecl),
   HWType(Bool, KnownDomain), WireOrReg(Wire), NetlistId(..),
   DeclarationType(Concurrent), tcCache, bbInputs)
import           Clash.Netlist.BlackBox.Types
  (BlackBoxFunction, BlackBoxMeta(..), TemplateKind(TDecl), RenderVoid(..),
   emptyBlackBoxMeta)

import           Clash.Verification.Internal
import           Clash.Verification.PrettyPrinters

checkBBF :: BlackBoxFunction
checkBBF _isD _primName args _ty =
  case cvProperty of
    Left err -> pure (Left err)
    Right cvExpr0 -> do
      cvExpr1 <- mapM (uncurry bindMaybe) cvExpr0
      let decls = concatMap snd cvExpr1
          cvExpr2 = fmap fst cvExpr1
      pure (Right (meta, bb (checkTF decls clkId propName renderAs cvExpr2)))
 where
  (Var (id2identifier -> clkId)) = indexNote "clk" (lefts args) 1
  (Var (id2identifier -> _clkId)) = indexNote "rst" (lefts args) 2
  propName = uncheckedTermToData (indexNote "propName" (lefts args) 3)
  renderAs = uncheckedTermToData (indexNote "renderAs" (lefts args) 4)
  cvProperty = termToDataError (indexNote "propArg" (lefts args) 5)

  bb = BBFunction "Clash.Primitives.Verification.checkTF" 0
  meta = emptyBlackBoxMeta {bbKind=TDecl, bbRenderVoid=RenderVoid}
  mkId = Clash.Netlist.Util.mkUniqueIdentifier Basic . Text.pack

  bindMaybe
    :: Maybe String
    -- ^ Hint for new identifier
    -> Term
    -- ^ Term to bind. Does not bind if it's already a reference to a signal
    -> NetlistMonad (Identifier, [Declaration])
    -- ^ ([new] reference to signal, [declarations need to get it in scope])
  bindMaybe _ (Var vId) = pure (id2identifier vId, [])
  bindMaybe Nothing t = bindMaybe (Just "s") t
  bindMaybe (Just nm) t = do
    tcm <- Lens.use tcCache
    newId <- mkId nm
    (expr0, decls) <- mkExpr False Concurrent (NetlistId newId (termType tcm t)) t
    pure
      ( newId
      , decls ++ [sigDecl Bool newId, Assignment newId expr0] )

  -- Simple wire without comment
  sigDecl :: HWType -> Identifier -> Declaration
  sigDecl typ nm = NetDecl' Nothing Wire nm (Right typ) Nothing

checkTF
  :: [Declaration]
  -> Identifier
  -> Text.Text
  -> RenderAs
  -> Property' Identifier
  -> TemplateFunction
checkTF decls clkId propName renderAs prop =
  TemplateFunction [] (const True) (checkTF' decls clkId propName renderAs prop)

checkTF'
  :: forall s
   . (HasCallStack, Backend s)
  => [Declaration]
  -- ^ Extra decls needed
  -> Identifier
  -- ^ Clock
  -> Text.Text
  -- ^ Prop name
  -> RenderAs
  -> Property' Identifier
  -> BlackBoxContext
  -> State s Doc
checkTF' decls clkId propName renderAs prop bbCtx = do
  blockName <- Clash.Backend.mkUniqueIdentifier Basic (propName <> "_block")
  getMon (blockDecl blockName (TickDecl renderedPslProperty : decls))

 where
  hdl = hdlKind (undefined :: s)

  edge =
    case bbInputs bbCtx !! 0 of
      (_, stripVoid -> KnownDomain _nm _period e _rst _init _polarity, _) -> e
      _ -> error $ "Unexpected first argument: " ++ show (bbInputs bbCtx !! 0)

  renderedPslProperty =
    let
      psl = pprPslProperty hdl propName clkId edge prop
      sva = pprSvaProperty propName clkId edge prop
    in
      case renderAs of
        PSL -> psl
        SVA -> sva
        AutoRenderAs ->
          case hdl of
            SystemVerilog -> sva
            Verilog -> psl
            VHDL -> psl
