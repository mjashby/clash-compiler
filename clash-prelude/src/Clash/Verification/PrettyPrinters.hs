{-|
Copyright  :  (C) 2019, Myrtle Software Ltd
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Verification
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Clash.Verification.PrettyPrinters
  ( pprPslProperty
  , pprSvaProperty

  -- * Debugging functions
  , pprProperty
  ) where

import           Clash.Annotations.Primitive      (HDL(..))
import           Clash.Signal.Internal            (ActiveEdge, ActiveEdge(..))
import           Clash.Verification.Internal
import           Data.Maybe                       (fromMaybe)
import           Data.Text                        (Text)
import           TextShow                         (showt)

data Symbol
  = TImpliesOverlapping
  | TImplies
  | Implies
  | BiImplies
  | Not
  | And
  | Or
  | To
  | Equals
  -- + [] ?
  | Assign
  | Is

------------------------------------------
--                 UTIL                 --
------------------------------------------
-- | Collapse constructs such as `next (next a)` down to `next[2] a`
squashBefore :: ModalAssertion' a -> [ModalAssertion' a]
squashBefore (CvBefore e1 e2) = e1s ++ e2s
 where
  e1s = case squashBefore e1 of {[] -> [e1]; es -> es}
  e2s = case squashBefore e2 of {[] -> [e2]; es -> es}
squashBefore _ = []

parensIf :: Bool -> Text -> Text
parensIf True s = "(" <> s <> ")"
parensIf False s = s

bracketsIf :: Bool -> Text -> Text
bracketsIf True s = "{" <> s <> "}"
bracketsIf False s = s

---------------------------------------
--                PSL                --
---------------------------------------
pslBasicBinOp
  :: HDL
  -> Bool
  -> Symbol
  -> BasicAssertion' Text
  -> BasicAssertion' Text
  -> Text
pslBasicBinOp hdl parens op e1 e2 =
  parensIf parens (e1' <> symbol hdl op <> e2')
 where
  e1' = pprPslBasicAssertion hdl True e1
  e2' = pprPslBasicAssertion hdl True e2

pslModalBinOp
  :: HDL
  -> Bool
  -> Symbol
  -> ModalAssertion' Text
  -> ModalAssertion' Text
  -> Text
pslModalBinOp hdl parens op e1 e2 =
  parensIf parens (e1' <> symbol hdl op <> e2')
 where
  e1' = pprPslModalAssertion hdl True e1
  e2' = pprPslModalAssertion hdl True e2

pslEdge :: HDL -> ActiveEdge -> Text -> Text
pslEdge SystemVerilog activeEdge clkId = pslEdge Verilog activeEdge clkId
pslEdge Verilog Rising clkId = "posedge " <> clkId
pslEdge Verilog Falling clkId = "negedge " <> clkId
pslEdge VHDL Rising clkId = "rising_edge(" <> clkId <> ")"
pslEdge VHDL Falling clkId = "falling_edge(" <> clkId <> ")"

-- | Taken from IEEE Std 1850-2010a, Annex B.1, p149
symbol :: HDL -> Symbol -> Text
symbol SystemVerilog = symbol Verilog
symbol Verilog = \case
  TImpliesOverlapping -> "|->"
  TImplies  -> "|=>"
  Implies   -> "->"
  BiImplies -> "<->"
  Not       -> "!"
  And       -> "&&"
  Or        -> "||"
  To        -> ":"
  Assign    -> "<="
  Is        -> "="
  Equals    -> "=="

symbol VHDL = \case
  TImpliesOverlapping -> "|->"
  TImplies  -> "|=>"
  Implies   -> " -> "
  BiImplies -> " <-> "
  Not       -> "not"
  And       -> " and "
  Or        -> " or "
  To        -> " to "
  Assign    -> "<="
  Is        -> "is"
  Equals    -> "="

-- | Pretty print Property. Doesn't print valid HDL, but can be used for
-- debugging purposes.
pprProperty :: Property dom -> Text
pprProperty (Property prop0) =
  let prop1 = fromMaybe "__autogen__" . fst <$> prop0 in
  pprPslProperty VHDL "prop" "clk" Rising prop1

pprPslProperty
  :: HDL
  -- ^ HDL to generate PSL expression for
  -> Text
  -- ^ Property name
  -> Text
  -- ^ Clock name
  -> ActiveEdge
  -- ^ Edge property should be sensitive to
  -> Property' Text
  -- ^ Assertion / Cover statement
  -> Text
pprPslProperty hdl propName clkId edge assertion =
  "psl property " <> propName <> " " <> symbol hdl Is <> "\n" <>
  "(" <> prop <> ") @(" <> pslEdge hdl edge clkId <> ")" <>
  ";\n" <> "psl " <> coverOrAssert <> " " <>
  propName <> ";"
 where
  (coverOrAssert, prop) =
    case assertion of
      CvCover e -> ("cover", pprPslModalAssertion hdl False e)
      CvAssert e -> ("assert", pprPslModalAssertion hdl False e)

pprPslModalAssertion :: HDL -> Bool -> ModalAssertion' Text -> Text
pprPslModalAssertion hdl parens e =
  case e of
    (CvTempPure e1) -> bracketsIf parens (pprPslBasicAssertion hdl parens e1)
    (CvNext 0 e1) -> pprPslModalAssertion hdl parens e1
    (CvNext 1 e1) -> " ## " <> pprPslModalAssertion hdl True e1
    (CvNext n e1) -> " ##" <> showt n <> " " <> pprPslModalAssertion hdl False e1

    (CvBefore _ _) -> "{" <> afters1 <> "}"
     where
      afters0 = map (pprPslModalAssertion hdl False) (squashBefore e)
      afters1 = foldl1 (\e1 e2 -> e1 <> "; " <> e2) afters0

    (CvTempImplies 0 e1 e2) -> pslBinOp TImpliesOverlapping e1 e2
    (CvTempImplies 1 e1 e2) -> pslBinOp TImplies e1 e2
    (CvTempImplies n e1 e2) -> pslBinOp TImplies e1 (CvNext n e2)

    (CvAlways e1) -> "always " <> pprPslModalAssertion hdl True e1
    (CvNever e1) -> "never " <> pprPslModalAssertion hdl True e1
 where
  pslBinOp = pslModalBinOp hdl True

pprPslBasicAssertion :: HDL -> Bool -> BasicAssertion' Text -> Text
pprPslBasicAssertion hdl parens e =
  case e of
    (CvPure p) -> p
    -- ModelSim/QuastaSim doesn't support booleans in PSL. Anytime we want to
    -- use a boolean literal we use (0 == 0) or (0 == 1) instead.
    (CvLit False) -> parensIf parens ("0" <> symbol hdl Equals <> "1")
    (CvLit True) -> parensIf parens ("0" <> symbol hdl Equals <> "0")

    (CvNot e1) ->
      parensIf parens (symbol hdl Not <> " " <> pprPslBasicAssertion hdl True e1)
    (CvAnd e1 e2) -> pslBinOp And e1 e2
    (CvOr e1 e2) -> pslBinOp Or e1 e2
    (CvImplies e1 e2) -> pslBinOp Implies e1 e2
 where
  pslBinOp = pslBasicBinOp hdl parens

---------------------------------------
--                SVA                --
---------------------------------------
svaEdge :: ActiveEdge -> Text -> Text
svaEdge Rising clkId = "posedge " <> clkId
svaEdge Falling clkId = "negedge " <> clkId

svaBasicBinOp
  :: Bool
  -> Symbol
  -> BasicAssertion' Text
  -> BasicAssertion' Text
  -> Text
svaBasicBinOp parens op e1 e2 =
  parensIf parens (e1' <> symbol SystemVerilog op <> e2')
 where
  e1' = pprSvaBasicAssertion True e1
  e2' = pprSvaBasicAssertion True e2

svaModalBinOp
  :: Bool
  -> Symbol
  -> ModalAssertion' Text
  -> ModalAssertion' Text
  -> Text
svaModalBinOp parens op e1 e2 =
  parensIf parens (e1' <> symbol SystemVerilog op <> e2')
 where
  e1' = pprSvaModalAssertion True e1
  e2' = pprSvaModalAssertion True e2

pprSvaBasicAssertion :: Bool -> BasicAssertion' Text -> Text
pprSvaBasicAssertion parens e =
  case e of
    (CvPure p) -> p
    (CvLit False) -> "false"
    (CvLit True) -> "true"

    (CvNot e1) ->
      parensIf parens (symbol' Not <> pprSvaBasicAssertion True e1)
    (CvAnd e1 e2) -> svaBinOp And e1 e2
    (CvOr e1 e2) -> svaBinOp Or e1 e2
    (CvImplies e1 e2) -> svaBinOp Implies e1 e2
 where
  svaBinOp = svaBasicBinOp parens
  symbol' = symbol SystemVerilog

pprSvaModalAssertion :: Bool -> ModalAssertion' Text -> Text
pprSvaModalAssertion parens e =
  case e of
    (CvTempPure e1) -> bracketsIf parens (pprSvaBasicAssertion parens e1)
    (CvNext 0 e1) -> pprSvaModalAssertion parens e1
    (CvNext n e1) -> "nexttime[" <> showt n <> "] " <> pprSvaModalAssertion False e1

    (CvBefore _ _) -> "{" <> afters1 <> "}"
     where
      afters0 = map (pprSvaModalAssertion False) (squashBefore e)
      afters1 = foldl1 (\e1 e2 -> "(" <> e1 <> ") ##1 (" <> e2 <> ")") afters0

    (CvTempImplies 0 e1 e2) -> svaBinOp TImpliesOverlapping e1 e2
    (CvTempImplies 1 e1 e2) -> svaBinOp TImplies e1 e2
    (CvTempImplies n e1 e2) -> svaBinOp TImplies e1 (CvNext n e2)

    (CvAlways e1) -> "always (" <> pprSvaModalAssertion False e1 <> ")"
    (CvNever _e) -> error "'never' not supported in SVA"
 where
  svaBinOp = svaModalBinOp parens

pprSvaProperty
  :: Text
  -- ^ Property name
  -> Text
  -- ^ Clock name
  -> ActiveEdge
  -- ^ Edge property should be sensitive to
  -> Property' Text
  -- ^ Assertion / Cover statement
  -> Text
pprSvaProperty propName clkId edge assertion =
  propName <> ": " <> coverOrAssert <> " property (@(" <>
  svaEdge edge clkId <> ") " <> prop <> ");"
 where
  (coverOrAssert, prop) =
    case assertion of
      CvCover e -> ("cover", pprSvaModalAssertion False e)
      CvAssert e -> ("assert", pprSvaModalAssertion False e)
