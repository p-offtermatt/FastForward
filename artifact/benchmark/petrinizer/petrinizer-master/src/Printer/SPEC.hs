{-# LANGUAGE OverloadedStrings #-}

module Printer.SPEC
    (printProperty)
where

import qualified Data.ByteString.Lazy as L
import Data.ByteString.Builder
import Data.Monoid

import Property

renderOp :: Op -> Builder
renderOp Ge = ">="
renderOp op = error $ "operand not supported for spec: " <> show op

renderConjunction :: (Show a) => Formula a -> Builder
renderConjunction (LinearInequation (Var x) op (Const c)) =
        stringUtf8 (show x) <> renderOp op <> integerDec c
renderConjunction f@(LinearInequation {}) =
        error $ "linear inequation not supported for spec: " <> show f
renderConjunction (Neg _) = error "negation not supported for spec"
renderConjunction (FTrue :&: p) = renderConjunction p
renderConjunction (p :&: FTrue) = renderConjunction p
renderConjunction (p :&: q) = renderConjunction p <> ", " <> renderConjunction q
renderConjunction f = error $ "formula not supported for spec: " <> show f

renderDisjunction :: (Show a) => Formula a -> Builder
renderDisjunction (FFalse :|: p) = renderDisjunction p
renderDisjunction (p :|: FFalse) = renderDisjunction p
renderDisjunction (p :|: q) = renderDisjunction p <> "\n" <> renderDisjunction q
renderDisjunction f = renderConjunction f

renderFormula :: (Show a) => Formula a -> Builder
renderFormula = renderDisjunction

renderProperty :: Property -> Builder
renderProperty (Property _ (Safety f)) = renderFormula f
renderProperty (Property _ (Liveness _)) =
        error "liveness property not supported for spec"
renderProperty (Property _ (Structural _)) =
        error "structural property not supported for spec"

printProperty :: Property -> L.ByteString
printProperty prop =
        toLazyByteString $ renderProperty prop

