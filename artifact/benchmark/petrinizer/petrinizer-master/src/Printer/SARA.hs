{-# LANGUAGE OverloadedStrings #-}

module Printer.SARA
    (printProperties)
where

import qualified Data.ByteString.Lazy as L
import Data.ByteString.Builder
import Data.Monoid

import Printer
import PetriNet
import Property

renderSimpleTerm :: Integer -> Term Place -> Builder
renderSimpleTerm fac (Var x) = if fac == 1 then renderPlace x
                               else integerDec fac <> renderPlace x
renderSimpleTerm fac (Const c) = integerDec (fac*c)
renderSimpleTerm fac (Const c :*: t) = renderSimpleTerm (fac*c) t
renderSimpleTerm fac (t :*: Const c) = renderSimpleTerm (fac*c) t
renderSimpleTerm fac (Minus t) = renderSimpleTerm (-fac) t
renderSimpleTerm _ t = error $ "term not supported for sara: " <> show t

renderTerm :: Term Place -> Builder
renderTerm (t :+: u) = renderTerm t <> "+" <> renderSimpleTerm 1 u
renderTerm (t :-: u) = renderTerm t <> "+" <> renderSimpleTerm (-1) u
renderTerm t = renderSimpleTerm 1 t

renderOp :: Op -> Builder
renderOp Ge = ">"
renderOp Eq = ":"
renderOp Le = "<"
renderOp op = error $ "operand not supported for sara: " <> show op

renderConjunction :: Formula Place -> Builder
renderConjunction (LinearInequation lhs op (Const c)) =
        renderTerm lhs <> renderOp op <> integerDec c
renderConjunction f@(LinearInequation {}) =
        error $ "linear inequation not supported for sara: " <> show f
renderConjunction (Neg _) = error "negation not supported for sara"
renderConjunction (FTrue :&: p) = renderConjunction p
renderConjunction (p :&: FTrue) = renderConjunction p
renderConjunction (p :&: q) = renderConjunction p <> ", " <> renderConjunction q
renderConjunction f = error $ "formula not supported for sara: " <> show f

renderDisjunction :: String -> String -> PetriNet ->
        Formula Place -> Builder
renderDisjunction propname filename net (FFalse :|: p) =
        renderDisjunction propname filename net p
renderDisjunction propname filename net (p :|: FFalse) =
        renderDisjunction propname filename net p
renderDisjunction propname filename net (p :|: q) =
        renderDisjunction propname filename net p <> "\n\n" <>
        renderDisjunction propname filename net q
renderDisjunction propname filename net f =
        "PROBLEM " <> stringUtf8 (validateId propname) <> ":\n" <>
        "GOAL REACHABILITY;\n" <>
        "FILE " <> stringUtf8 (reverse (takeWhile (/='/') (reverse filename)))
            <> " TYPE LOLA;\n" <>
        "INITIAL " <> intercalate ","
            (map (\(p,i) -> renderPlace p <> ":" <> integerDec i) (linitials net))
            <> ";\n" <>
        "FINAL COVER;\n" <>
        "CONSTRAINTS " <> renderConjunction f <> ";"

renderFormula :: String -> String -> PetriNet -> Formula Place -> Builder
renderFormula = renderDisjunction

renderProperty :: String -> PetriNet -> Property -> Builder
renderProperty filename net (Property propname (Safety f)) =
        renderFormula propname filename net f
renderProperty _ _ (Property _ (Liveness _)) =
        error "liveness property not supported for sara"
renderProperty _ _ (Property _ (Structural _)) =
        error "structural property not supported for sara"

printProperties :: String -> PetriNet -> [Property] -> L.ByteString
printProperties filename net props =
        toLazyByteString $ intercalate "\n" $
            map (renderProperty filename net) props
