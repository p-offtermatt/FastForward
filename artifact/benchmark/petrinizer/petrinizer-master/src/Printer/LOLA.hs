{-# LANGUAGE OverloadedStrings #-}

module Printer.LOLA
    (printNet,printProperty)
where

import qualified Data.ByteString.Lazy as L
import Data.ByteString.Builder
import Data.Monoid

import Printer
import PetriNet
import Property

renderNet :: PetriNet -> Builder
renderNet net =
        let showWeight (p,x) = renderPlace p <> ":" <> integerDec x
            ps = "PLACE " <> intercalate ","
                    (map renderPlace (places net)) <> ";\n"
            is = "MARKING " <> intercalate ","
                    (map showWeight (linitials net)) <> ";\n"
            makeTransition t =
                let (preT,postT) = context net t
                    preS = "CONSUME " <> intercalate ","
                                (map showWeight preT) <> ";\n"
                    postS = "PRODUCE " <> intercalate ","
                                (map showWeight postT) <> ";\n"
                in  "TRANSITION " <> renderTransition t <> "\n" <> preS <> postS
            ts = map makeTransition (transitions net)
        in  intercalate "\n" (ps:is:ts)

printNet :: PetriNet -> L.ByteString
printNet = toLazyByteString . renderNet

renderTerm :: (Show a) => Term a -> Builder
renderTerm (Var x) = stringUtf8 $ show x
renderTerm (Const c) = integerDec c
renderTerm (Minus t) = "-" <> renderTerm t
renderTerm (t :+: u) = "(" <> renderTerm t <> " + " <> renderTerm u <> ")"
renderTerm (t :-: u) = "(" <> renderTerm t <> " - " <> renderTerm u <> ")"
renderTerm (t :*: u) = renderTerm t <> " * " <> renderTerm u

renderOp :: Op -> Builder
renderOp Gt = " > "
renderOp Ge = " >= "
renderOp Eq = " = "
renderOp Ne = " != "
renderOp Le = " <= "
renderOp Lt = " < "

-- TODO: reduce parantheses in built formula
renderFormula :: (Show a) => Formula a -> Builder
renderFormula FTrue = "TRUE"
renderFormula FFalse = "FALSE"
renderFormula (LinearInequation lhs op rhs) =
        renderTerm lhs <> renderOp op <> renderTerm rhs
renderFormula (Neg p) = "NOT " <> "(" <> renderFormula p <> ")"
renderFormula (p :&: q) = renderFormula p <> " AND " <> renderFormula q
renderFormula (p :|: q) = "(" <> renderFormula p <> " OR " <> renderFormula q <> ")"

renderProperty :: Property -> Builder
renderProperty (Property _ (Safety f)) = "EF (" <> renderFormula f <> ")\n"
renderProperty (Property _ (Liveness _)) =
        error "liveness property not supported for lola"
renderProperty (Property _ (Structural _)) =
        error "structural property not supported for lola"

printProperty :: Property -> L.ByteString
printProperty = toLazyByteString . renderProperty
