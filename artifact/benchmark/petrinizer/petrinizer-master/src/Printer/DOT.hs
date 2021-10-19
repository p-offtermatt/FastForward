{-# LANGUAGE OverloadedStrings #-}

module Printer.DOT
    (printNet)
where

import qualified Data.ByteString.Lazy as L
import Data.ByteString.Builder
import Data.Monoid

import Printer
import PetriNet

-- TODO: mark initially labeled places
renderNet :: PetriNet -> Builder
renderNet net =
            "digraph petrinet {\n" <>
            mconcat (map placeLabel (places net)) <>
            mconcat (map transLabel (transitions net)) <>
            "}\n"
        where
            placeLabel p = renderPlace p <>  " [label=\"" <> renderPlace p <>
                (if initial net p > 0 then
                    "(" <> integerDec (initial net p) <> ")"
                 else
                    ""
                ) <> "\"];\n"
            transLabel t = renderTransition t <>
                " [label=\"" <> renderTransition t <> "\", shape=box, " <>
                "style=filled, fillcolor=\"#AAAAAA\"];\n" <>
                mconcat (map (\p -> arcLabel (p,t)) (pre net t)) <>
                mconcat (map (\p -> arcLabel (t,p)) (post net t))
            arcLabel (a,b) = renderShow a <> " -> " <> renderShow b <> "\n"

printNet :: PetriNet -> L.ByteString
printNet = toLazyByteString . renderNet
