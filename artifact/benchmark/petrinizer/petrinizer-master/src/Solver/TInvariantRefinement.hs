module Solver.TInvariantRefinement
    (checkSComponent,checkSComponentSat,
     getSComponentOutIn,
     getSComponentCompsCut,
     SCompCut)
where

import Data.SBV
import Data.List (partition)

import PetriNet
import Solver

checkEmptyPlaces :: PetriNet -> [String] -> ModelI -> ModelSI -> SBool
checkEmptyPlaces net fired ax m =
        checkPrePostPlaces net m .&&
        checkPrePostTransitions net m .&&
        checkSubsetTransitions fired m .&&
        checkNotEmpty fired m .&&
        checkClosed net ax m .&&
        checkTokens net m .&&
        checkBinary m

checkEmptyPlacesSat :: PetriNet -> [String] -> [String] -> ModelI ->
        ([String], ModelSI -> SBool)
checkEmptyPlacesSat net ts' ps' ax =
        (ps', checkEmptyPlaces net ts' ps' ax)

--getSComponentOutIn :: PetriNet -> ModelI -> ModelI -> ([String], [String])
--getSComponentOutIn net ax as =
--        partition (cElem ax) $ filter (cElem as) (transitions net)

-- TODO: use strongly connected components and min cuts
--getSComponentCompsCut :: PetriNet -> ModelI -> ModelI -> SCompCut
--getSComponentCompsCut net ax as =
--        let (t, u) = partition (cElem ax) $ filter (cElem as) (transitions net)
--            (t1, t2) = partition (cElem as . prime) t
--        in  [(t1, True), (t2, True), (u, False)]

subnetPlaces :: PetriNet -> [String] -> [String]
subnetPlaces net ts' = filter checkPlace (places net)
        where checkPlace p = any (`elem` ts') (pre net p ++ post net p)

