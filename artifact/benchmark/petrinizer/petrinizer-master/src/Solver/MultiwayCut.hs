module Solver.MultiwayCut
    (checkMultiwayCutSat)
where

import Data.SBV
import qualified Data.Map as M
import qualified Data.Set as S

import Util
import Solver
import PetriNet

checkConnection :: PetriNet -> S.Set Place ->
        SIMap Place -> SIMap Transition -> Place -> SBool
checkConnection net ps m x p =
            sAnd $ map checkTransition $ post net p
        where checkTransition t =
                let p' = S.elemAt 0 $ S.fromList (post net t) `S.intersection` ps
                in  val x t .> 0 .=> val m p .== val m p'

checkConnections :: PetriNet -> S.Set Place ->
        SIMap Place -> SIMap Transition -> SBool
checkConnections net ps m x =
        sAnd $ map (checkConnection net ps m x) $ S.toList ps

checkSizeLimit :: SIMap Transition -> Maybe Integer -> SBool
checkSizeLimit _ Nothing = true
checkSizeLimit x (Just size) = (.< literal size) $ sumVal x

checkBinary :: SIMap Transition -> SBool
checkBinary = sAnd . map (\x -> x .== 0 .|| x .== 1) . vals

checkNonNegativityConstraints :: SIMap Place -> SBool
checkNonNegativityConstraints = sAnd . map (.>= 0) . vals

checkComponents :: [(Place, Integer)] -> SIMap Place -> SBool
checkComponents componentMap m = sAnd $ map checkComponent componentMap
        where checkComponent (p, n) = val m p .== literal n

checkMultiwayCut :: PetriNet -> [(Place, Integer)] -> S.Set Place ->
        SIMap Place -> SIMap Transition -> Maybe Integer -> SBool
checkMultiwayCut net componentMap ps m x sizeLimit =
        checkConnections net ps m x .&&
        checkComponents componentMap m .&&
        checkBinary x .&&
        checkNonNegativityConstraints m .&&
        checkSizeLimit x sizeLimit

getMultiwayCut :: [[Place]] -> [Place] -> [Transition] -> IMap Transition -> ([Transition], Integer)
getMultiwayCut componentMap ps ts x =
            let tCut = M.keys (M.filter (> 0) x)
                cutSize = length tCut
            in  (tCut, cutSize)

makeComponentMap :: (Ord a) => [[a]] -> [(a, Integer)]
makeComponentMap ps = concat $ zipWith zip ps $ map repeat [1..]

checkMultiwayCutSat :: PetriNet -> [[Place]] -> [Place] -> [Transition] -> Maybe Integer ->
        ConstraintProblem Integer ([Transition], Integer)
checkMultiwayCutSat net placeComponents ps ts sizeLimit =
        let m = makeVarMap ps
            x = makeVarMap ts
            componentMap = makeComponentMap placeComponents
            placeSet = S.fromList ps
        in  ("multiway cut", "multiway cut",
            getNames m ++ getNames x,
            \fm -> checkMultiwayCut net componentMap placeSet
                    (fmap fm m) (fmap fm x) sizeLimit,
            \fm -> getMultiwayCut componentMap ps ts (fmap fm x))

