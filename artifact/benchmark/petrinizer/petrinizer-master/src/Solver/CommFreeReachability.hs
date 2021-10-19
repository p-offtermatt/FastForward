module Solver.CommFreeReachability
    (checkCommFreeReachability,checkCommFreeReachabilitySat)
where

import Data.SBV

import PetriNet
import Property
import Solver
import Solver.StateEquation

checkSubnet :: PetriNet -> ModelSI -> SBool
checkSubnet net m =
            sAnd $ map checkPrePost $ transitions net
        where checkPrePost t =
                  let preCond = sAnd $ map checkNonNegativity $ pre net t
                      postCond = sAnd $ map checkNonNegativity $ post net t
                  in  mVal m t .> 0 .=> preCond .&& postCond
              checkNonNegativity p = mVal m (prime p) .>= 0

checkRoots :: PetriNet -> ModelSI -> SBool
checkRoots net m =
            sAnd $ map checkRoot $ places net
        where checkRoot p =
                  mVal m (prime p) .== 0 .=> fromBool (initial net p > 0)

checkNodes :: PetriNet -> ModelSI -> SBool
checkNodes net m =
            sAnd $ map checkNode $ places net
        where checkNode p =
                      mVal m (prime p) .> 0 .=> sOr (map checkPreCond (pre net p))
                  where checkPreCond t =
                           mVal m (prime p) .==
                              mVal m (prime (head (pre net t))) + 1 .&&
                           mVal m t .> 0

checkMarkableSubnet :: PetriNet -> ModelSI -> SBool
checkMarkableSubnet net m =
        checkSubnet net m .&&
        checkRoots net m .&&
        checkNodes net m

checkCommFreeReachability :: PetriNet -> Formula -> ModelSI -> SBool
checkCommFreeReachability net f m =
        checkStateEquation net f [] m .&&
        checkMarkableSubnet net m

checkCommFreeReachabilitySat :: PetriNet -> Formula ->
        ([String], ModelSI -> SBool)
checkCommFreeReachabilitySat net f =
        (places net ++ transitions net ++ map prime (places net),
         checkCommFreeReachability net f)
