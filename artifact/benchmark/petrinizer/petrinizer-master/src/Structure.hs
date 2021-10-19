module Structure
    (Structure(..),
     checkStructure,
     checkParallelT,
     checkCommunicationFree)
where

import PetriNet
import Data.List (intersect,sort)

-- TODO: use formulas instead of hard-coded properties
data Structure = FreeChoice | Parallel | FinalPlace | CommunicationFree

instance Show Structure where
        show FreeChoice = "free choice"
        show Parallel = "parallel"
        show FinalPlace = "final place"
        show CommunicationFree = "communication free"

checkStructure :: PetriNet -> Structure -> Bool
checkStructure net FreeChoice =
            all freeChoiceCond [(p,s) | p <- places net, s <- places net]
        where freeChoiceCond (p,s) =
                  let ppost = sort $ post net p
                      spost = sort $ post net s
                  in null (ppost `intersect` spost) || ppost == spost
checkStructure net Parallel =
        any (checkParallelT net) (transitions net)
checkStructure net FinalPlace =
            length (filter finalPlace (places net)) == 1
        where finalPlace p = null (post net p) &&
                  all (\t -> length (post net t) == 1) (pre net p)
checkStructure net CommunicationFree = checkCommunicationFree net

checkCommunicationFree :: PetriNet -> Bool
checkCommunicationFree net =
            all checkTransition (transitions net) &&
            all checkWeights (transitions net)
        where checkTransition t = length (pre net t) == 1
              checkWeights t = all checkWeight (lpre net t)
              checkWeight (_,w) = w <= 1

checkParallelT :: PetriNet -> Transition -> Bool
checkParallelT net t =
                  any postComp [(p,s) | p <- post net t, s <- post net t]
        where postComp (p,s) =
                  let ppost = sort $ post net p
                      spost = sort $ post net s
                  in  p /= s &&
                      not (null ppost) && not (null spost) && ppost /= spost &&
                      any (\u -> length (pre net u) == 1) ppost &&
                      any (\u -> length (pre net u) == 1) spost

