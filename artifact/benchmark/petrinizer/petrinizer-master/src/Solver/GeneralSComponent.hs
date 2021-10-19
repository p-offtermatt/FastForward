module Solver.GeneralSComponent
    (checkGeneralSComponentSat)
where

import Data.SBV
import Data.List (genericLength)
import qualified Data.Map as M

import Util
import PetriNet
import Solver

checkPrePostPlaces :: PetriNet -> SIMap Place -> SIMap Transition ->
        SBool
checkPrePostPlaces net p' t' =
            sAnd $ map checkPrePostPlace $ places net
        where checkPrePostPlace p =
                  let incoming = map (positiveVal t') $ pre net p
                      outgoing = map (positiveVal t') $ post net p
                      pVal = positiveVal p' p
                  in  pVal .=> sAnd incoming .&& sAnd outgoing

checkPrePostTransitions :: PetriNet -> SIMap Place -> SIMap Transition ->
        SBool
checkPrePostTransitions net p' t' =
            sAnd $ map checkPrePostTransition $ transitions net
        where checkPrePostTransition t =
                  let incoming = mval p' $ pre net t
                      outgoing = mval p' $ post net t
                      tVal = positiveVal t' t
                  in  tVal .=> sum incoming .== 1 .&& sum outgoing .== 1

checkMinimum :: FiringVector -> SIMap Transition -> SBool
checkMinimum x yt = sOr $ map checkTransition $ elems x
        where checkTransition t = val yt t .== 1

checkMaximum :: FiringVector -> SIMap Transition -> SInteger -> SBool
checkMaximum x yt k = sOr $ map checkTransition $ elems x
        where checkTransition t = val yt t .== k

checkNext :: FiringVector -> SIMap Transition -> SInteger -> SBool
checkNext x yt k = sAnd $ map checkTransition $ elems x
        where checkTransition t = val yt t .< k .=> sOr (map checkNextVal (elems x))
                  where checkNextVal t' = val yt t' .== val yt t + 1

checkSubset :: PetriNet -> SIMap Place -> SIMap Transition ->
        SIMap Place -> SIMap Transition -> SBool
checkSubset net p' t' yp yt =
            sAnd (map (checkSub yp p') (places net)) .&&
            sAnd (map (checkSub yt t') (transitions net))
        where checkSub xs ys x = positiveVal xs x .=> positiveVal ys x

checkClosed :: PetriNet -> SIMap Place -> SIMap Transition ->
        SIMap Place -> SIMap Transition -> SBool
checkClosed net p' t' yp yt =
            sAnd (map checkPlaceClosed (places net)) .&&
            sAnd (map checkTransitionClosed (transitions net))
        where checkPlaceClosed p =
                  let pVal = positiveVal p' p
                      postVal = sAnd $ map checkTransition $ pre net p ++ post net p
                      checkTransition t = positiveVal yt t .=> val yt t .== val yp p
                  in  pVal .=> postVal
              checkTransitionClosed t =
                  let tVal = positiveVal yt t
                      postVal = sAnd $ map checkPlace $ pre net t ++ post net t
                      checkPlace p = positiveVal p' p .=> val yt t .== val yp p
                  in  tVal .=> postVal

checkZeroUnused :: FiringVector -> SIMap Transition -> SIMap Transition -> SBool
checkZeroUnused x t' yt =
            sAnd (map checkTransition (elems x))
        where checkTransition t = val yt t .>= val t' t

checkTokens :: PetriNet -> SIMap Place -> SInteger -> SBool
checkTokens net p' k =
            sum (map addPlace $ linitials net) .< k
        where addPlace (p,i) = literal i * val p' p

checkBinary :: SIMap Place -> SIMap Transition -> SBool
checkBinary p' t' = checkBins p' .&& checkBins t'
        where checkBins xs = sAnd $ map (\x -> x .== 0 .|| x .== 1) $ vals xs

checkCut :: PetriNet -> SIMap Transition -> SIMap Transition -> SIMap Transition -> SBool
checkCut net t' yt cut =
            sAnd $ map checkTransition $ transitions net
        where checkTransition t =
                let pos = val yt t .== 0 .&& val t' t .> 0 .=> val cut t .== 1
                    neg = val yt t .> 0 .|| val t' t .== 0 .=> val cut t .== 0
                in  pos .&& neg

checkNonNegativityConstraints :: SIMap Place -> SIMap Transition -> SBool
checkNonNegativityConstraints yp yt = checkNN yp .&& checkNN yt
        where checkNN xs = sAnd $ map (.>= 0) $ vals xs

checkSizeLimit :: SIMap Place -> SIMap Transition -> SIMap Transition ->
        Maybe (Integer, Integer) -> SBool
checkSizeLimit _ _ _ Nothing = true
checkSizeLimit p' _ cut (Just (pSize, cutSize)) =
        let pSizeNext = sumVal p'
            cutSizeNext = sumVal cut
            pSizeNow = literal pSize
            cutSizeNow = literal cutSize
--            checkTransition (t, tVal) = val cut t .<= literal tVal
--        in  sAnd (map checkTransition cutMap) .&& cutSizeNext .< cutSizeNow
--        in  (pSizeNext .< pSizeNow) .|| (pSizeNext .== pSizeNow .&& cutSizeNext .< cutSizeNow)
--        in  (cutSizeNext .< cutSizeNow) .|| (cutSizeNext .== cutSizeNow .&& pSizeNext .< pSizeNow)
        in  (cutSizeNext .< cutSizeNow)

checkSComponent :: PetriNet -> FiringVector -> Maybe (Integer, Integer) -> SIMap Place ->
        SIMap Transition -> SIMap Place -> SIMap Transition -> SIMap Transition ->
        SInteger -> SBool
checkSComponent net x sizeLimit p' t' yp yt cut k =
        checkPrePostPlaces net p' t' .&&
        checkPrePostTransitions net p' t' .&&
        checkMinimum x yt .&&
        checkMaximum x yt k .&&
        checkNext x yt k .&&
        checkSubset net p' t' yp yt .&&
        checkZeroUnused x t' yt .&&
        checkSizeLimit p' t' cut sizeLimit .&&
        checkClosed net p' t' yp yt .&&
        checkTokens net p' k .&&
        checkBinary p' t' .&&
        checkNonNegativityConstraints yp yt .&&
        checkCut net t' yt cut

checkGeneralSComponentSat :: PetriNet -> FiringVector -> Maybe (Integer, Integer) ->
        ConstraintProblem Integer (Cut, (Integer, Integer))
checkGeneralSComponentSat net x sizeLimit =
        let p' = makeVarMap $ places net
            t' = makeVarMap $ transitions net
            yp = makeVarMapWith prime $ places net
            yt = makeVarMapWith prime $ transitions net
            cut = makeVarMapWith (prime . prime) $ transitions net
        in  ("general S-component constraints", "cut",
            ["@k"] ++ getNames p' ++ getNames t' ++ getNames yp ++ getNames yt ++ getNames cut,
            \fm -> checkSComponent net x sizeLimit (fmap fm p') (fmap fm t')
                    (fmap fm yp) (fmap fm yt) (fmap fm cut) (fm "@k"),
            \fm -> cutFromAssignment net x (fmap fm p') (fmap fm t')
                    (fmap fm yp) (fmap fm yt) (fmap fm cut) (fm "@k"))

cutFromAssignment :: PetriNet -> FiringVector -> IMap Place -> IMap Transition ->
        IMap Place -> IMap Transition -> IMap Transition -> Integer ->
        (Cut, (Integer, Integer))
cutFromAssignment net x p' t' yp yt cut k =
        let ps = M.keys $ M.filter (> 0) p'
            ts = M.keys $ M.filter (> 0) t'
            cs = M.keys $ M.filter (> 0) cut
            filterComp i = M.keys . M.filter (== i)
            components = map (\i -> (filterComp i yp, filterComp i yt)) [1..k]
            psy = map (\p -> (yp M.! p, ([p],[]))) ps
            tsy = map (\t -> (yt M.! t, ([],[t]))) ts
            myInsert curMap (kVal, (ps', ts')) = M.insertWith insertPair kVal (ps', ts') curMap
            insertPair (ps'', ts'') (ps', ts') = (ps' ++ ps'', ts' ++ ts'')
            compMap = foldl myInsert M.empty (psy ++ tsy)
        in  ((components, cs), (genericLength ps, genericLength cs))
