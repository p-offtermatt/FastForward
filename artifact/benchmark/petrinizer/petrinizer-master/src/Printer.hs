module Printer
    (validateId,intercalate,renderPlace,renderTransition,renderShow)
where

import Data.Char
import Data.ByteString.Builder
import Data.Monoid

import PetriNet

validateId :: String -> String
validateId "" = "_"
validateId (x:xs) = (if isAlpha x then x else '_') :
        map (\c -> if isAlphaNum c then c else '_') xs


intercalate :: Builder -> [Builder] -> Builder
intercalate _ [] = mempty
intercalate sep (x:xs) = x <> go xs
      where go = foldr (\y -> (<>) (sep <> y)) mempty

renderPlace :: Place -> Builder
renderPlace = renderShow

renderTransition :: Transition -> Builder
renderTransition = renderShow

renderShow :: (Show a) => a -> Builder
renderShow = stringUtf8 . show
