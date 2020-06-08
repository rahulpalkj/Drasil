module Language.Drasil.Code.Imperative.ConceptMatch (
  chooseConcept, conceptToGOOL
) where

import Language.Drasil.Choices (Choices(..), CodeConcept(..), MatchedConceptMap)

import GOOL.Drasil (SValue, OOProg, MathConstant(..))

import Prelude hiding (pi)
import qualified Data.Map as Map (map)
import Control.Monad.State (State)
import Text.PrettyPrint.HughesPJ (Doc)

-- Currently we don't have any Choices that would prevent a CodeConcept from being mapped, so we just take the head of the list of CodeConcepts
chooseConcept :: Choices -> State Doc MatchedConceptMap
chooseConcept chs = sequence $ Map.map (chooseConcept' chs) (conceptMatch chs)
  where chooseConcept' _ [] = error $ "Empty list of CodeConcepts in the " ++ 
          "ConceptMatchMap"
        chooseConcept' _ cs = return $ head cs

conceptToGOOL :: (OOProg r) => CodeConcept -> SValue r
conceptToGOOL Pi = pi

