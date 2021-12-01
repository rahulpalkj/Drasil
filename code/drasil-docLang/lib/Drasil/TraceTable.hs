--{-# LANGUAGE LambdaCase #-}
-- | Defines a DLPlate for tracability between pieces of information.
module Drasil.TraceTable where

import Drasil.DocumentLanguage.Core

import Language.Drasil
import Language.Drasil.Development (lnames')
import Database.Drasil (TraceMap, traceMap)
import Theory.Drasil (Theory(..))

import Control.Lens ((^.))
import Data.Functor.Constant (Constant(Constant))
import Data.Generics.Multiplate (foldFor, preorderFold, purePlate)

-- | Creates a dependency plate for 'UID's.
dependencyPlate :: DLPlate (Constant [(UID, [UID])])
dependencyPlate = preorderFold $ purePlate {
  goals = Constant <$> \(GProg _ c) -> getDependenciesOf [defs] c,
  assumptions = Constant <$> \(AssumpProg a) -> getDependenciesOf [defs] a,
  tMs = Constant <$> \(TMProg _ _ t)     -> getDependenciesOf [\x -> map (^. defn) (x ^. defined_quant) ++
    map (^. defn) (x ^. operations), notes] t,
  dDs = Constant <$> \(DDProg _ _ d _) -> getDependenciesOf [derivs, notes] d,
  gDs = Constant <$> \(GDProg _ _ g _) -> getDependenciesOf [defs, derivs, notes] g,
  iMs = Constant <$> \(IMProg _ _ i _) -> getDependenciesOf [derivs, notes] i,
  fReqsSub' = Constant . getDependenciesOf [defs] <$> \(FReqsProg' c _) -> c,
  fReqsSub = Constant . getDependenciesOf [defs] <$> \(FReqsProg c _) -> c,
  nonFReqsSub = Constant . getDependenciesOf [defs] <$> \(NonFReqsProg c) -> c,
  lcsSec = Constant . getDependenciesOf [defs] <$> \(LCsProg c) -> c,
  ucsSec = Constant . getDependenciesOf [defs] <$> \(UCsProg c) -> c
} where
  getDependenciesOf :: HasUID a => [a -> [Sentence]] -> [a] -> [(UID, [UID])]
  getDependenciesOf fs = map (\x -> (x ^. uid, concatMap (lnames' . ($ x)) fs))
  defs :: Definition a => a -> [Sentence]
  defs x = [x ^. defn]
  derivs :: HasDerivation a => a -> [Sentence]
  derivs x = maybe [] (\(Derivation h d) -> h : d) $ x ^. derivations
  notes :: HasAdditionalNotes a => a -> [Sentence]
  notes = (^. getNotes)

-- | Creates a traceability map from document sections.
generateTraceMap :: [DocSection] -> TraceMap
generateTraceMap = traceMap . concatMap (foldFor docSec dependencyPlate)
