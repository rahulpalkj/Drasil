module Language.Drasil.Code.Imperative.State (
  State(..)
) where

import Language.Drasil.CodeSpec (AuxFile, CodeSpec, Comments, 
  ConstraintBehaviour, InputModule, Logging, Structure)

-- Private State, used to push these options around the generator
data State = State {
  codeSpec :: CodeSpec,
  date :: String,
  inStruct :: Structure,
  inMod :: InputModule,
  logName :: String,
  logKind :: Logging,
  commented :: [Comments],
  auxiliaries :: [AuxFile],
  currentModule :: String,

  onSfwrC :: ConstraintBehaviour,
  onPhysC :: ConstraintBehaviour
}