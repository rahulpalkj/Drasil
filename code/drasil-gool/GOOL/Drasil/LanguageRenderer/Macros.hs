{-# LANGUAGE PostfixOperators #-}

-- | Language-polymorphic functions that are defined by GOOL code
module GOOL.Drasil.LanguageRenderer.Macros (
  decrement, decrement1, increment, increment1,
  runStrategy, listSlice, stringListVals, stringListLists
) where

import GOOL.Drasil.CodeType (CodeType(..))
import GOOL.Drasil.ClassInterface (Label, MSBody, MSBlock, SVariable, SValue, 
  MSStatement, bodyStatements, oneLiner, TypeElim(getType),
  VariableElim(variableType), ValueSym(valueType), 
  NumericExpression((#+), (#-), (#*), (#/)), Comparison(..), 
  StatementSym(multi), AssignStatement((&+=), (&++)), (&=))
import qualified GOOL.Drasil.ClassInterface as S (BlockSym(block), 
  TypeSym(int, listInnerType), VariableSym(var), Literal(litInt), 
  VariableValue(valueOf), List(listSize, listAppend, listAccess), 
  StatementSym(valStmt), AssignStatement(assign), 
  DeclStatement(varDecDef, listDec), ControlStatement(for, forRange))
import GOOL.Drasil.RendererClasses (RenderSym, RenderValue(cast))
import qualified GOOL.Drasil.RendererClasses as S (
  RenderStatement(stmt, emptyStmt))
import qualified GOOL.Drasil.RendererClasses as RC (BodyElim(..),
  StatementElim(statement))
import GOOL.Drasil.Helpers (toCode, onStateValue, on2StateValues)
import GOOL.Drasil.State (MS, lensMStoVS)

import Data.Map as Map (lookup, fromList)
import Data.Maybe (fromMaybe)
import Control.Lens.Zoom (zoom)
import Text.PrettyPrint.HughesPJ (Doc, vcat)

decrement :: (RenderSym r) => SVariable r -> SValue r -> MSStatement r
decrement vr vl = vr &= (S.valueOf vr #- vl)

decrement1 :: (RenderSym r) => SVariable r -> MSStatement r
decrement1 v = v &= (S.valueOf v #- S.litInt 1)

increment :: (RenderSym r) => SVariable r -> SValue r -> MSStatement r
increment vr vl = vr &= S.valueOf vr #+ vl

increment1 :: (RenderSym r) => SVariable r -> MSStatement r
increment1 vr = vr &= S.valueOf vr #+ S.litInt 1

strat :: (RenderSym r, Monad r) => MSStatement r -> MSBody r -> MS (r Doc)
strat = on2StateValues (\result b -> toCode $ vcat [RC.body b, 
  RC.statement result])

runStrategy :: (RenderSym r, Monad r) => String -> [(Label, MSBody r)] -> 
  Maybe (SValue r) -> Maybe (SVariable r) -> MS (r Doc)
runStrategy l strats rv av = maybe
  (strError l "RunStrategy called on non-existent strategy") 
  (strat (S.stmt resultState)) (Map.lookup l (Map.fromList strats))
  where resultState = maybe S.emptyStmt asgState av
        asgState v = maybe (strError l 
          "Attempt to assign null return to a Value") (v &=) rv
        strError n s = error $ "Strategy '" ++ n ++ "': " ++ s ++ "."

listSlice :: (RenderSym r) => Maybe (SValue r) -> Maybe (SValue r) -> 
  Maybe (SValue r) -> SVariable r -> SValue r -> MSBlock r
listSlice b e s vnew vold = 
  let l_temp = "temp"
      var_temp = S.var l_temp (onStateValue variableType vnew)
      v_temp = S.valueOf var_temp
      l_i = "i_temp"
      var_i = S.var l_i S.int
      v_i = S.valueOf var_i
  in
    S.block [
      S.listDec 0 var_temp,
      S.for (S.varDecDef var_i (fromMaybe (S.litInt 0) b)) 
        (v_i ?< fromMaybe (S.listSize vold) e) (maybe (var_i &++) (var_i &+=) s)
        (oneLiner $ S.valStmt $ S.listAppend v_temp (S.listAccess vold v_i)),
      vnew &= v_temp]
      
stringListVals :: (RenderSym r) => [SVariable r] -> SValue r -> MSStatement r
stringListVals vars sl = zoom lensMStoVS sl >>= (\slst -> multi $ checkList 
  (getType $ valueType slst))
  where checkList (List String) = assignVals vars 0
        checkList _ = error 
          "Value passed to stringListVals must be a list of strings"
        assignVals [] _ = []
        assignVals (v:vs) n = S.assign v (cast (onStateValue variableType v) 
          (S.listAccess sl (S.litInt n))) : assignVals vs (n+1)

stringListLists :: (RenderSym r) => [SVariable r] -> SValue r -> MSStatement r
stringListLists lsts sl = zoom lensMStoVS sl >>= (\slst -> checkList (getType $ 
  valueType slst))
  where checkList (List String) = mapM (zoom lensMStoVS) lsts >>= listVals . 
          map (getType . variableType)
        checkList _ = error 
          "Value passed to stringListLists must be a list of strings"
        listVals [] = loop
        listVals (List _:vs) = listVals vs
        listVals _ = error 
          "All values passed to stringListLists must have list types"
        loop = S.forRange var_i (S.litInt 0) (S.listSize sl #/ numLists) 
          (S.litInt 1) (bodyStatements $ appendLists (map S.valueOf lsts) 0)
        appendLists [] _ = []
        appendLists (v:vs) n = S.valStmt (S.listAppend v (cast 
          (S.listInnerType $ onStateValue valueType v)
          (S.listAccess sl ((v_i #* numLists) #+ S.litInt n)))) 
          : appendLists vs (n+1)
        numLists = S.litInt (toInteger $ length lsts)
        var_i = S.var "stringlist_i" S.int
        v_i = S.valueOf var_i