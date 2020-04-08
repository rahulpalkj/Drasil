{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PostfixOperators #-}

-- | The logic to render C# code is contained in this module
module GOOL.Drasil.LanguageRenderer.CSharpRenderer (
  -- * C# Code Configuration -- defines syntax of all C# code
  CSharpCode(..)
) where

import Utils.Drasil (indent)

import GOOL.Drasil.CodeType (CodeType(..))
import GOOL.Drasil.ClassInterface (Label, MSBody, VSType, SVariable, SValue, 
  MSStatement, MSParameter, SMethod, ProgramSym(..), FileSym(..), 
  PermanenceSym(..), BodySym(..), oneLiner, BlockSym(..), TypeSym(..), 
  ControlBlock(..), InternalControlBlock(..), VariableSym(..), ValueSym(..), 
  NumericExpression(..), BooleanExpression(..), ValueExpression(..), funcApp,
  selfFuncApp, extFuncApp, newObj, Selector(..), ($.), InternalValueExp(..), 
  objMethodCall, objMethodCallNoParams, FunctionSym(..), SelectorFunction(..), 
  StatementSym(..), (&=), ControlStatement(..), ScopeSym(..), 
  ParameterSym(..), MethodSym(..), StateVarSym(..), ClassSym(..), ModuleSym(..),
  ODEInfo(..), ODEOptions(..), ODEMethod(..))
import GOOL.Drasil.RendererClasses (RenderSym, InternalFile(..),
  ImportSym(..), InternalPerm(..), InternalBody(..), InternalBlock(..), 
  InternalType(..), UnaryOpSym(..), BinaryOpSym(..), InternalOp(..), 
  InternalVariable(..), InternalValue(..), InternalFunction(..), 
  InternalStatement(..), InternalScope(..), MethodTypeSym(..), 
  InternalParam(..), InternalMethod(..), InternalStateVar(..), 
  InternalClass(..), InternalMod(..), BlockCommentSym(..))
import GOOL.Drasil.LanguageRenderer (classDocD, multiStateDocD, bodyDocD, 
  outDoc, printFileDocD, destructorError, paramDocD, methodDocD, listDecDocD, 
  mkSt, mkStNoEnd, breakDocD, continueDocD, mkStateVal, mkVal, mkVar, 
  classVarDocD, objVarDocD, funcDocD, castDocD, listSetFuncDocD, castObjDocD, 
  staticDocD, dynamicDocD, bindingError, privateDocD, publicDocD, dot, 
  blockCmtStart, blockCmtEnd, docCmtStart, bodyStart, bodyEnd, endStatement, 
  commentStart, elseIfLabel, inLabel, blockCmtDoc, docCmtDoc, commentedItem, 
  addCommentsDocD, commentedModD, variableList, appendToBody, surroundBody)
import qualified GOOL.Drasil.LanguageRenderer.LanguagePolymorphic as G (
  multiBody, block, multiBlock, bool, int, float, double, char, string, 
  listType, arrayType, listInnerType, obj, funcType, void, runStrategy, 
  listSlice, notOp, csc, sec, cot, negateOp, equalOp, notEqualOp, greaterOp, 
  greaterEqualOp, lessOp, lessEqualOp,plusOp, minusOp, multOp, divideOp, 
  moduloOp, andOp, orOp, var, staticVar, extVar, self, classVar, objVarSelf, 
  listVar, arrayElem, iterVar, pi, litTrue, litFalse, litChar, litDouble, 
  litFloat, litInt, litString, litList, valueOf, arg, argsList, inlineIf, 
  objAccess, objMethodCall, objMethodCallNoParams, indexOf, call, 
  funcAppMixedArgs, selfFuncAppMixedArgs, extFuncAppMixedArgs, 
  libFuncAppMixedArgs, newObjMixedArgs, libNewObjMixedArgs, lambda, notNull, 
  func, get, set, listSize, listAdd, listAppend, iterBegin, iterEnd, 
  listAccess, listSet, getFunc, setFunc, listAddFunc, listAppendFunc, 
  iterBeginError, iterEndError, listAccessFunc, listSetFunc, printSt, state, 
  loopState, emptyState, assign, multiAssignError, decrement, increment, 
  decrement1, increment1, varDec, varDecDef, listDec, listDecDef', arrayDec, 
  arrayDecDef, objDecNew, objDecNewNoParams, extObjDecNew, 
  extObjDecNewNoParams, constDecDef, discardInput, openFileR, openFileW, 
  openFileA, closeFile, discardFileLine, stringListVals, stringListLists, 
  returnState, multiReturnError, valState, comment, freeError, throw, ifCond, 
  switch, ifExists, for, forRange, forEach, while, tryCatch, checkState, 
  notifyObservers, construct, param, method, getMethod, setMethod, constructor, 
  docMain, function, mainFunction, docFunc, docInOutFunc, intFunc, stateVar, 
  stateVarDef, constVar, buildClass, implementingClass, docClass, 
  commentedClass, intClass, buildModule', modFromData, fileDoc, docMod, 
  fileFromData)
import GOOL.Drasil.LanguageRenderer.LanguagePolymorphic (unOpPrec, unExpr, 
  unExpr', unExprNumDbl, typeUnExpr, powerPrec, binExpr, binExprNumDbl', 
  typeBinExpr)
import GOOL.Drasil.AST (Terminator(..), ScopeTag(..), FileType(..), 
  FileData(..), fileD, FuncData(..), fd, ModData(..), md, updateMod, 
  MethodData(..), mthd, updateMthd, OpData(..), od, ParamData(..), pd, 
  updateParam, ProgData(..), progD, TypeData(..), td, ValData(..), vd, 
  updateValDoc, Binding(..), VarData(..), vard)
import GOOL.Drasil.Helpers (toCode, toState, onCodeValue, onStateValue, 
  on2CodeValues, on2StateValues, on3CodeValues, on3StateValues, onCodeList, 
  onStateList)
import GOOL.Drasil.State (VS, lensGStoFS, lensMStoVS, modifyReturn, revFiles,
  addLangImport, addLangImportVS, addLibImport, setFileType, getClassName, 
  setCurrMain, setODEDepVars, getODEDepVars)

import Prelude hiding (break,print,(<>),sin,cos,tan,floor)
import Control.Lens.Zoom (zoom)
import Control.Applicative (Applicative)
import Control.Monad (join)
import Control.Monad.State (modify)
import Data.List (elemIndex, intercalate)
import Text.PrettyPrint.HughesPJ (Doc, text, (<>), (<+>), ($$), parens, empty,
  vcat, lbrace, rbrace, colon, space)

csExt :: String
csExt = "cs"

newtype CSharpCode a = CSC {unCSC :: a} deriving Eq

instance Functor CSharpCode where
  fmap f (CSC x) = CSC (f x)

instance Applicative CSharpCode where
  pure = CSC
  (CSC f) <*> (CSC x) = CSC (f x)

instance Monad CSharpCode where
  return = CSC
  CSC x >>= f = f x

instance ProgramSym CSharpCode where
  type Program CSharpCode = ProgData
  prog n files = do
    fs <- mapM (zoom lensGStoFS) files
    modify revFiles
    return $ onCodeList (progD n) fs

instance RenderSym CSharpCode

instance FileSym CSharpCode where
  type RenderFile CSharpCode = FileData
  fileDoc m = modify (setFileType Combined) >> G.fileDoc csExt top bottom m

  docMod = G.docMod csExt

instance InternalFile CSharpCode where
  top _ = toCode empty
  bottom = toCode empty

  commentedMod cmt m = on2StateValues (on2CodeValues commentedModD) m cmt

  fileFromData = G.fileFromData (\m fp -> onCodeValue (fileD fp) m)

instance ImportSym CSharpCode where
  type Import CSharpCode = Doc
  langImport n = toCode $ csImport n
  modImport = langImport

  importDoc = unCSC

instance PermanenceSym CSharpCode where
  type Permanence CSharpCode = Doc
  static = toCode staticDocD
  dynamic = toCode dynamicDocD

instance InternalPerm CSharpCode where
  permDoc = unCSC
  binding = error $ bindingError csName

instance BodySym CSharpCode where
  type Body CSharpCode = Doc
  body = onStateList (onCodeList bodyDocD)

  addComments s = onStateValue (onCodeValue (addCommentsDocD s commentStart))

instance InternalBody CSharpCode where
  bodyDoc = unCSC
  docBody = onStateValue toCode
  multiBody = G.multiBody 

instance BlockSym CSharpCode where
  type Block CSharpCode = Doc
  block = G.block

instance InternalBlock CSharpCode where
  blockDoc = unCSC
  docBlock = onStateValue toCode
  multiBlock = G.multiBlock

instance TypeSym CSharpCode where
  type Type CSharpCode = TypeData
  bool = addSystemImport G.bool
  int = G.int
  float = G.float
  double = G.double
  char = G.char
  string = G.string
  infile = csInfileType
  outfile = csOutfileType
  listType t = modify (addLangImportVS "System.Collections.Generic") >> 
    G.listType "List" t
  arrayType = G.arrayType
  listInnerType = G.listInnerType
  obj = G.obj
  -- enumType = G.enumType
  funcType = G.funcType
  iterator t = t
  void = G.void

  getType = cType . unCSC
  getTypeString = typeString . unCSC
  
instance InternalType CSharpCode where
  getTypeDoc = typeDoc . unCSC
  typeFromData t s d = toCode $ td t s d

instance ControlBlock CSharpCode where
  runStrategy = G.runStrategy

  solveODE info opts = modify (addLibImport "Microsoft.Research.Oslo" . 
    addLangImport "System.Linq") >> 
    multiBlock [
      block [
        objDecNewNoParams optsVar,
        objVar optsVar (var "AbsoluteTolerance" float) &= absTol opts,
        objVar optsVar (var "AbsoluteTolerance" float) &= relTol opts],
      block [
        varDecDef sol (extFuncApp "Ode" (csODEMethod $ solveMethod opts) odeT 
        [tInit info, 
        newObj vec [initVal info], 
        lambda [iv, dv] (newObj vec [dv >>= (\dpv -> modify (setODEDepVars 
          [variableName dpv]) >> ode info)]),
        valueOf optsVar])],
      block [
        varDecDef points (objMethodCallNoParams spArray 
        (objMethodCall void (valueOf sol) "SolveFromToStep" 
          [tInit info, tFinal info, stepSize opts]) "ToArray"),
        listDecDef dv [],
        forEach sp (valueOf points) 
          (oneLiner $ valState $ listAppend (valueOf dv) (valueOf $ 
          objVar sp (var "X" (listInnerType $ onStateValue variableType dv))))]
    ]
    where optsVar = var "opts" (obj "Options")
          iv = indepVar info
          dv = depVar info
          odeT = obj "IEnumerable<SolPoint>"
          vec = obj "Vector"
          sol = var "sol" odeT
          spArray = arrayType (obj "SolPoint")
          points = var "points" spArray
          sp = var "sp" (obj "SolPoint")

instance InternalControlBlock CSharpCode where
  listSlice' = G.listSlice

instance UnaryOpSym CSharpCode where
  type UnaryOp CSharpCode = OpData
  notOp = G.notOp
  negateOp = G.negateOp
  sqrtOp = addSystemImport $ unOpPrec "Math.Sqrt"
  absOp = addSystemImport $ unOpPrec "Math.Abs"
  logOp = addSystemImport $ unOpPrec "Math.Log10"
  lnOp = addSystemImport $ unOpPrec "Math.Log"
  expOp = addSystemImport $ unOpPrec "Math.Exp"
  sinOp = addSystemImport $ unOpPrec "Math.Sin"
  cosOp = addSystemImport $ unOpPrec "Math.Cos"
  tanOp = addSystemImport $ unOpPrec "Math.Tan"
  asinOp = addSystemImport $ unOpPrec "Math.Asin"
  acosOp = addSystemImport $ unOpPrec "Math.Acos"
  atanOp = addSystemImport $ unOpPrec "Math.Atan"
  floorOp = addSystemImport $ unOpPrec "Math.Floor"
  ceilOp = addSystemImport $ unOpPrec "Math.Ceiling"

instance BinaryOpSym CSharpCode where
  type BinaryOp CSharpCode = OpData
  equalOp = G.equalOp
  notEqualOp = G.notEqualOp
  greaterOp = G.greaterOp
  greaterEqualOp = G.greaterEqualOp
  lessOp = G.lessOp
  lessEqualOp = G.lessEqualOp
  plusOp = G.plusOp
  minusOp = G.minusOp
  multOp = G.multOp
  divideOp = G.divideOp
  powerOp = addSystemImport $ powerPrec "Math.Pow"
  moduloOp = G.moduloOp
  andOp = G.andOp
  orOp = G.orOp

instance InternalOp CSharpCode where
  uOpDoc = opDoc . unCSC
  bOpDoc = opDoc . unCSC
  uOpPrec = opPrec . unCSC
  bOpPrec = opPrec . unCSC
  
  uOpFromData p d = toState $ toCode $ od p d
  bOpFromData p d = toState $ toCode $ od p d

instance VariableSym CSharpCode where
  type Variable CSharpCode = VarData
  var = G.var
  staticVar = G.staticVar
  const = var
  extVar = G.extVar
  self = G.self
  -- enumVar = G.enumVar
  classVar = G.classVar classVarDocD
  extClassVar = classVar
  objVar = on2StateValues csObjVar
  objVarSelf = G.objVarSelf
  listVar  = G.listVar
  arrayElem i = G.arrayElem (litInt i)
  iterVar = G.iterVar

  variableName = varName . unCSC
  variableType = onCodeValue varType

instance InternalVariable CSharpCode where
  variableBind = varBind . unCSC
  variableDoc = varDoc . unCSC
  varFromData b n t d = on2CodeValues (vard b n) t (toCode d)

instance ValueSym CSharpCode where
  type Value CSharpCode = ValData
  litTrue = G.litTrue
  litFalse = G.litFalse
  litChar = G.litChar
  litDouble = G.litDouble
  litFloat = G.litFloat
  litInt = G.litInt
  litString = G.litString
  litArray = G.litList arrayType
  litList = G.litList listType

  pi = G.pi

  -- ($:) = enumElement

  valueOf v = join $ on2StateValues (\dvs vr -> maybe (G.valueOf v) (listAccess 
    (G.valueOf v) . litInt . toInteger) (elemIndex (variableName vr) dvs)) 
    getODEDepVars v
  arg n = G.arg (litInt n) argsList
  -- enumElement = G.enumElement
  
  argsList = G.argsList "args"

  valueType = onCodeValue valType

instance NumericExpression CSharpCode where
  (#~) = unExpr' negateOp
  (#/^) = unExprNumDbl sqrtOp
  (#|) = unExpr absOp
  (#+) = binExpr plusOp
  (#-) = binExpr minusOp
  (#*) = binExpr multOp
  (#/) = binExpr divideOp
  (#%) = binExpr moduloOp
  (#^) = binExprNumDbl' powerOp

  log = unExprNumDbl logOp
  ln = unExprNumDbl lnOp
  exp = unExprNumDbl expOp
  sin = unExprNumDbl sinOp
  cos = unExprNumDbl cosOp
  tan = unExprNumDbl tanOp
  csc = G.csc
  sec = G.sec
  cot = G.cot
  arcsin = unExprNumDbl asinOp
  arccos = unExprNumDbl acosOp
  arctan = unExprNumDbl atanOp
  floor = unExpr floorOp
  ceil = unExpr ceilOp

instance BooleanExpression CSharpCode where
  (?!) = typeUnExpr notOp bool
  (?&&) = typeBinExpr andOp bool
  (?||) = typeBinExpr orOp bool

  (?<) = typeBinExpr lessOp bool
  (?<=) = typeBinExpr lessEqualOp bool
  (?>) = typeBinExpr greaterOp bool
  (?>=) = typeBinExpr greaterEqualOp bool
  (?==) = typeBinExpr equalOp bool
  (?!=) = typeBinExpr notEqualOp bool
  
instance ValueExpression CSharpCode where
  inlineIf = G.inlineIf

  funcAppMixedArgs = G.funcAppMixedArgs
  selfFuncAppMixedArgs = G.selfFuncAppMixedArgs dot self
  extFuncAppMixedArgs = G.extFuncAppMixedArgs
  libFuncAppMixedArgs = G.libFuncAppMixedArgs
  newObjMixedArgs = G.newObjMixedArgs "new "
  extNewObjMixedArgs _ = newObjMixedArgs
  libNewObjMixedArgs = G.libNewObjMixedArgs

  lambda = G.lambda csLambda

  notNull = G.notNull

instance InternalValue CSharpCode where
  inputFunc = addSystemImport $ mkStateVal string (text "Console.ReadLine()")
  printFunc = addSystemImport $ mkStateVal void (text "Console.Write")
  printLnFunc = addSystemImport $ mkStateVal void (text "Console.WriteLine")
  printFileFunc = on2StateValues (\v -> mkVal v . printFileDocD "Write" . 
    valueDoc) void
  printFileLnFunc = on2StateValues (\v -> mkVal v . printFileDocD "WriteLine" . 
    valueDoc) void
  
  cast = csCast

  call = G.call (colon <> space)
  
  valuePrec = valPrec . unCSC
  valueDoc = val . unCSC
  valFromData p t d = on2CodeValues (vd p) t (toCode d)

instance Selector CSharpCode where
  objAccess = G.objAccess

  argExists i = listAccess argsList (litInt $ fromIntegral i)
  
  indexOf = G.indexOf "IndexOf"
  
instance InternalValueExp CSharpCode where
  objMethodCallMixedArgs' = G.objMethodCall 
  objMethodCallNoParams' = G.objMethodCallNoParams

instance FunctionSym CSharpCode where
  type Function CSharpCode = FuncData
  func = G.func

  get = G.get
  set = G.set

  listSize = G.listSize
  listAdd = G.listAdd
  listAppend = G.listAppend

  iterBegin = G.iterBegin
  iterEnd = G.iterEnd

instance SelectorFunction CSharpCode where
  listAccess = G.listAccess
  listSet = G.listSet

instance InternalFunction CSharpCode where
  getFunc = G.getFunc
  setFunc = G.setFunc

  listSizeFunc = funcFromData (funcDocD (text "Count")) int
  listAddFunc _ = G.listAddFunc "Insert"
  listAppendFunc = G.listAppendFunc "Add"

  iterBeginFunc _ = error $ G.iterBeginError csName
  iterEndFunc _ = error $ G.iterEndError csName

  listAccessFunc = G.listAccessFunc
  listSetFunc = G.listSetFunc listSetFuncDocD 
    
  functionType = onCodeValue fType
  functionDoc = funcDoc . unCSC

  funcFromData d = onStateValue (onCodeValue (`fd` d))

instance InternalStatement CSharpCode where
  printSt _ _ = G.printSt
  
  multiAssign _ _ = error $ G.multiAssignError csName
  multiReturn _ = error $ G.multiReturnError csName 

  state = G.state
  loopState = G.loopState

  emptyState = G.emptyState
  statementDoc = fst . unCSC
  statementTerm = snd . unCSC
  
  stateFromData d t = toCode (d, t)

instance StatementSym CSharpCode where
  type Statement CSharpCode = (Doc, Terminator)
  assign = G.assign Semi
  (&-=) = G.decrement
  (&+=) = G.increment
  (&++) = G.increment1
  (&--) = G.decrement1

  varDec v = zoom lensMStoVS v >>= (\v' -> csVarDec (variableBind v') $ 
    G.varDec static dynamic v)
  varDecDef = G.varDecDef
  listDec n v = zoom lensMStoVS v >>= (\v' -> G.listDec (listDecDocD v') 
    (litInt n) v)
  listDecDef = G.listDecDef'
  arrayDec n = G.arrayDec (litInt n)
  arrayDecDef = G.arrayDecDef
  objDecDef = varDecDef
  objDecNew = G.objDecNew
  extObjDecNew = G.extObjDecNew
  objDecNewNoParams = G.objDecNewNoParams
  extObjDecNewNoParams = G.extObjDecNewNoParams
  constDecDef = G.constDecDef
  funcDecDef = csFuncDecDef

  print = outDoc False Nothing printFunc
  printLn = outDoc True Nothing printLnFunc
  printStr = outDoc False Nothing printFunc . litString
  printStrLn = outDoc True Nothing printLnFunc . litString

  printFile f = outDoc False (Just f) (printFileFunc f)
  printFileLn f = outDoc True (Just f) (printFileLnFunc f)
  printFileStr f = outDoc False (Just f) (printFileFunc f) . litString
  printFileStrLn f = outDoc True (Just f) (printFileLnFunc f) . litString

  getInput v = v &= csInput (onStateValue variableType v) inputFunc
  discardInput = G.discardInput csDiscardInput
  getFileInput f v = v &= csInput (onStateValue variableType v) (csFileInput f)
  discardFileInput f = valState $ csFileInput f

  openFileR = G.openFileR csOpenFileR
  openFileW = G.openFileW csOpenFileWorA
  openFileA = G.openFileA csOpenFileWorA
  closeFile = G.closeFile "Close"

  getFileInputLine = getFileInput
  discardFileLine = G.discardFileLine "ReadLine"
  stringSplit d vnew s = assign vnew $ newObj (listType string) 
    [s $. func "Split" (listType string) [litChar d]]

  stringListVals = G.stringListVals
  stringListLists = G.stringListLists

  break = toState $ mkSt breakDocD
  continue = toState $ mkSt continueDocD

  returnState = G.returnState Semi

  valState = G.valState Semi

  comment = G.comment commentStart

  free _ = error $ G.freeError csName -- could set variable to null? Might be misleading.

  throw msg = modify (addLangImport "System") >> G.throw csThrowDoc Semi msg

  inOutCall = csInOutCall funcApp
  selfInOutCall = csInOutCall selfFuncApp
  extInOutCall m = csInOutCall (extFuncApp m)

  multi = onStateList (onCodeList multiStateDocD)

instance ControlStatement CSharpCode where
  ifCond = G.ifCond bodyStart elseIfLabel bodyEnd
  switch = G.switch

  ifExists = G.ifExists

  for = G.for bodyStart bodyEnd
  forRange = G.forRange
  forEach = G.forEach bodyStart bodyEnd (text "foreach") inLabel 
  while = G.while bodyStart bodyEnd

  tryCatch = G.tryCatch csTryCatch

  checkState = G.checkState
  notifyObservers = G.notifyObservers

  getFileInputAll f v = while ((f $. funcFromData (text ".EndOfStream") bool) 
    ?!) (oneLiner $ valState $ listAppend (valueOf v) (csFileInput f))

instance ScopeSym CSharpCode where
  type Scope CSharpCode = Doc
  private = toCode privateDocD
  public = toCode publicDocD

instance InternalScope CSharpCode where
  scopeDoc = unCSC
  scopeFromData _ = toCode

instance MethodTypeSym CSharpCode where
  type MethodType CSharpCode = TypeData
  mType = zoom lensMStoVS 
  construct = G.construct

instance ParameterSym CSharpCode where
  type Parameter CSharpCode = ParamData
  param = G.param paramDocD
  pointerParam = param

instance InternalParam CSharpCode where
  parameterName = variableName . onCodeValue paramVar
  parameterType = variableType . onCodeValue paramVar
  parameterDoc = paramDoc . unCSC
  paramFromData v d = on2CodeValues pd v (toCode d)

instance MethodSym CSharpCode where
  type Method CSharpCode = MethodData
  method = G.method
  getMethod = G.getMethod
  setMethod = G.setMethod
  constructor ps is b = getClassName >>= (\n -> G.constructor n ps is b)

  docMain = G.docMain
 
  function = G.function
  mainFunction = G.mainFunction string "Main"

  docFunc = G.docFunc

  inOutMethod n = csInOut (method n)

  docInOutMethod n = G.docInOutFunc (inOutMethod n)

  inOutFunc n = csInOut (function n)

  docInOutFunc n = G.docInOutFunc (inOutFunc n)

instance InternalMethod CSharpCode where
  intMethod m n s p t ps b = modify (if m then setCurrMain else id) >> 
    on3StateValues (\tp pms bd -> methodFromData Pub $ methodDocD n s p tp pms 
    bd) t (sequence ps) b
  intFunc = G.intFunc
  commentedFunc cmt m = on2StateValues (on2CodeValues updateMthd) m 
    (onStateValue (onCodeValue commentedItem) cmt)
    
  destructor _ = error $ destructorError csName
  
  methodDoc = mthdDoc . unCSC
  methodFromData _ = toCode . mthd

instance StateVarSym CSharpCode where
  type StateVar CSharpCode = Doc
  stateVar = G.stateVar
  stateVarDef _ = G.stateVarDef
  constVar _ = G.constVar empty

instance InternalStateVar CSharpCode where
  stateVarDoc = unCSC
  stateVarFromData = onStateValue toCode

instance ClassSym CSharpCode where
  type Class CSharpCode = Doc
  buildClass = G.buildClass
  -- enum = G.enum
  extraClass = buildClass
  implementingClass = G.implementingClass

  docClass = G.docClass

instance InternalClass CSharpCode where
  intClass = G.intClass classDocD

  inherit n = toCode $ maybe empty ((colon <+>) . text) n
  implements is = toCode $ colon <+> text (intercalate ", " is)

  commentedClass = G.commentedClass

  classDoc = unCSC
  classFromData d = d

instance ModuleSym CSharpCode where
  type Module CSharpCode = ModData
  buildModule n = G.buildModule' n langImport
  
instance InternalMod CSharpCode where
  moduleDoc = modDoc . unCSC
  modFromData n = G.modFromData n (toCode . md n)
  updateModuleDoc f = onCodeValue (updateMod f)

instance BlockCommentSym CSharpCode where
  type BlockComment CSharpCode = Doc
  blockComment lns = toCode $ blockCmtDoc lns blockCmtStart blockCmtEnd
  docComment = onStateValue (\lns -> toCode $ docCmtDoc lns docCmtStart 
    blockCmtEnd)

  blockCommentDoc = unCSC

addSystemImport :: VS a -> VS a
addSystemImport = (>>) $ modify (addLangImportVS "System")

csName :: String
csName = "C#"

csODEMethod :: ODEMethod -> String
csODEMethod RK45 = "RK547M"
csODEMethod BDF = "GearBDF"
csODEMethod _ = error "Chosen ODE method unavailable in C#"

csImport :: Label -> Doc
csImport n = text ("using " ++ n) <> endStatement

csInfileType :: (RenderSym repr) => VSType repr
csInfileType = modifyReturn (addLangImportVS "System.IO") $ 
  typeFromData File "StreamReader" (text "StreamReader")

csOutfileType :: (RenderSym repr) => VSType repr
csOutfileType = modifyReturn (addLangImportVS "System.IO") $ 
  typeFromData File "StreamWriter" (text "StreamWriter")

csLambda :: (RenderSym repr) => [repr (Variable repr)] -> repr (Value repr) -> 
  Doc
csLambda ps ex = parens (variableList ps) <+> text "=>" <+> valueDoc ex

csCast :: VSType CSharpCode -> SValue CSharpCode -> SValue CSharpCode
csCast t v = join $ on2StateValues (\tp vl -> csCast' (getType tp) (getType $ 
  valueType vl) tp vl) t v
  where csCast' Double String _ _ = funcApp "Double.Parse" double [v]
        csCast' Float String _ _ = funcApp "Single.Parse" float [v]
        csCast' _ _ tp vl = mkStateVal t (castObjDocD (castDocD (getTypeDoc 
          tp)) (valueDoc vl))

csFuncDecDef :: (RenderSym repr) => SVariable repr -> 
  [SVariable repr] -> SValue repr -> MSStatement repr
csFuncDecDef v ps r = on3StateValues (\vr pms b -> mkStNoEnd $ 
  getTypeDoc (variableType vr) <+> text (variableName vr) <> parens 
  (variableList pms) <+> bodyStart $$ indent (bodyDoc b) $$ bodyEnd) 
  (zoom lensMStoVS v) (mapM (zoom lensMStoVS) ps) (oneLiner $ returnState r)

csThrowDoc :: (RenderSym repr) => repr (Value repr) -> Doc
csThrowDoc errMsg = text "throw new" <+> text "Exception" <> 
  parens (valueDoc errMsg)

csTryCatch :: (RenderSym repr) => repr (Body repr) -> repr (Body repr) -> Doc
csTryCatch tb cb = vcat [
  text "try" <+> lbrace,
  indent $ bodyDoc tb,
  rbrace <+> text "catch" <+> 
    lbrace,
  indent $ bodyDoc cb,
  rbrace]

csDiscardInput :: (RenderSym repr) => repr (Value repr) -> Doc
csDiscardInput = valueDoc

csFileInput :: (RenderSym repr) => SValue repr -> SValue repr
csFileInput = onStateValue (\f -> mkVal (valueType f) (valueDoc f <> dot <> 
  text "ReadLine()"))

csInput :: (RenderSym repr) => VSType repr -> SValue repr -> SValue repr
csInput tp inF = tp >>= (\t -> csInputImport (getType t) $ onStateValue (\inFn 
  -> mkVal t $ text (csInput' (getType t)) <> parens (valueDoc inFn)) inF)
  where csInput' Integer = "Int32.Parse"
        csInput' Float = "Single.Parse"
        csInput' Double = "Double.Parse"
        csInput' Boolean = "Boolean.Parse"
        csInput' String = ""
        csInput' Char = "Char.Parse"
        csInput' _ = error "Attempt to read value of unreadable type"
        csInputImport t = if t `elem` [Integer, Float, Double, Boolean, Char] 
          then addSystemImport else id

csOpenFileR :: (RenderSym repr) => SValue repr -> VSType repr -> SValue repr
csOpenFileR n r = newObj r [n]

csOpenFileWorA :: (RenderSym repr) => SValue repr -> VSType repr -> SValue repr 
  -> SValue repr
csOpenFileWorA n w a = newObj w [n, a] 

csRef :: Doc -> Doc
csRef p = text "ref" <+> p

csOut :: Doc -> Doc
csOut p = text "out" <+> p

csInOutCall :: (Label -> VSType CSharpCode -> [SValue CSharpCode] -> 
  SValue CSharpCode) -> Label -> [SValue CSharpCode] -> [SVariable CSharpCode] 
  -> [SVariable CSharpCode] -> MSStatement CSharpCode
csInOutCall f n ins [out] [] = assign out $ f n (onStateValue variableType out) 
  ins
csInOutCall f n ins [] [out] = assign out $ f n (onStateValue variableType out) 
  (valueOf out : ins)
csInOutCall f n ins outs both = valState $ f n void (map (onStateValue 
  (onCodeValue (updateValDoc csRef)) . valueOf) both ++ ins ++ map 
  (onStateValue (onCodeValue (updateValDoc csOut)) . valueOf) outs)

csVarDec :: Binding -> MSStatement CSharpCode -> MSStatement CSharpCode
csVarDec Static _ = error "Static variables can't be declared locally to a function in C#. Use stateVar to make a static state variable instead."
csVarDec Dynamic d = d

csObjVar :: (RenderSym repr) => repr (Variable repr) -> repr (Variable repr) -> 
  repr (Variable repr)
csObjVar o v = csObjVar' (variableBind v)
  where csObjVar' Static = error 
          "Cannot use objVar to access static variables through an object in C#"
        csObjVar' Dynamic = mkVar (variableName o ++ "." ++ variableName v) 
          (variableType v) (objVarDocD (variableDoc o) (variableDoc v))

csInOut :: (CSharpCode (Scope CSharpCode) -> CSharpCode (Permanence CSharpCode) 
    -> VSType CSharpCode -> [MSParameter CSharpCode] -> MSBody CSharpCode -> 
    SMethod CSharpCode)
  -> CSharpCode (Scope CSharpCode) -> CSharpCode (Permanence CSharpCode) -> 
  [SVariable CSharpCode] -> [SVariable CSharpCode] -> [SVariable CSharpCode] -> 
  MSBody CSharpCode -> SMethod CSharpCode
csInOut f s p ins [v] [] b = f s p (onStateValue variableType v) (map param ins)
  (on3StateValues (on3CodeValues surroundBody) (varDec v) b (returnState $ 
  valueOf v))
csInOut f s p ins [] [v] b = f s p (onStateValue variableType v) 
  (map param $ v : ins) (on2StateValues (on2CodeValues appendToBody) b 
  (returnState $ valueOf v))
csInOut f s p ins outs both b = f s p void (map (onStateValue (onCodeValue 
  (updateParam csRef)) . param) both ++ map param ins ++ map (onStateValue 
  (onCodeValue (updateParam csOut)) . param) outs) b