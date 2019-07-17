-- | Alphabet of capital/lowercase English letters as symbols
module Language.Drasil.ShortHands where

import Language.Drasil.Symbol (Symbol(Var))

-- | c_ means capital _; l_ means lowercase _
cA,cB,cC,cD,cE,cF,cG,cH,cI,cJ,cK,cL,cM,cN,cO,cP,cQ,cR,cS,cT,cU,cV,cW,cX,cY,cZ,
  lA,lB,lC,lD,lE,lF,lG,lH,lI,lJ,lK,lL,lM,lN,lO,lP,lQ,lR,lS,lT,lU,lV,
  lW,lX,lY,lZ,lAlpha,cAlpha,lBeta,cBeta,lGamma,cGamma,lDelta,cDelta,lEpsilon,vEpsilon, 
  cEpsilon,lZeta,cZeta,lEta,cEta,lTheta,cTheta,lIota,cIota,lKappa,cKappa,lLambda,cLambda,
  lMu,cMu,lNu,cNu,lXi,cXi,lOmicron,cOmicron,lPi,cPi,lRho,cRho,lSigma,cSigma,lTau,cTau,
  lUpsilon,cUpsilon,lPhi,vPhi,cPhi,lChi,cChi,lPsi,cPsi,lOmega,cOmega,lNabla,lEll :: Symbol

cA = Var "A"
cB = Var "B"
cC = Var "C"
cD = Var "D"
cE = Var "E"
cF = Var "F"
cG = Var "G"
cH = Var "H"
cI = Var "I"
cJ = Var "J"
cK = Var "K"
cL = Var "L"
cM = Var "M"
cN = Var "N"
cO = Var "O"
cP = Var "P"
cQ = Var "Q"
cR = Var "R"
cS = Var "S"
cT = Var "T"
cU = Var "U"
cV = Var "V"
cW = Var "W"
cX = Var "X"
cY = Var "Y"
cZ = Var "Z"
lA = Var "a"
lB = Var "b"
lC = Var "c"
lD = Var "d"
lE = Var "e"
lF = Var "f"
lG = Var "g"
lH = Var "h"
lI = Var "i"
lJ = Var "j"
lK = Var "k"
lL = Var "l"
lM = Var "m"
lN = Var "n"
lO = Var "o"
lP = Var "p"
lQ = Var "q"
lR = Var "r"
lS = Var "s"
lT = Var "t"
lU = Var "u"
lV = Var "v"
lW = Var "w"
lX = Var "x"
lY = Var "y"
lZ = Var "z"
lAlpha = Var "α"
cAlpha = Var "Α"
lBeta = Var "β"
cBeta = Var "Β"
lGamma = Var "γ"
cGamma = Var "Γ"
lDelta = Var "δ"
cDelta = Var "Δ"
lEpsilon = Var "ϵ"
vEpsilon = Var "ε"
cEpsilon = Var "Ε"
lZeta = Var "ζ"
cZeta = Var "Ζ"
lEta = Var "η"
cEta = Var "Η"
lTheta = Var "θ"
cTheta = Var "Θ"
lIota = Var "ι"
cIota = Var "Ι"
lKappa = Var "κ"
cKappa = Var "Κ"
lLambda = Var "λ"
cLambda = Var "Λ"
lMu = Var "μ"
cMu = Var "Μ"
lNu = Var "ν"
cNu = Var "Ν"
lXi = Var "ξ"
cXi = Var "Ξ"
lOmicron = Var "ο"
cOmicron = Var "Ο"
lPi = Var "π"
cPi = Var "Π"
lRho = Var "ρ"
cRho = Var "Ρ"
lSigma = Var "σ"
cSigma = Var "Σ" 
lTau = Var "τ"
cTau = Var "Τ" 
lUpsilon = Var "υ"
cUpsilon = Var "Υ"
lPhi = Var "ϕ"
vPhi = Var "φ"
cPhi = Var "Φ"
lChi = Var "χ"
cChi = Var "Χ"
lPsi = Var "ψ"
cPsi = Var "Ψ"
lOmega = Var "ω"
cOmega = Var "Ω"
lNabla = Var "∇"
lEll = Var "ℓ"
