module Drasil.SSP.Defs where

import Drasil.SSP.Units

import Language.Drasil
import Data.Drasil.SI_Units
import Data.Drasil.Concepts.Documentation

import Control.Lens ((^.))

--FIXME: Remove unitless
sspSymbolList :: [UWrapper]
sspSymbolList = map uw sspSymbols

sspSymbols :: [UnitalChunk]
sspSymbols = [fricAngle, cohesion, dryWeight, satWeight, waterWeight,
              elastMod, poisson, coords, hWT, hUS, hSlip, xi, critCoords,
              fs, fsloc, si, pi_f, ti, ri, wi, kc, hi, dHi, ei, xi_2,
              ubi, uti, ni, ni_prime, ni_star, qi, alpha_i, beta_i,
              omega_i, lambda, fi, bi, lbi, lsi, hi_2, n, f, m, upsilon,
              delta, k, k_sti, k_bti, k_sni, k_bni, k_tr, k_no, du_i,
              dv_i, dx_i, dy_i]

fricAngle, cohesion, dryWeight, satWeight, waterWeight, elastMod, 
  poisson, coords, hWT, hUS, hSlip, xi, critCoords, fs, fsloc, si, pi_f,
  ti, ri, wi, kc, hi, dHi, ei, xi_2, ubi, uti, ni, ni_prime, ni_star,
  qi, alpha_i, beta_i, omega_i, lambda, fi, bi, lbi, lsi, hi_2, n, f,
  m, upsilon, delta, k, k_sti, k_bti, k_sni, k_bni, k_tr, k_no, du_i,
  dv_i, dx_i, dy_i, s, p :: UnitalChunk

--FIXME: Many of these need to be split into term, defn pairs as their defns are
-- mixed into the terms.

fixme :: String
fixme = "FIXME: Define this or remove the need for definitions"

fricAngle   = uc' "varphi'" (cn "effective angle of friction")
  fixme
  (Concat [Greek Phi_V, Atomic "'"]) degree

cohesion    = uc' "c'" (cn "effective cohesion")
  fixme
  (Atomic "c'") pascal

dryWeight   = uc' "gamma" (cn "unit weight of dry soil/ground layer")
  fixme
  (Greek Gamma_L) specific_weight

satWeight   = uc' "gamma_sat" (cn "unit weight of saturated soil/ground layer")
  fixme
  (sub (Greek Gamma_L) (Atomic "Sat")) specific_weight

waterWeight = uc' "gamma_w" (cn "unit weight of water")
  fixme
  (sub (Greek Gamma_L) lW) specific_weight

elastMod    = uc' "E" (cn "elastic modulus")
  fixme
  cE pascal

poisson     = uc' "nu" (cn "Poisson's ratio")
  fixme
  (Greek Nu_L) unitless

coords      = uc' "(x,y)" 
  (cn $ "cartesian position coordinates; y is considered parallel to the " ++ 
  "direction of the force of gravity and x is considered perpendicular to y")
  fixme
  (Atomic "(x,y)") metre

hWT         = uc' "y_wt,i" 
  (cn $ "the y ordinate, or height of the water table at i; refers to either " ++
  "slice i midpoint, or slice interface i")
  fixme
  (sub lY (Atomic "wt,i")) metre

hUS         = uc' "y_us,i" (cn $ "the y ordinate, or height of the top of the " ++
  "slope at i; refers to either slice i midpoint, or slice interface i")
  fixme
  (sub lY (Atomic "us,i")) metre

hSlip       = uc' "y_slip,i" (cn $ "the y ordinate, or height of the slip " ++
  "surface at i; refers to either slice i midpoint, or slice interface i") 
  fixme
  (sub lY (Atomic "slip,i")) metre

xi          = uc' "x_i" 
  (cn "the x ordinate; refers to either slice i midpoint, or slice interface i")
  fixme
  (sub lX lI) metre

critCoords  = uc' "(xcs,ycs)" (cn $ "the set of x and y coordinates that " ++
  "describe the vertices of the critical slip surface")
  fixme
  (Concat [sub (Atomic "({x") (Atomic "cs"), sub (Atomic "},{y") (Atomic "cs"), 
  Atomic "})"]) metre

fs          = uc' "FS" (cn $ "global factor of safety describing the " ++
  "stability of a surface in a slope")
  fixme
  (Atomic "FS") unitless

fsloc       = uc' "FS_loci" (cn "local factor of safety specific to a slice i")
  fixme
  (sub (Atomic "FS") (Atomic "Loc,i")) unitless

si          = uc' "S_i" (cn "mobilized shear force for slice i")
  fixme
  (sub cS lI) newton

s           = uc' "S" (cn "mobilized shear force")
  fixme
  cS newton

pi_f        = uc' "P_i" (cn $ "shear resistance; Mohr Coulomb frictional " ++
  "force that describes the limit of mobilized shear force the slice i " ++
  "can withstand before failure")
  fixme
  (sub cP lI) newton

p           = uc' "P" (cn "shear resistance")
  fixme
  (cP) newton

ti          = uc' "T_i" 
  (cn "mobilized shear force without the influence of interslice forces for slice i")
  fixme
  (sub cT lI) newton

ri          = uc' "R_i" 
  (cn "shear resistance without the influence of interslice forces for slice i")
  fixme
  (sub cR lI) newton

wi          = uc' "W_i" (cn "weight; downward force caused by gravity on slice i")
  fixme
  (sub cW lI) newton

kc          = uc' "K_c" (cn $ "earthquake load factor; proportionality factor " ++
  "of force that weight pushes outwards; caused by seismic earth movements")
  fixme
  (sub cK lC) unitless

hi          = uc' "H_i" (cn $ "interslice water force exerted in the " ++
  "x-ordinate direction between adjacent slices (for interslice index i)")
  fixme
  (sub cH lI) newton

dHi         = uc' "dH_i" (cn "difference between interslice forces on acting in the x-ordinate direction of the slice on each side (for interslice index i)")
  fixme
  (sub (Concat [Greek Delta, cH]) lI) newton

ei          = uc' "E_i" (cn "interslice normal force being exerted between adjacent slices (for interslice index i)")
  fixme
  (sub cE lI) newton

xi_2        = uc' "X_i" (cn "interslice shear force being exerted between adjacent slices (for interslice index i)")
  fixme
  (sub cX lI) newton

ubi         = uc' "U_b,i" (cn "base hydrostatic force arising from water pressure within the slice (for slice index i)")
  fixme
  (sub cU (Atomic "b,i")) newton

uti         = uc' "U_t,i" (cn "surface hydrostatic force arising from water pressure acting into the slice from standing water on the slope surface (for slice index i)")
  fixme
  (sub cU (Atomic "t,i")) newton

ni          = uc' "N_i" (cn "total reactive force for a soil surface subject to a body resting on it")
  fixme
  (sub cN lI) newton

ni_prime    = uc' "N'_i" (cn "effective normal force of a soil surface, subtracting pore water reactive force from total reactive force")
  fixme
  (sub (Atomic "N'") lI) newton

ni_star     = uc' "N*_i" (cn "effective normal force of a soil surface, neglecting the influence of interslice forces")
  fixme
  (sub (Atomic "N*") lI) newton

qi          = uc' "Q_i" (cn "imposed surface load; a downward force acting into the surface from midpoint of slice i")
  fixme
  (sub cQ lI) newton

alpha_i     = uc' "alpha_i" (cn "angle of the base of the mass relative to the horizontal (for slice index i)")
  fixme
  (sub (Greek Alpha_L) lI) degree

beta_i      = uc' "beta_i" (cn "angle of the surface of the mass relative to the horizontal (for slice index i)") 
  fixme
  (sub (Greek Beta_L) lI) degree

omega_i     = uc' "omega_i" (cn "angle of imposed surface load acting into the surface relative to the vertical (for slice index i)")
  fixme
  (sub (Greek Omega_L) lI) degree

lambda      = uc' "lambda" (cn "ratio between interslice normal and shear forces (applied to all interslices)")
  fixme
  (Greek Lambda_L) unitless

fi          = uc' "f_i" (cn "scaling function for magnitude of interslice forces as a function of the x coordinate (at interslice index i); can be constant or a half-sine")
  fixme
  (sub lF lI) unitless

bi          = uc' "b_i" (cn "base width of the slice in the x-ordinate direction only (for slice index i)")
  fixme
  (sub lB lI) metre

lbi         = uc' "l_b,i" (cn "total length of the base of a slice (for slice index i)")
  fixme
  (sub (Greek Ell) (Atomic "b,i")) metre

lsi         = uc' "l_s,i" (cn "length of an interslice surface, from slip base to slope surface in a vertical line from an interslice vertex (for interslice index i)")
  fixme
  (sub (Greek Ell) (Atomic "s,i")) metre

hi_2        = uc' "h_i" (cn "midpoint height; distance from the slip base to the slope surface in a vertical line from the midpoint of the slice (for slice i)")
  fixme
  (sub lH lI) metre

n           = uc' "n" (cn "number of slices the slip mass has been divided into")
  fixme
  lN unitless

f           = uc' "F" (cn "generic force; assumed 1D allowing a scalar")
  fixme
  cF metre

m           = uc' "M" (cn "moment of a body; assumed 2D allowing a scalar")
  fixme
  cM momentOfForceU

upsilon     = uc' "Upsilon" (cn "generic minimization function or algorithm")
  fixme
  (Greek Upsilon) unitless

delta       = uc' "delta" (cn "generic displacement of a body")
  fixme
  (Greek Delta_L) metre

k           = uc' "K" (cn "stiffness (how much a body resists displacement when subject to a force)")
  fixme
  (cK) stiffnessU

k_sti       = uc' "K_st,i" (cn "shear stiffness of an interslice surface, without length adjustment (for interslice index i)")
  fixme
  (sub cK (Atomic "st,i")) pascal

k_bti       = uc' "K_bt,i" (cn "shear stiffness of a slice base surface, without length adjustment (for slice index i)")
  fixme
  (sub cK (Atomic "bt,i")) pascal

k_sni       = uc' "K_sn,i" (cn "normal stiffness of an interslice surface, without length adjustment (for interslice index i)")
  fixme
  (sub cK (Atomic "sn,i")) pascal

k_bni       = uc' "K_bn,i" (cn "normal stiffness of a slice base surface, without length adjustment (for slice index i)")
  fixme
  (sub cK (Atomic "bn,i")) pascal

k_tr        = uc' "K_tr" (cn "residual shear stiffness")
  fixme
  (sub cK (Atomic "tr")) pascal

k_no        = uc' "K_no" (cn "residual normal stiffness")
  fixme
  (sub cK (Atomic "no")) pascal

du_i        = uc' "du_i" (cn "shear displacement of a slice (for slice index i)")
  fixme
  (sub (Concat [Greek Delta_L, Atomic "u"]) lI) metre

dv_i        = uc' "dv_i" (cn "normal displacement of a slice (for slice index i)")
  fixme
  (sub (Concat [Greek Delta_L, Atomic "v"]) lI) metre

dx_i        = uc' "dx_i" (cn "displacement of a slice in the x-ordinate direction (for slice index i)") 
  fixme
  (sub (Concat [Greek Delta_L, Atomic "x"]) lI) metre

dy_i        = uc' "dy_i" (cn "displacement of a slice in the y-ordinate direction (for slice index i)") 
  fixme
  (sub (Concat [Greek Delta_L, Atomic "y"]) lI) metre


----Acronyms-----
-- FIXME: Use acronyms
acronyms :: [CINP]
acronyms = [assumption,dataDefn,genDefn,goalStmt,inModel,likelyChg,
  physSyst,requirement,srs,ssa,thModel]
  
ssa, ssp :: CINP
ssa = commonINP "ssa" (cnIS "slope stability analysis") "SSA"
ssp = commonINP "ssp" (cn' "slope stability problem") "SSP"

slpSrf, crtSlpSrf :: NPNC
slpSrf = npnc "slpSrf" (cn' "slip surface")
crtSlpSrf = compoundNPNC (npnc "critical" (cn "critical")) slpSrf


----Theoretical Models----
fs_rc :: RelationConcept
fs_rc = makeRC "fs_rc" (pn "Factor of Safety") fs_desc fs_rel

fs_rel :: Relation
fs_rel = (C fs) := (C p) / (C s)

fs_desc :: Sentence
fs_desc = 
  S "The stability metric of the slope, known as the factor of safety" +:+
  (sParen (P (fs ^. symbol))) `sC` S "is determined by the ratio of the" +:+
  S "shear force at the base of the slope" +:+ (sParen (P (s ^. symbol))) `sC` 
  S "and the resistive shear" +:+. (sParen (P (p ^. symbol)))
