module Drasil.DblPendulum.GenDefs (genDefns, velocityIXGD, velocityIYGD, accelerationIXGD, accelerationIYGD, hForceOnPendulumGD, vForceOnPendulumGD) where

import Prelude hiding (cos, sin)
import Language.Drasil
import Theory.Drasil (GenDefn, gdNoRefs)
import Utils.Drasil

-- import Data.Drasil.Concepts.Documentation (coordinate, symbol_)
import Data.Drasil.Concepts.Math (xComp, yComp)
import Data.Drasil.Quantities.Physics(velocity, angularVelocity, xVel, yVel,
    angularAccel, xAccel, yAccel, acceleration, force, tension, gravitationalAccel)
import Data.Drasil.Concepts.Physics(pendulum)
import Data.Drasil.Quantities.PhysicalProperties(mass)


-- import Drasil.Projectile.Assumptions (cartSyst, constAccel, pointMass, timeStartZero, twoDMotion)
-- import Drasil.Projectile.References (hibbeler2004)
import Drasil.DblPendulum.Unitals (lenRod, pendAngle)

genDefns :: [GenDefn]
genDefns = [velocityIXGD, velocityIYGD, accelerationIXGD, accelerationIYGD, hForceOnPendulumGD, vForceOnPendulumGD] 


-- ----------
velocityIXGD :: GenDefn
velocityIXGD = gdNoRefs velocityIXRC (getUnit velocity)
           (Just velocityIXDeriv) "velocityIX" [{-Notes-}]

velocityIXRC :: RelationConcept
velocityIXRC =  makeRC "velocityIXRC" (nounPhraseSent $ foldlSent_ 
            [ phrase xComp `sOf` phrase velocity `ofThe` phrase pendulum])
            EmptyS velocityIXRel

velocityIXRel :: Relation             
velocityIXRel = sy xVel $= sy angularVelocity * sy lenRod * cos (sy pendAngle)

velocityIXDeriv :: Derivation
velocityIXDeriv = mkDerivName (phrase xComp +:+ phrase velocity) [ E velocityIXRel]



---------------------
velocityIYGD :: GenDefn
velocityIYGD = gdNoRefs velocityIYRC (getUnit velocity)
           (Just velocityIYDeriv) "velocityIY" [{-Notes-}]

velocityIYRC :: RelationConcept
velocityIYRC = makeRC "velocityIYRC" (nounPhraseSent $ foldlSent_ 
            [ phrase yComp `sOf` phrase velocity `ofThe` phrase pendulum]) EmptyS velocityIYRel
 
velocityIYRel :: Relation             
velocityIYRel = sy yVel $= sy angularVelocity * sy lenRod * cos (sy pendAngle)

velocityIYDeriv :: Derivation
velocityIYDeriv = mkDerivName (phrase yComp +:+ phrase velocity) [ E velocityIYRel]

-----------------------
accelerationIXGD :: GenDefn
accelerationIXGD = gdNoRefs accelerationIXRC (getUnit acceleration)
           (Just accelerationIXDeriv) "accelerationIX" [{-Notes-}]

accelerationIXRC :: RelationConcept
accelerationIXRC = makeRC "accelerationIXRC" (nounPhraseSent $ foldlSent_ 
            [ phrase xComp `sOf` phrase acceleration `ofThe` phrase pendulum]) EmptyS accelerationIXRel
 
accelerationIXRel :: Relation             
accelerationIXRel = sy xAccel $= negate (sy angularVelocity * sy lenRod * sin (sy pendAngle)) + sy angularAccel * sy lenRod * cos (sy pendAngle)

accelerationIXDeriv :: Derivation
accelerationIXDeriv = mkDerivName (phrase xComp +:+ phrase acceleration) [ E accelerationIXRel]

-----------------------
accelerationIYGD :: GenDefn
accelerationIYGD = gdNoRefs accelerationIYRC (getUnit acceleration)
           (Just accelerationIYDeriv) "accelerationIY" [{-Notes-}]

accelerationIYRC :: RelationConcept
accelerationIYRC = makeRC "accelerationIYRC" (nounPhraseSent $ foldlSent_ 
            [ phrase yComp `sOf` phrase acceleration `ofThe` phrase pendulum]) EmptyS accelerationIYRel
 
accelerationIYRel :: Relation             
accelerationIYRel = sy yAccel $= (sy angularVelocity * sy lenRod * cos (sy pendAngle)) + sy angularAccel * sy lenRod * sin (sy pendAngle)

accelerationIYDeriv :: Derivation
accelerationIYDeriv = mkDerivName (phrase yComp +:+ phrase acceleration) [ E accelerationIYRel]

-------------------------------------Horizontal force acting on the pendulum 
hForceOnPendulumGD :: GenDefn
hForceOnPendulumGD = gdNoRefs hForceOnPendulumRC (getUnit force)
           (Just hForceOnPendulumDeriv) "hForceOnPendulum" [{-Notes-}]

hForceOnPendulumRC :: RelationConcept
hForceOnPendulumRC = makeRC "hForceOnPendulumRC" (nounPhraseSent $ foldlSent_ 
            [ S "horizontal", phrase force, S "on the", phrase pendulum]) EmptyS hForceOnPendulumRel
 
hForceOnPendulumRel :: Relation             
hForceOnPendulumRel = sy force $= sy mass * sy xAccel $= negate (sy tension * sin (sy pendAngle))
                     

hForceOnPendulumDeriv :: Derivation
hForceOnPendulumDeriv = mkDerivName (phrase force +:+ phrase pendulum) [ E hForceOnPendulumRel]

----------------------------------------Vertical force acting on the pendulum 
vForceOnPendulumGD :: GenDefn
vForceOnPendulumGD = gdNoRefs vForceOnPendulumRC (getUnit force)
           (Just vForceOnPendulumDeriv) "vForceOnPendulum" [{-Notes-}]

vForceOnPendulumRC :: RelationConcept
vForceOnPendulumRC = makeRC "vForceOnPendulumRC" (nounPhraseSent $ foldlSent_ 
            [ S "vertical", phrase force, S "on the", phrase pendulum]) EmptyS vForceOnPendulumRel
 
vForceOnPendulumRel :: Relation             
vForceOnPendulumRel = sy force $= sy mass * sy yAccel $= sy tension * cos (sy pendAngle) - sy mass * sy gravitationalAccel

vForceOnPendulumDeriv :: Derivation
vForceOnPendulumDeriv = mkDerivName (phrase force +:+ phrase pendulum) [ E vForceOnPendulumRel]