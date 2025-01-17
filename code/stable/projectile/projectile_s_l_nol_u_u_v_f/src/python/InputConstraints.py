## \file InputConstraints.py
# \author Samuel J. Crawford, Brooks MacLachlan, and W. Spencer Smith
# \brief Provides the function for checking the physical constraints on the input
import math

## \brief Verifies that input values satisfy the physical constraints
# \param v_launch launch speed: the initial speed of the projectile when launched (m/s)
# \param theta launch angle: the angle between the launcher and a straight line from the launcher to the target (rad)
# \param p_target target position: the distance from the launcher to the target (m)
def input_constraints(v_launch, theta, p_target):
    if (not(v_launch > 0.0)) :
        print("Warning: ", end="")
        print("v_launch has value ", end="")
        print(v_launch, end="")
        print(", but is suggested to be ", end="")
        print("above ", end="")
        print(0.0, end="")
        print(".")
    if (not(0.0 < theta and theta < math.pi / 2.0)) :
        print("Warning: ", end="")
        print("theta has value ", end="")
        print(theta, end="")
        print(", but is suggested to be ", end="")
        print("between ", end="")
        print(0.0, end="")
        print(" and ", end="")
        print(math.pi / 2.0, end="")
        print(" ((pi)/(2))", end="")
        print(".")
    if (not(p_target > 0.0)) :
        print("Warning: ", end="")
        print("p_target has value ", end="")
        print(p_target, end="")
        print(", but is suggested to be ", end="")
        print("above ", end="")
        print(0.0, end="")
        print(".")
