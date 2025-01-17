## \file InputParameters.py
# \author Samuel J. Crawford, Brooks MacLachlan, and W. Spencer Smith
# \brief Provides the structure for holding input values, the function for reading inputs, and the function for checking the physical constraints on the input
import math

## \brief Structure for holding the input values
class InputParameters:
    ## \brief Initializes input object by reading inputs and checking physical constraints on the input
    # \param filename name of the input file
    def __init__(self, filename):
        self.get_input(filename)
        self.input_constraints()
    
    ## \brief Reads input from a file with the given file name
    # \param filename name of the input file
    def get_input(self, filename):
        infile = open(filename, "r")
        infile.readline()
        self.v_launch = float(infile.readline())
        infile.readline()
        self.theta = float(infile.readline())
        infile.readline()
        self.p_target = float(infile.readline())
        infile.close()
    
    ## \brief Verifies that input values satisfy the physical constraints
    def input_constraints(self):
        if (not(self.v_launch > 0.0)) :
            print("Warning: ", end="")
            print("v_launch has value ", end="")
            print(self.v_launch, end="")
            print(", but is suggested to be ", end="")
            print("above ", end="")
            print(0.0, end="")
            print(".")
        if (not(0.0 < self.theta and self.theta < math.pi / 2.0)) :
            print("Warning: ", end="")
            print("theta has value ", end="")
            print(self.theta, end="")
            print(", but is suggested to be ", end="")
            print("between ", end="")
            print(0.0, end="")
            print(" and ", end="")
            print(math.pi / 2.0, end="")
            print(" ((pi)/(2))", end="")
            print(".")
        if (not(self.p_target > 0.0)) :
            print("Warning: ", end="")
            print("p_target has value ", end="")
            print(self.p_target, end="")
            print(", but is suggested to be ", end="")
            print("above ", end="")
            print(0.0, end="")
            print(".")
