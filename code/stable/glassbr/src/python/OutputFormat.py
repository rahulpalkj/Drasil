## \file OutputFormat.py
# \author Nikitha Krithnan and W. Spencer Smith
# \brief Provides the function for writing outputs
## \brief Writes the output values to output.txt
# \param isSafePb probability of glass breakage safety requirement
# \param isSafeLR 3 second load equivalent resistance safety requirement
# \param P_b probability of breakage: the fraction of glass lites or plies that would break at the first occurrence of a specified load and duration, typically expressed in lites per 1000 (Ref: astm2016)
# \param J stress distribution factor (Function)
def write_output(isSafePb, isSafeLR, P_b, J):
    outfile = open("log.txt", "a")
    print("function write_output called with inputs: {", file=outfile)
    print("  isSafePb = ", end="", file=outfile)
    print(isSafePb, end="", file=outfile)
    print(", ", file=outfile)
    print("  isSafeLR = ", end="", file=outfile)
    print(isSafeLR, end="", file=outfile)
    print(", ", file=outfile)
    print("  P_b = ", end="", file=outfile)
    print(P_b, end="", file=outfile)
    print(", ", file=outfile)
    print("  J = ", end="", file=outfile)
    print(J, file=outfile)
    print("  }", file=outfile)
    outfile.close()
    
    outputfile = open("output.txt", "w")
    print("isSafePb = ", end="", file=outputfile)
    print(isSafePb, file=outputfile)
    print("isSafeLR = ", end="", file=outputfile)
    print(isSafeLR, file=outputfile)
    print("P_b = ", end="", file=outputfile)
    print(P_b, file=outputfile)
    print("J = ", end="", file=outputfile)
    print(J, file=outputfile)
    outputfile.close()
