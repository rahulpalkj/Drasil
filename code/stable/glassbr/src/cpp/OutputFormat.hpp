/** \file OutputFormat.hpp
    \author Nikitha Krithnan and W. Spencer Smith
    \brief Provides the function for writing outputs
*/
#ifndef OutputFormat_h
#define OutputFormat_h

#include <string>

using std::ofstream;
using std::string;

/** \brief Writes the output values to output.txt
    \param isSafePb probability of glass breakage safety requirement
    \param isSafeLR 3 second load equivalent resistance safety requirement
    \param P_b probability of breakage: the fraction of glass lites or plies that would break at the first occurrence of a specified load and duration, typically expressed in lites per 1000 (Ref: astm2016)
    \param J stress distribution factor (Function)
*/
void write_output(bool isSafePb, bool isSafeLR, double P_b, double J);

#endif
