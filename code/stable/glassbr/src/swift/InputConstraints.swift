/** InputConstraints.swift
    Provides the function for checking the physical constraints and software constraints on the input
    - Authors: Nikitha Krithnan and W. Spencer Smith
*/
import Foundation

/** Verifies that input values satisfy the physical constraints and software constraints
    - Parameter inParams: structure holding the input values
*/
func input_constraints(_ inParams: inout InputParameters) throws -> Void {
    var outfile: FileHandle
    do {
        outfile = try FileHandle(forWritingTo: FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first!.appendingPathComponent("log.txt"))
        try outfile.seekToEnd()
    } catch {
        throw "Error opening file."
    }
    do {
        try outfile.write(contentsOf: Data("function input_constraints called with inputs: {".utf8))
        try outfile.write(contentsOf: Data("\n".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data("  inParams = ".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data("Instance of InputParameters object".utf8))
        try outfile.write(contentsOf: Data("\n".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data("  }".utf8))
        try outfile.write(contentsOf: Data("\n".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.close()
    } catch {
        throw "Error closing file."
    }
    
    if !(0.1 <= inParams.a && inParams.a <= 5.0) {
        print("a has value ", terminator: "")
        print(inParams.a, terminator: "")
        print(", but is expected to be ", terminator: "")
        print("between ", terminator: "")
        print(0.1, terminator: "")
        print(" (d_min)", terminator: "")
        print(" and ", terminator: "")
        print(5.0, terminator: "")
        print(" (d_max)", terminator: "")
        print(".")
        throw "InputError"
    }
    if !(0.1 <= inParams.b && inParams.b <= 5.0) {
        print("b has value ", terminator: "")
        print(inParams.b, terminator: "")
        print(", but is expected to be ", terminator: "")
        print("between ", terminator: "")
        print(0.1, terminator: "")
        print(" (d_min)", terminator: "")
        print(" and ", terminator: "")
        print(5.0, terminator: "")
        print(" (d_max)", terminator: "")
        print(".")
        throw "InputError"
    }
    if !(4.5 <= inParams.w && inParams.w <= 910.0) {
        print("w has value ", terminator: "")
        print(inParams.w, terminator: "")
        print(", but is expected to be ", terminator: "")
        print("between ", terminator: "")
        print(4.5, terminator: "")
        print(" (w_min)", terminator: "")
        print(" and ", terminator: "")
        print(910.0, terminator: "")
        print(" (w_max)", terminator: "")
        print(".")
        throw "InputError"
    }
    if !(6.0 <= inParams.SD && inParams.SD <= 130.0) {
        print("SD has value ", terminator: "")
        print(inParams.SD, terminator: "")
        print(", but is expected to be ", terminator: "")
        print("between ", terminator: "")
        print(6.0, terminator: "")
        print(" (SD_min)", terminator: "")
        print(" and ", terminator: "")
        print(130.0, terminator: "")
        print(" (SD_max)", terminator: "")
        print(".")
        throw "InputError"
    }
    if !(inParams.AR <= 5.0) {
        print("AR has value ", terminator: "")
        print(inParams.AR, terminator: "")
        print(", but is expected to be ", terminator: "")
        print("below ", terminator: "")
        print(5.0, terminator: "")
        print(" (AR_max)", terminator: "")
        print(".")
        throw "InputError"
    }
    
    if !(inParams.a > 0.0) {
        print("a has value ", terminator: "")
        print(inParams.a, terminator: "")
        print(", but is expected to be ", terminator: "")
        print("above ", terminator: "")
        print(0.0, terminator: "")
        print(".")
        throw "InputError"
    }
    if !(inParams.a >= inParams.b) {
        print("a has value ", terminator: "")
        print(inParams.a, terminator: "")
        print(", but is expected to be ", terminator: "")
        print("above ", terminator: "")
        print(inParams.b, terminator: "")
        print(" (b)", terminator: "")
        print(".")
        throw "InputError"
    }
    if !(0.0 < inParams.b && inParams.b <= inParams.a) {
        print("b has value ", terminator: "")
        print(inParams.b, terminator: "")
        print(", but is expected to be ", terminator: "")
        print("between ", terminator: "")
        print(0.0, terminator: "")
        print(" and ", terminator: "")
        print(inParams.a, terminator: "")
        print(" (a)", terminator: "")
        print(".")
        throw "InputError"
    }
    if !(inParams.w > 0.0) {
        print("w has value ", terminator: "")
        print(inParams.w, terminator: "")
        print(", but is expected to be ", terminator: "")
        print("above ", terminator: "")
        print(0.0, terminator: "")
        print(".")
        throw "InputError"
    }
    if !(0.0 <= inParams.P_btol && inParams.P_btol <= 1.0) {
        print("P_btol has value ", terminator: "")
        print(inParams.P_btol, terminator: "")
        print(", but is expected to be ", terminator: "")
        print("between ", terminator: "")
        print(0.0, terminator: "")
        print(" and ", terminator: "")
        print(1.0, terminator: "")
        print(".")
        throw "InputError"
    }
    if !(inParams.TNT > 0.0) {
        print("TNT has value ", terminator: "")
        print(inParams.TNT, terminator: "")
        print(", but is expected to be ", terminator: "")
        print("above ", terminator: "")
        print(0.0, terminator: "")
        print(".")
        throw "InputError"
    }
    if !(inParams.SD > 0.0) {
        print("SD has value ", terminator: "")
        print(inParams.SD, terminator: "")
        print(", but is expected to be ", terminator: "")
        print("above ", terminator: "")
        print(0.0, terminator: "")
        print(".")
        throw "InputError"
    }
    if !(inParams.AR >= 1.0) {
        print("AR has value ", terminator: "")
        print(inParams.AR, terminator: "")
        print(", but is expected to be ", terminator: "")
        print("above ", terminator: "")
        print(1.0, terminator: "")
        print(".")
        throw "InputError"
    }
}