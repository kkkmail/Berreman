namespace BerremanTests
open System
open System.Numerics
open Berreman
open Berreman.FourierTransformPrimitives
open Xunit
open Xunit.Abstractions
open FsCheck

type FourierTransformTests(output : ITestOutputHelper) =

    let writeLine (s : string) = output.WriteLine s

    /// Utility function: compare two Complex arrays or lists within a tolerance,
    /// logging all relevant info for any failed comparisons in a table-like format.
    let areClose tolerance (expected : seq<Complex>) (actual : seq<Complex>) =
        let arrE = Seq.toArray expected
        let arrA = Seq.toArray actual

        // First check if lengths differ.
        if arrE.Length <> arrA.Length then
            writeLine $"Lengths differ: expected {arrE.Length}, got {arrA.Length}"
            false
        else
            // Create a list of all indices where the difference exceeds 'tolerance'
            let differences =
                [
                    for i in 0 .. arrE.Length - 1 do
                        let e = arrE.[i]
                        let a = arrA.[i]
                        let reDiff = abs(e.Real - a.Real)
                        let imDiff = abs(e.Imaginary - a.Imaginary)
                        if reDiff > tolerance || imDiff > tolerance then
                            yield i, e, a, reDiff, imDiff
                ]

            if differences <> [] then
                // Print a header
                writeLine ""
                writeLine "Idx  | Expected                    | Actual                      | reDiff      | imDiff"
                writeLine "---- | --------------------------- | --------------------------- | ----------- | -----------"

                // Print each mismatch on one line, with padding to form columns
                for (idx, e, a, reDiff, imDiff) in differences do
                    let idxStr     = idx.ToString().PadRight(4)
                    // Complex.ToString() is typically "(x, y)", so we can pad to e.g. 27 chars
                    let expectedStr = e.ToString().PadRight(27)
                    let actualStr   = a.ToString().PadRight(27)
                    let reDiffStr   = reDiff.ToString("0.###E+0").PadRight(11)
                    let imDiffStr   = imDiff.ToString("0.###E+0").PadRight(11)

                    writeLine $"{idxStr} | {expectedStr} | {actualStr} | {reDiffStr} | {imDiffStr}"
                false
            else
                true

    // --------------------
    // The remaining tests:
    // --------------------

    [<Fact>]
    member _.``SimpleFft forward->backward recovers original list`` () =
        let input = [ Complex(1.0, 0.0)
                      Complex(0.0, 2.0)
                      Complex(-1.0, 1.0)
                      Complex(3.0, -1.0) ]
        let forward = SimpleFourierTransform.fft ForwardTransform input
        let backward = SimpleFourierTransform.fft BackwardTransform forward
        Assert.True(areClose 1e-12 input backward, "Forward->Backward did not recover original list")

    [<Fact>]
    member _.``SimpleFft backward->forward recovers original list`` () =
        let input = [ Complex(2.0, 0.0)
                      Complex(1.0, -1.0)
                      Complex(0.0, 3.0)
                      Complex(-2.0, 2.0) ]
        let backward = SimpleFourierTransform.fft BackwardTransform input
        let forward = SimpleFourierTransform.fft ForwardTransform backward
        Assert.True(areClose 1e-12 input forward, "Backward->Forward did not recover original list")

    [<Fact>]
    member _.``SimpleFft delta impulse produces constant frequency domain`` () =
        let input = [ Complex(1.0, 0.0)
                      Complex(0.0, 0.0)
                      Complex(0.0, 0.0)
                      Complex(0.0, 0.0) ]
        let output = SimpleFourierTransform.fft ForwardTransform input
        let expected = [ Complex(1.0, 0.0)
                         Complex(1.0, 0.0)
                         Complex(1.0, 0.0)
                         Complex(1.0, 0.0) ]
        Assert.True(areClose 1e-12 expected output, "Delta impulse did not become constant")

    [<Fact>]
    member _.``OptimizedFft forward->backward recovers original array`` () =
        let input = [| Complex(1.0, 0.0)
                       Complex(2.0, -1.0)
                       Complex(-1.0, 4.0)
                       Complex(3.0, 3.0) |]
        let forward = FourierTransform.fft ForwardTransform input
        let backward = FourierTransform.fft BackwardTransform forward
        Assert.True(areClose 1e-12 input backward, "Forward->Backward did not recover original array")

    [<Fact>]
    member _.``OptimizedFft backward->forward recovers original array`` () =
        let input = [| Complex(3.0, 2.0)
                       Complex(1.0, 0.0)
                       Complex(-2.0, 1.0)
                       Complex(4.0, -3.0) |]
        let backward = FourierTransform.fft BackwardTransform input
        let forward = FourierTransform.fft ForwardTransform backward
        Assert.True(areClose 1e-12 input forward, "Backward->Forward did not recover original array")

    [<Fact>]
    member _.``OptimizedFft matches Dft for small input`` () =
        let input = [| Complex(1.0, 0.0)
                       Complex(0.0, 1.0)
                       Complex(2.0, -1.0)
                       Complex(-3.0, 5.0) |]
        let dftResult = FourierTransform.dft ForwardTransform input
        let fftResult = FourierTransform.fft ForwardTransform input
        Assert.True(areClose 1e-12 dftResult fftResult, "FFT and DFT differ on small input")

    [<Fact>]
    member _.``OptimizedFft delta impulse test`` () =
        let input = [| Complex(1.0, 0.0)
                       Complex(0.0, 0.0)
                       Complex(0.0, 0.0)
                       Complex(0.0, 0.0) |]
        let output = FourierTransform.fft ForwardTransform input
        let expected = [| Complex(1.0, 0.0)
                          Complex(1.0, 0.0)
                          Complex(1.0, 0.0)
                          Complex(1.0, 0.0) |]
        Assert.True(areClose 1e-12 expected output, "Delta impulse did not become constant")
