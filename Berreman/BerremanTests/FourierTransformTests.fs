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


    /// <summary>
    ///   Creates a single-frequency sinusoid array of length N,
    ///   x[n] = exp(i * direction.sign * 2π * freqIndex * n / N).
    ///   ForwardTransform => direction.sign = -1.0 => negative exponent
    ///   BackwardTransform => direction.sign = +1.0 => positive exponent
    /// </summary>
    let makeSinusoid (direction: FourierTransformDirection) freqIndex N =
        let s = direction.sign  // -1.0 if forward, +1.0 if backward
        [| for n in 0 .. N-1 do
             let theta = s * 2.0 * Math.PI * float freqIndex * float n / float N
             yield Complex(cos theta, sin theta)
        |]

    // --------------------
    // The tests:
    // --------------------

    [<Fact>]
    member _.``SimpleFft forward->backward recovers original list`` () =
        let input = [ Complex(1.0, 0.0)
                      Complex(0.0, 2.0)
                      Complex(-1.0, 1.0)
                      Complex(3.0, -1.0) ]
        let forward = SimpleFourierTransform.fft ForwardTransform input |> List.ofArray
        let backward = SimpleFourierTransform.fft BackwardTransform forward
        Assert.True(areClose 1e-12 input backward, "Forward->Backward did not recover original list")

    [<Fact>]
    member _.``SimpleFft backward->forward recovers original list`` () =
        let input = [ Complex(2.0, 0.0)
                      Complex(1.0, -1.0)
                      Complex(0.0, 3.0)
                      Complex(-2.0, 2.0) ]
        let backward = SimpleFourierTransform.fft BackwardTransform input |> List.ofArray
        let forward = SimpleFourierTransform.fft ForwardTransform backward
        Assert.True(areClose 1e-12 input forward, "Backward->Forward did not recover original list")

    [<Fact>]
    member _.``SimpleFft delta impulse produces constant frequency domain`` () =
        let input = [ Complex(1.0, 0.0)
                      Complex(0.0, 0.0)
                      Complex(0.0, 0.0)
                      Complex(0.0, 0.0) ]
        let output = SimpleFourierTransform.fft ForwardTransform input
        let expected = [ Complex(0.5, 0.0)
                         Complex(0.5, 0.0)
                         Complex(0.5, 0.0)
                         Complex(0.5, 0.0) ]
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
        let expected = [| Complex(0.5, 0.0)
                          Complex(0.5, 0.0)
                          Complex(0.5, 0.0)
                          Complex(0.5, 0.0) |]
        Assert.True(areClose 1e-12 expected output, "Delta impulse did not become constant")

    // --------------------
    // Additional tests verifying that ForwardTransform uses a negative exponent
    // and BackwardTransform uses a positive exponent, under symmetric normalization.
    // We confirm that a pure sinusoid e^(± i*2π freqIndex * n / N) yields a single spike
    // at bin = freqIndex for the "matching" direction, and yields a spike in bin = N - freqIndex
    // (or near zero in that bin) for the "mismatched" direction.
    // --------------------


    [<Fact>]
    member _.``Forward transform of e^(-i 2π freqIndex n/N) => single spike at freqIndex`` () =
        // We create a sinusoid that matches ForwardTransform's exponent convention (negative).
        // Then do forward FFT -> we should see one bin "freqIndex" with amplitude ~ sqrt(N).
        let N = 4
        let freqIndex = 1

        let input = makeSinusoid ForwardTransform freqIndex N
        let output = FourierTransform.fft ForwardTransform input

        // Because each transform is normalized by 1/sqrt(N), the amplitude from
        // summation of e^(-i * 2π freqIndex n / N) = N => final is sqrt(N).
        let magnitudes = output |> Array.map (fun c -> c.Magnitude)
        let maxVal = magnitudes |> Array.max
        let maxIdx = magnitudes |> Array.mapi (fun i x -> i, x) |> Array.maxBy snd |> fst

        Assert.Equal(freqIndex, maxIdx)
        let expected = sqrt (float N)
        Assert.InRange(maxVal, expected - 1e-12, expected + 1e-12)

        // Other bins ~ 0
        for i in 0 .. N-1 do
            if i <> freqIndex then
                Assert.InRange(magnitudes.[i], 0.0, 1e-12)

    [<Fact>]
    member _.``Backward transform of e^(+i 2π freqIndex n/N) => single spike at freqIndex`` () =
        // We create a sinusoid that matches BackwardTransform's exponent (positive).
        // Then do backward FFT -> we should see a spike at bin = freqIndex.
        let N = 4
        let freqIndex = 1

        let input = makeSinusoid BackwardTransform freqIndex N
        let output = FourierTransform.fft BackwardTransform input

        let magnitudes = output |> Array.map (fun c -> c.Magnitude)
        let maxVal = magnitudes |> Array.max
        let maxIdx = magnitudes |> Array.mapi (fun i x -> i, x) |> Array.maxBy snd |> fst

        Assert.Equal(freqIndex, maxIdx)
        let expected = sqrt (float N)
        Assert.InRange(maxVal, expected - 1e-12, expected + 1e-12)

        for i in 0 .. N-1 do
            if i <> freqIndex then
                Assert.InRange(magnitudes.[i], 0.0, 1e-12)

    [<Fact>]
    member _.``Forward transform of e^(+i) sinusoid => spike at bin = N - freqIndex`` () =
        // This test shows that if we feed the "wrong" sign to forward transform,
        // we don't get a spike at 'freqIndex' but instead at (N - freqIndex).
        let N = 4
        let freqIndex = 1

        // e^(+i * 2π freqIndex n / N) => a "backward" sinusoid
        // but we feed it to the forward transform => effectively it acts like
        // e^(-i * 2π (N-freqIndex) n / N).
        let input = makeSinusoid BackwardTransform freqIndex N
        let output = FourierTransform.fft ForwardTransform input

        let magnitudes = output |> Array.map (fun c -> c.Magnitude)
        let maxVal = magnitudes |> Array.max
        let maxIdx = magnitudes |> Array.mapi (fun i x -> i, x) |> Array.maxBy snd |> fst

        // We expect the max bin to be N - freqIndex = 3 (since freqIndex = 1).
        Assert.Equal(N - freqIndex, maxIdx)
        let expected = sqrt (float N)
        Assert.InRange(maxVal, expected - 1e-12, expected + 1e-12)

        // Meanwhile bin 'freqIndex' is near zero
        Assert.InRange(magnitudes.[freqIndex], 0.0, 1e-12)

    [<Fact>]
    member _.``Backward transform of e^(-i) sinusoid => spike at bin = N - freqIndex`` () =
        // Similarly, if we feed the backward transform a "forward" sinusoid,
        // we see the spike appear at bin = (N - freqIndex).
        let N = 4
        let freqIndex = 1

        let input = makeSinusoid ForwardTransform freqIndex N // negative exponent
        let output = FourierTransform.fft BackwardTransform input

        let magnitudes = output |> Array.map (fun c -> c.Magnitude)
        let maxVal = magnitudes |> Array.max
        let maxIdx = magnitudes |> Array.mapi (fun i x -> i, x) |> Array.maxBy snd |> fst

        Assert.Equal(N - freqIndex, maxIdx)
        let expected = sqrt (float N)
        Assert.InRange(maxVal, expected - 1e-12, expected + 1e-12)

        Assert.InRange(magnitudes.[freqIndex], 0.0, 1e-12)
