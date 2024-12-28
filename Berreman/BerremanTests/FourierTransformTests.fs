namespace BerremanTests
open System
open System.Numerics
open Berreman
open Berreman.FourierTransformPrimitives
open Xunit
open Xunit.Abstractions
open FsCheck

type Samples = Samples of Complex []


type FftGenerators =
    static member Complex () : Arbitrary<Complex> =
        let f =
            Arb.generate<float>
            |> Gen.map (fun v ->
                if Double.IsNaN v then
                    0.
                elif Double.IsPositiveInfinity v then
                    1.
                elif Double.IsNegativeInfinity v then
                    -1.
                else
                v % 1000.
            )
        let c =
            Gen.constant (fun r i -> Complex (r, i))
            <*> f
            <*> f
        Arb.fromGen c

    static member Samples () : Arbitrary<Samples> =
        let s =
            gen {
                let! n = Gen.choose (1, 3) |> Gen.map (fun i -> 1 <<< i)
                let! c = Gen.arrayOfLength n Arb.generate<Complex>
                return Samples c
            }
        Arb.fromGen s


type FftTests =
    static member ``Test ilog2`` (i : int) =
        let e = (abs i) % 29 + 2
        let v = 1 <<< e
        let a = FourierTransform.Details.ilog2 v
        e = a
    static member ``Simple test`` (s : Samples) =
        let (Samples vs) = s
        let e = FourierTransform.dft ForwardTransform vs
        let a = FourierTransform.fft ForwardTransform vs
        if e.Length = a.Length then
            let rec loop s i =
                if i < e.Length then
                    let d = e[i] - a[i]
                    loop (s + d*d) (i + 1)
                else s
            let s = loop Complex.Zero 0
            let r = s.Magnitude / float e.Length < 1E-10
            if not r then printfn $"E:%A{e}\nA:%A{a}"
            r
        else false

type FourierTransformTests(output : ITestOutputHelper) =

    let runTests () =
        let config =
            { Config.Quick with
                Arbitrary = typeof<FftGenerators> :: Config.Quick.Arbitrary
                MaxTest   = 1000
                MaxFail   = 1000
            }

        Check.All<FftTests> config


    // now () returns current time in milliseconds since start
    let now : unit -> int64 =
        let sw = System.Diagnostics.Stopwatch ()
        sw.Start ()
        fun () -> sw.ElapsedMilliseconds

    // time estimates the time 'action' repeated a number of times
    let time repeat action =
        let inline cc i       = GC.CollectionCount i

        let v                 = action ()

        GC.Collect (2, GCCollectionMode.Forced, true)

        let bcc0, bcc1, bcc2  = cc 0, cc 1, cc 2
        let b                 = now ()

        for i in 1..repeat do
          action () |> ignore

        let e = now ()
        let ecc0, ecc1, ecc2  = cc 0, cc 1, cc 2

        v, (e - b), ecc0 - bcc0, ecc1 - bcc1, ecc2 - bcc2


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
                        let e = arrE[i]
                        let a = arrA[i]
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
                for idx, e, a, reDiff, imDiff in differences do
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
    /// Note the (-) in "let s = - direction.sign". This is to pick up the necessary frequency.
    /// Creates a single-frequency sinusoid array of length N,
    /// x[n] = exp(i * direction.sign * 2π * freqIndex * n / N).
    /// ForwardTransform => direction.sign = -1.0 => negative exponent
    /// BackwardTransform => direction.sign = +1.0 => positive exponent
    /// </summary>
    let makeSinusoid (direction: FourierTransformDirection) freqIndex N =
        let s = - direction.sign  // -1.0 if forward, +1.0 if backward
        [| for n in 0 .. N-1 do
             let theta = s * 2.0 * Math.PI * float freqIndex * float n / float N
             yield Complex(cos theta, sin theta)
        |]


    [<Fact>]
    member _.runTest() : unit =
        let trim (c : Complex) =
          let trim (f : float) = if abs f < 1E-14 then 0. else f
          Complex (trim c.Real, trim c.Imaginary)

        runTests ()

        let repeat  = 1000
        let l       = 1024

        let input   =
            [| for x in 0..(l - 1) ->
                let a = FourierTransform.tau*float x/float l
                let v = cos a + cos (2.*a)
                let c = Complex (v / (float l / 2.), 0.) |> trim
                c
            |]

        let inputList = input |> List.ofArray

        let output1 =
            inputList
            |> SimpleFourierTransform.fft ForwardTransform
            |> Array.map trim

        let output2 =
            input
            |> FourierTransform.dft ForwardTransform
            |> Array.map trim

        let output3 =
            input
            |> FourierTransform.fft ForwardTransform
            |> Array.map trim

        if l < 64 then
            writeLine $"Input      : %A{input}"
            writeLine $"Simple FFT : %A{output1}"
            writeLine $"Faster DFT : %A{output2}"
            writeLine $"Faster FFT : %A{output3}"

        let testCases =
            [|
                "Simple FFT"  , fun () -> SimpleFourierTransform.fft ForwardTransform  inputList        |> ignore
                // "Faster DFT"  , fun () -> FourierTransform.dft input  |> ignore
                "Faster FFT"  , fun () -> FourierTransform.fft ForwardTransform  input  |> ignore
            |]

        for name, action in testCases do
            writeLine $"Running %s{name} %d{repeat} times ..."
            let _, ms, cc0, cc1, cc2 = time repeat action
            writeLine $"  it took %d{ms} ms and (%d{cc0}, %d{cc1}, %d{cc2}) CC"

        ()


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
                Assert.InRange(magnitudes[i], 0.0, 1e-12)

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
                Assert.InRange(magnitudes[i], 0.0, 1e-12)

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
        Assert.InRange(magnitudes[freqIndex], 0.0, 1e-12)

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

        Assert.InRange(magnitudes[freqIndex], 0.0, 1e-12)
