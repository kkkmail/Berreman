namespace BerremanTests
open System
open System.Numerics
open Berreman
//open Berreman.FourierTransform
open Xunit
open Xunit.Abstractions
open FsCheck


///// See: https://gist.github.com/mrange/da57f972b3dfdfb44f28fd340841586c
//module FourierTransformTests =
//    let x = 1


//module Tests =

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
      let e = FourierTransform.dft vs
      let a = FourierTransform.fft vs
      if e.Length = a.Length then
        let rec loop s i =
          if i < e.Length then
            let d = e.[i] - a.[i]
            loop (s + d*d) (i + 1)
          else
            s
        let s = loop Complex.Zero 0
        let r = s.Magnitude / float e.Length < 1E-10
        if not r then printfn $"E:%A{e}\nA:%A{a}"
        r
      else
        false


type FourierTransformTests(output : ITestOutputHelper) =

    let writeLine s = output.WriteLine s

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
        let inline cc i       = System.GC.CollectionCount i

        let v                 = action ()

        System.GC.Collect (2, System.GCCollectionMode.Forced, true)

        let bcc0, bcc1, bcc2  = cc 0, cc 1, cc 2
        let b                 = now ()

        for i in 1..repeat do
          action () |> ignore

        let e = now ()
        let ecc0, ecc1, ecc2  = cc 0, cc 1, cc 2

        v, (e - b), ecc0 - bcc0, ecc1 - bcc1, ecc2 - bcc2

    [<Fact>]
    member _.runTest() : unit =
//[<EntryPoint>]
//let main argv =
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
          |> SimpleFourierTransform.fft
          |> List.map trim

        let output2 =
          input
          |> FourierTransform.dft
          |> Array.map trim

        let output3 =
          input
          |> FourierTransform.fft
          |> Array.map trim

        if l < 64 then
          writeLine $"Input      : %A{input}"
          writeLine $"Simple FFT : %A{output1}"
          writeLine $"Faster DFT : %A{output2}"
          writeLine $"Faster FFT : %A{output3}"

        let testCases =
          [|
            "Simple FFT"  , fun () -> SimpleFourierTransform.fft inputList        |> ignore
        //      "Faster DFT"  , fun () -> FourierTransform.dft input  |> ignore
            "Faster FFT"  , fun () -> FourierTransform.fft input  |> ignore
          |]

        for name, action in testCases do
          writeLine $"Running %s{name} %d{repeat} times ..."
          let _, ms, cc0, cc1, cc2 = time repeat action
          writeLine $"  it took %d{ms} ms and (%d{cc0}, %d{cc1}, %d{cc2}) CC"

        ()
