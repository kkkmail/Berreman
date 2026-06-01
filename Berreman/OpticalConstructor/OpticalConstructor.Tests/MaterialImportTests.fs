namespace OpticalConstructor.Tests

open System.IO
open System.Numerics
open Berreman.Constants
open Berreman.MathNetNumericsMath
open Berreman.Fields
open Berreman.MaterialProperties
open Berreman.Dispersion
open Analytics.Variables
open OpticalConstructor.Domain.Units
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Storage.MaterialImport
open Xunit

/// refractiveindex.info YAML / CSV import + CSV export (§D.9, AC-D3). Proves λ is
/// reduced to the engine meter base on import (µm YAML, nm CSV), the resulting
/// `getEps` reproduces the tabulated n,k, tabulated entries interpolate linearly,
/// and `exportCsv` round-trips through `importCsv`.
module MaterialImportTests =

    let private fixtures = Path.Combine(System.AppContext.BaseDirectory, "fixtures")
    let private readFixture name = File.ReadAllText(Path.Combine(fixtures, name))

    let private closeC (tol : float) (a : Complex) (b : Complex) =
        abs (a.Real - b.Real) <= tol && abs (a.Imaginary - b.Imaginary) <= tol

    let private epsAt (entry : MaterialEntry) (w : WaveLength) = (entry.properties.epsWithDisp.getEps w).[0, 0]

    let private nkSquared n k =
        (Eps.fromComplexRefractionIndex (ComplexRefractionIndex(createComplex n k))).[0, 0]

    [<Fact>]
    let ``AC-D3 refractiveindex.info µm YAML import reduces λ to meters and reproduces n,k`` () =
        match importRefractiveIndexInfo (readFixture "Si.yml") with
        | Error e -> Assert.Fail($"{e}")
        | Ok entry ->
            // 0.4 µm is a tabulated sample (n = 5.570, k = 0.387).
            let got = epsAt entry (toWaveLength Micrometer 0.4)
            Assert.True(closeC 1e-9 got (nkSquared 5.570 0.387), $"got {got}")

    [<Fact>]
    let ``nm CSV import reduces λ to meters and reproduces n,k`` () =
        match importCsv (readFixture "sample-nk.csv") with
        | Error e -> Assert.Fail($"{e}")
        | Ok entry ->
            // 500 nm is a tabulated sample (n = 1.480, k = 0.005).
            let got = epsAt entry (toWaveLength Nanometer 500.0)
            Assert.True(closeC 1e-9 got (nkSquared 1.480 0.005), $"got {got}")

    [<Fact>]
    let ``tabulated import interpolates linearly between samples`` () =
        match importRefractiveIndexInfo (readFixture "Si.yml") with
        | Error e -> Assert.Fail($"{e}")
        | Ok entry ->
            // 0.55 µm is the midpoint of 0.5 (4.293, 0.045) and 0.6 (3.939, 0.025).
            let got = epsAt entry (toWaveLength Micrometer 0.55)
            Assert.True(closeC 1e-9 got (nkSquared 4.116 0.035), $"got {got}")

    [<Fact>]
    let ``exportCsv samples n,k over the range and round-trips through importCsv`` () =
        let entry =
            match importCsv (readFixture "sample-nk.csv") with
            | Ok e -> e
            | Error e -> failwith (string e)
        let range : Range<WaveLength> =
            { startValue = WaveLength.nm 450.0<nm>; endValue = WaveLength.nm 650.0<nm>; numberOfPoints = 4 }
        let csv = exportCsv entry range
        Assert.Contains("wavelength_nm,n,k", csv)
        match importCsv csv with
        | Ok back ->
            let w = toWaveLength Nanometer 500.0 // a shared sample of both tables
            Assert.True(closeC 1e-6 (epsAt entry w) (epsAt back w), "round-trip n,k at 500 nm")
        | Error e -> Assert.Fail($"{e}")
