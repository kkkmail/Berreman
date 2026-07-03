namespace OpticalConstructor.Tests

open System
open Xunit
open Berreman.Constants
open Berreman.Geometry
open Berreman.Fields
open Berreman.Media
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.Propagation

/// Spec 0027 (024) Phase 3/4 — the pure MM/SV propagation pipeline. The headline proof is Malus' law:
/// a rotating ideal LP analyzer after an ideal LP input (identity sample) traces I = I₀ cos²θ, peaking
/// when aligned and ~0 when crossed. Plus the ideal-polarizer Stokes/Mueller constants, the engine
/// sample-MM sanity bound, and the ellipsometer Ψ/Δ round-trip.
module PropagationTests =

    let private close (a : float) (b : float) : bool = abs (a - b) <= 1.0e-7
    let private closeT (eps : float) (a : float) (b : float) : bool = abs (a - b) <= eps

    let private deg (d : float) : Angle = Angle.degree d

    // ============================ Stokes / Mueller constants ============================

    [<Fact>]
    let ``stokesComponents reads S0..S3 back in order`` () =
        let sv = StokesVector.create [ 1.0; 0.2; -0.3; 0.5 ]
        let s0v, s1, s2, s3 = stokesComponents sv
        Assert.True(close 1.0 s0v)
        Assert.True(close 0.2 s1)
        Assert.True(close -0.3 s2)
        Assert.True(close 0.5 s3)
        Assert.True(close 1.0 (s0 sv))

    [<Fact>]
    let ``inputStokes of an ideal linear polarizer at 0 and 45 degrees`` () =
        let s0v0, s1_0, s2_0, s3_0 = stokesComponents (inputStokes IdealLinear (deg 0.0))
        Assert.True(close 1.0 s0v0)
        Assert.True(close 1.0 s1_0)
        Assert.True(close 0.0 s2_0)
        Assert.True(close 0.0 s3_0)
        let _, s1_45, s2_45, s3_45 = stokesComponents (inputStokes IdealLinear (deg 45.0))
        Assert.True(close 0.0 s1_45)
        Assert.True(close 1.0 s2_45)
        Assert.True(close 0.0 s3_45)

    [<Fact>]
    let ``inputStokes of ideal circular polarizers is fully circularly polarized`` () =
        let _, _, _, s3l = stokesComponents (inputStokes IdealCircularLeft (deg 0.0))
        let _, _, _, s3r = stokesComponents (inputStokes IdealCircularRight (deg 0.0))
        Assert.True(close 1.0 s3l)
        Assert.True(close -1.0 s3r)

    [<Fact>]
    let ``an aligned ideal linear analyzer passes the full S0, a crossed one passes ~0`` () =
        let svIn = inputStokes IdealLinear (deg 0.0)
        // Aligned analyzer (0°) on an already-LP-at-0° input passes the FULL intensity 1.0 (the ½ factor
        // only halves UNPOLARIZED input; an aligned ideal analyzer transmits a parallel beam in full).
        let aligned = analyzerMueller IdealLinear (deg 0.0) * svIn
        Assert.True(closeT 1.0e-9 1.0 (s0 aligned))
        // Crossed analyzer (90°): S0 ≈ 0.
        let crossed = analyzerMueller IdealLinear (deg 90.0) * svIn
        Assert.True(closeT 1.0e-9 0.0 (s0 crossed))

    // ============================ the Malus proof ============================

    [<Fact>]
    let ``a rotating ideal analyzer after an ideal input traces Malus I = I0 cos^2 theta`` () =
        let svIn = inputStokes IdealLinear (deg 0.0)
        let curve = rotatingAnalyzerCurve svIn identityMueller IdealLinear 73
        // I(0) is the peak; every sample must match I(0)·cos²θ within tolerance.
        let i0 = curve.points |> List.head |> snd
        Assert.True(i0 > 0.0)
        for (d, i) in curve.points do
            let theta = d * Math.PI / 180.0
            let expected = i0 * (cos theta) ** 2.0
            Assert.True(closeT 1.0e-9 expected i, sprintf "at %g° expected %g got %g" d expected i)

    [<Fact>]
    let ``the Malus curve peaks when aligned and is ~0 when crossed`` () =
        let svIn = inputStokes IdealLinear (deg 0.0)
        let curve = rotatingAnalyzerCurve svIn identityMueller IdealLinear 73
        let at (d : float) : float =
            curve.points |> List.find (fun (a, _) -> close a d) |> snd
        let i0 = at 0.0
        // Aligned (0° / 180°) = peak; crossed (90° / 270°) ≈ 0.
        Assert.True(closeT 1.0e-9 i0 (at 180.0))
        Assert.True(closeT 1.0e-9 0.0 (at 90.0))
        Assert.True(closeT 1.0e-9 0.0 (at 270.0))
        Assert.True(at 0.0 > at 45.0)
        Assert.True(at 45.0 > at 90.0)

    [<Fact>]
    let ``propagate equals applying the analyzer to the sample-propagated vector`` () =
        let svIn = inputStokes IdealLinear (deg 30.0)
        let analyzer = analyzerMueller IdealLinear (deg 15.0)
        let direct = analyzer * (identityMueller * svIn)
        let viaPropagate = propagate svIn identityMueller analyzer
        let _, a1, a2, a3 = stokesComponents direct
        let _, b1, b2, b3 = stokesComponents viaPropagate
        Assert.True(close a1 b1)
        Assert.True(close a2 b2)
        Assert.True(close a3 b3)

    // ============================ the engine sample MM (real physics) ============================

    let private glassSample : Sample =
        {
            id = "sample-glass-1mm"
            name = "Glass plate (n=1.52, 1 mm)"
            materialId = "glass-1.52"
            thickness = Thickness.mm 1.0<mm>
            substrate = Library.Plate
            description = "Single transparent-glass plate, n = 1.52, thickness 1 mm, in vacuum."
        }

    [<Fact>]
    let ``the engine sample Mueller matrix transmits at most the input intensity`` () =
        let mm = sampleMuellerT glassSample (WaveLength.nm 600.0<nm>) IncidenceAngle.normal
        let svOut = mm * unpolarizedStokes
        let out = s0 svOut
        // A physical transmittance: 0 < S0 ≤ 1 for unit unpolarized input.
        Assert.True(out > 0.0, sprintf "transmitted S0 was not positive: %g" out)
        Assert.True(out <= 1.0 + 1.0e-9, sprintf "transmitted S0 exceeded the input: %g" out)

    // ============================ Phase 4 — ellipsometer Ψ/Δ ============================

    /// Build a fully-polarized Stokes vector for known (Ψ, Δ) under the readout's convention:
    ///   S = [1; −cos2Ψ; sin2Ψ cosΔ; −sin2Ψ sinΔ].
    let private stokesOfPsiDelta (psiDeg : float) (deltaDeg : float) : StokesVector =
        let twoPsi = 2.0 * psiDeg * Math.PI / 180.0
        let delta = deltaDeg * Math.PI / 180.0
        StokesVector.create [ 1.0; - cos twoPsi; sin twoPsi * cos delta; - sin twoPsi * sin delta ]

    [<Fact>]
    let ``psiDeltaOfStokes round-trips known Psi / Delta pairs`` () =
        let cases = [ 45.0, 0.0; 30.0, 90.0; 20.0, 180.0; 10.0, -45.0 ]
        for (psiDeg, deltaDeg) in cases do
            let pd = psiDeltaOfStokes (stokesOfPsiDelta psiDeg deltaDeg)
            Assert.True(closeT 1.0e-6 psiDeg pd.psi.degrees, sprintf "Ψ: expected %g got %g" psiDeg pd.psi.degrees)
            // Δ wraps to (−180, 180]; compare after normalizing the difference.
            let dwrap = ((pd.delta.degrees - deltaDeg + 540.0) % 360.0) - 180.0
            Assert.True(closeT 1.0e-6 0.0 dwrap, sprintf "Δ: expected %g got %g" deltaDeg pd.delta.degrees)

    [<Fact>]
    let ``ellipsometerReadout of a real sample is finite and in range`` () =
        let svIn = inputStokes IdealLinear (deg 45.0)
        let mm = sampleMuellerT glassSample (WaveLength.nm 600.0<nm>) IncidenceAngle.normal
        let pd = ellipsometerReadout (mm * svIn)
        Assert.False(Double.IsNaN pd.psi.degrees)
        Assert.False(Double.IsNaN pd.delta.degrees)
        Assert.InRange(pd.psi.degrees, 0.0, 90.0)
        Assert.InRange(pd.delta.degrees, -180.0, 180.0)

    [<Fact>]
    let ``the multilayer sample maps to a real 41-film stack`` () =
        let multilayer : Sample =
            {
                id = "sample-multilayer-qw"
                name = "Quarter-wave glass/vacuum multilayer (41 layers)"
                materialId = "glass-1.52"
                thickness = Thickness.nm 100.0<nm>
                substrate = Library.ThinFilm
                description = "41-layer quarter-wave stack."
            }
        let system = sampleToSystem multilayer (WaveLength.nm 600.0<nm>)
        Assert.Equal(41, List.length system.films)

    // ============================ Spec 0027 (026) Part 1 — curated samples ============================

    /// Every seeded `SampleItem` (so the test auto-covers new samples added to the Library).
    let private seededSamples : Sample list =
        Library.seedEntries
        |> List.choose (function SampleItem s -> Some s | _ -> None)

    /// The wavelength a given sample is exercised at (EUV samples live at ~10 nm; everything else 600 nm).
    let private runWaveLengthFor (s : Sample) : WaveLength =
        if s.id = "sample-euv-mosi" then WaveLength.nm 10.0<nm> else WaveLength.nm 600.0<nm>

    [<Fact>]
    let ``the curated Library seeds the expected new samples`` () =
        let ids = seededSamples |> List.map (fun s -> s.id) |> Set.ofList
        let expected =
            [
                "sample-glass-1mm"; "sample-glass-2mm"; "sample-glass-film-600"
                "sample-glass-vacuum"; "sample-glass-film-200"; "sample-multilayer-qw"
                "sample-euv-mosi"; "sample-uniaxial"; "sample-biaxial"
                "sample-active-crystal"; "sample-langasite-silicon"
            ]
        for id in expected do
            Assert.True(Set.contains id ids, sprintf "missing seeded sample %s" id)

    [<Fact>]
    let ``every seeded sample maps to a finite, energy-conserving sample Mueller matrix`` () =
        for s in seededSamples do
            let w = runWaveLengthFor s
            let mm = sampleMuellerT s w IncidenceAngle.normal
            let out = s0 (mm * unpolarizedStokes)
            Assert.False(System.Double.IsNaN out, sprintf "%s produced NaN S0" s.id)
            Assert.False(System.Double.IsInfinity out, sprintf "%s produced infinite S0" s.id)
            Assert.True(out >= -1.0e-9, sprintf "%s transmitted a negative S0: %g" s.id out)
            Assert.True(out <= 1.0 + 1.0e-9, sprintf "%s transmitted S0 > input: %g" s.id out)

    [<Fact>]
    let ``sampleToSystem is total for every seeded sample with the expected layer count`` () =
        for s in seededSamples do
            let w = runWaveLengthFor s
            let system = sampleToSystem s w
            match s.id with
            | "sample-multilayer-qw" -> Assert.Equal(41, List.length system.films)
            | "sample-euv-mosi" -> Assert.Equal(200, List.length system.films)
            | "sample-active-crystal"
            | "sample-glass-vacuum"
            | "sample-glass-1mm"
            | "sample-glass-2mm" ->
                Assert.Empty system.films
                Assert.True(Option.isSome system.substrate, sprintf "%s should be a substrate plate" s.id)
            | _ ->
                // The remaining seeded samples are single-film systems.
                Assert.Equal(1, List.length system.films)

    [<Fact>]
    let ``every seeded sample carries a non-empty description`` () =
        for s in seededSamples do
            Assert.False(System.String.IsNullOrWhiteSpace s.description, sprintf "%s has an empty description" s.id)

    [<Fact>]
    let ``the dispersive langasite sample evaluates differently at different wavelengths`` () =
        let langasite = seededSamples |> List.find (fun s -> s.id = "sample-langasite-silicon")
        // Silicon's dispersion makes the transmitted intensity wavelength-dependent; the dispersive seam
        // (getSystem w v) is therefore actually being evaluated at the run wavelength.
        let i400 = s0 (sampleMuellerT langasite (WaveLength.nm 400.0<nm>) IncidenceAngle.normal * unpolarizedStokes)
        let i800 = s0 (sampleMuellerT langasite (WaveLength.nm 800.0<nm>) IncidenceAngle.normal * unpolarizedStokes)
        Assert.False(Double.IsNaN i400)
        Assert.False(Double.IsNaN i800)
        Assert.True(abs (i400 - i800) > 1.0e-9, sprintf "dispersion not observed: I(400)=%g I(800)=%g" i400 i800)

    // ============================ Spec 0027 (026) Part 2 — R2 / λ sweeps ============================

    open OpticalConstructor.Domain.Experiments

    let private linear45 : StokesVector = inputStokes IdealLinear (deg 45.0)

    [<Fact>]
    let ``r2SweepCurve returns n points whose x runs 0..89 monotone with finite y`` () =
        let n = 31
        let curve = r2SweepCurve linear45 glassSample (WaveLength.nm 600.0<nm>) None n
        Assert.Equal(n, List.length curve)
        let xs = curve |> List.map fst
        Assert.True(close 0.0 (List.head xs))
        Assert.True(close r2SweepMaxDegrees (List.last xs), sprintf "last x = %g, expected 89" (List.last xs))
        Assert.True(close 89.0 (List.last xs))
        Assert.True((xs = List.sort xs), "incidence x-values must be sorted ascending")
        for (_, y) in curve do
            Assert.False(Double.IsNaN y)
            Assert.False(Double.IsInfinity y)

    [<Fact>]
    let ``intensityThroughAnalyzerOpt None equals S0 of the raw sample output`` () =
        let mm = sampleMuellerT glassSample (WaveLength.nm 600.0<nm>) IncidenceAngle.normal
        let withNone = intensityThroughAnalyzerOpt linear45 mm None
        let direct = s0 (mm * linear45)
        Assert.True(close direct withNone)

    [<Fact>]
    let ``intensityThroughAnalyzerOpt Some passes through the analyzer Mueller matrix`` () =
        let mm = sampleMuellerT glassSample (WaveLength.nm 600.0<nm>) IncidenceAngle.normal
        let analyzer = Some (IdealLinear, deg 90.0)
        let withAnalyzer = intensityThroughAnalyzerOpt linear45 mm analyzer
        let expected = intensity (propagate linear45 mm (analyzerMueller IdealLinear (deg 90.0)))
        Assert.True(close expected withAnalyzer)

    [<Fact>]
    let ``waveLengthSweepIntensity spans the chosen range with finite y`` () =
        let n = 21
        let curve = waveLengthSweepIntensity linear45 glassSample IncidenceAngle.normal None 200.0 800.0 n
        Assert.Equal(n, List.length curve)
        let xs = curve |> List.map fst
        Assert.True(close 200.0 (List.head xs))
        Assert.True(close 800.0 (List.last xs))
        for (_, y) in curve do
            Assert.False(Double.IsNaN y)

    [<Fact>]
    let ``r2SweepPsiDelta returns two equal-length Psi / Delta curves`` () =
        let n = 19
        let psi, delta = r2SweepPsiDelta linear45 glassSample (WaveLength.nm 600.0<nm>) n
        Assert.Equal(n, List.length psi)
        Assert.Equal(List.length psi, List.length delta)
        Assert.True(close 89.0 (List.last (psi |> List.map fst)))

    [<Fact>]
    let ``waveLengthSweepPsiDelta returns two equal-length Psi / Delta curves over the range`` () =
        let n = 17
        let psi, delta = waveLengthSweepPsiDelta linear45 glassSample IncidenceAngle.normal 300.0 700.0 n
        Assert.Equal(n, List.length psi)
        Assert.Equal(List.length psi, List.length delta)
        Assert.True(close 300.0 (List.head (psi |> List.map fst)))
        Assert.True(close 700.0 (List.last (delta |> List.map fst)))

    [<Fact>]
    let ``the new Experiment cases expose their swept element and description`` () =
        let id = ElementId.create "swept"
        let r2 = SweepR2 id
        let lam = SweepWaveLength id
        Assert.Equal<ElementId>(id, r2.sweptElement)
        Assert.Equal<ElementId>(id, lam.sweptElement)
        Assert.Contains("R2", r2.description)
        Assert.Contains("wavelength", lam.description)
