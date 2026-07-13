namespace OpticalConstructor.Tests

open System
open Xunit
open Berreman.Constants
open Berreman.Geometry
open Berreman.Fields
open Berreman.Media
open Berreman.MaterialProperties
open OpticalProperties.Standard
open OpticalProperties.Active
open OpticalProperties.Dispersive
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.Lifecycle       // VersionsInUse.empty (spec 0038 step 022)
open OpticalConstructor.Domain.MaterialStore   // MaterialProxy.createInMemory (versioned, spec 0038 step 021)
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
            Assert.True(closeT 1.0e-9 expected i, $"at %g{d}° expected %g{expected} got %g{i}")

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
            id = SampleId.create ()
            name = "Glass plate (n=1.52, 1 mm)"
            structure =
                {
                    films = []
                    substrate = Some { materialId = MaterialLibrary.MaterialVersionId.firstOf MaterialLibrary.MaterialIds.glass152; thickness = Thickness.mm 1.0<mm>; orientation = PrimaryAxes }
                    lower = None
                }
            substrate = Library.Plate
            description = "Single transparent-glass plate, n = 1.52, thickness 1 mm, in vacuum."
        }

    /// The versioned material store over the standard built-ins (spec 0038 step 022): the sample
    /// resolver now takes a `MaterialProxy` and resolves each layer's pinned `MaterialVersionId`
    /// through the store's by-version `resolveVersion`. The seeds pin version one, which this store
    /// seeds active, so every known built-in resolves.
    let private standardProxy : MaterialLibrary.MaterialProxy =
        MaterialLibrary.MaterialProxy.createInMemory (fun _ -> []) VersionsInUse.empty

    /// Resolve a sample against the standard material store, failing the test on a typed error (every
    /// sample used by these tests references known built-in ids).
    let private resolveOrFail (s : Sample) : ResolvedSample =
        match resolveSampleMaterials standardProxy s with
        | Ok r -> r
        | Error e -> failwith ($"sample %s{s.name} did not resolve: %A{e}")

    let private glassResolved : ResolvedSample = resolveOrFail glassSample

    [<Fact>]
    let ``the engine sample Mueller matrix transmits at most the input intensity`` () =
        let mm = sampleMuellerT glassResolved (WaveLength.nm 600.0<nm>) IncidenceAngle.normal
        let svOut = mm * unpolarizedStokes
        let out = s0 svOut
        // A physical transmittance: 0 < S0 ≤ 1 for unit unpolarized input.
        Assert.True(out > 0.0, $"transmitted S0 was not positive: %g{out}")
        Assert.True(out <= 1.0 + 1.0e-9, $"transmitted S0 exceeded the input: %g{out}")

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
            Assert.True(closeT 1.0e-6 psiDeg pd.psi.degrees, $"Ψ: expected %g{psiDeg} got %g{pd.psi.degrees}")
            // Δ wraps to (−180, 180]; compare after normalizing the difference.
            let dwrap = ((pd.delta.degrees - deltaDeg + 540.0) % 360.0) - 180.0
            Assert.True(closeT 1.0e-6 0.0 dwrap, $"Δ: expected %g{deltaDeg} got %g{pd.delta.degrees}")

    [<Fact>]
    let ``ellipsometerReadout of a real sample is finite and in range`` () =
        let svIn = inputStokes IdealLinear (deg 45.0)
        let mm = sampleMuellerT glassResolved (WaveLength.nm 600.0<nm>) IncidenceAngle.normal
        let pd = ellipsometerReadout (mm * svIn)
        Assert.False(Double.IsNaN pd.psi.degrees)
        Assert.False(Double.IsNaN pd.delta.degrees)
        Assert.InRange(pd.psi.degrees, 0.0, 90.0)
        Assert.InRange(pd.delta.degrees, -180.0, 180.0)

    [<Fact>]
    let ``the multilayer sample maps to a real 41-film stack`` () =
        let system = sampleToSystem (resolveOrFail SeedSamples.multilayerQw) (WaveLength.nm 600.0<nm>)
        Assert.Equal(41, List.length system.films)

    // ============================ Spec 0027 (026) Part 1 — curated samples ============================

    /// Every seeded `SampleItem` (so the test auto-covers new samples added to the Library).
    let private seededSamples : Sample list =
        Library.seedEntries
        |> List.choose (function SampleItem s -> Some s | _ -> None)

    /// The wavelength a given sample is exercised at (EUV samples live at ~10 nm; everything else 600 nm).
    let private runWaveLengthFor (s : Sample) : WaveLength =
        if s.id = SeedSamples.euvMoSi.id then WaveLength.nm 10.0<nm> else WaveLength.nm 600.0<nm>

    [<Fact>]
    let ``the curated Library seeds exactly the named SeedSamples`` () =
        // The samples in `seedEntries` ARE the named `SeedSamples` values, in order (spec 0033
        // step 002 — the id literals live only in `SeedSamples`).
        Assert.Equal<SampleId list>(
            SeedSamples.all |> List.map (fun s -> s.id),
            seededSamples |> List.map (fun s -> s.id))

    [<Fact>]
    let ``every seeded sample maps to a finite, energy-conserving sample Mueller matrix`` () =
        for s in seededSamples do
            let w = runWaveLengthFor s
            let mm = sampleMuellerT (resolveOrFail s) w IncidenceAngle.normal
            let out = s0 (mm * unpolarizedStokes)
            Assert.False(System.Double.IsNaN out, $"%s{s.name} produced NaN S0")
            Assert.False(System.Double.IsInfinity out, $"%s{s.name} produced infinite S0")
            Assert.True(out >= -1.0e-9, $"%s{s.name} transmitted a negative S0: %g{out}")
            Assert.True(out <= 1.0 + 1.0e-9, $"%s{s.name} transmitted S0 > input: %g{out}")

    [<Fact>]
    let ``sampleToSystem is total for every seeded sample with the expected layer count`` () =
        let plateIds =
            [ SeedSamples.activeCrystal.id; SeedSamples.glassVacuum.id; SeedSamples.glassPlate1mm.id; SeedSamples.glassPlate2mm.id ]
        for s in seededSamples do
            let w = runWaveLengthFor s
            let system = sampleToSystem (resolveOrFail s) w
            if s.id = SeedSamples.multilayerQw.id then Assert.Equal(41, List.length system.films)
            elif s.id = SeedSamples.euvMoSi.id then Assert.Equal(200, List.length system.films)
            elif plateIds |> List.contains s.id then
                Assert.Empty system.films
                Assert.True(Option.isSome system.substrate, $"%s{s.name} should be a substrate plate")
            else
                // The remaining seeded samples are single-film systems.
                Assert.Equal(1, List.length system.films)

    [<Fact>]
    let ``every seeded sample carries a non-empty description`` () =
        for s in seededSamples do
            Assert.False(System.String.IsNullOrWhiteSpace s.description, $"%s{s.name} has an empty description")

    [<Fact>]
    let ``the dispersive langasite sample evaluates differently at different wavelengths`` () =
        let langasite = resolveOrFail SeedSamples.langasiteSilicon
        // Silicon's dispersion makes the transmitted intensity wavelength-dependent; the dispersive seam
        // (getProperties w) is therefore actually being evaluated at the run wavelength.
        let i400 = s0 (sampleMuellerT langasite (WaveLength.nm 400.0<nm>) IncidenceAngle.normal * unpolarizedStokes)
        let i800 = s0 (sampleMuellerT langasite (WaveLength.nm 800.0<nm>) IncidenceAngle.normal * unpolarizedStokes)
        Assert.False(Double.IsNaN i400)
        Assert.False(Double.IsNaN i800)
        Assert.True(abs (i400 - i800) > 1.0e-9, $"dispersion not observed: I(400)=%g{i400} I(800)=%g{i800}")

    // ============================ Spec 0027 (026) Part 2 — R2 / λ sweeps ============================

    open OpticalConstructor.Domain.Experiments

    let private linear45 : StokesVector = inputStokes IdealLinear (deg 45.0)

    [<Fact>]
    let ``r2SweepCurve returns n points whose x runs 0..89 monotone with finite y`` () =
        let n = 31
        let curve = r2SweepCurve BranchTransmitted linear45 glassResolved (WaveLength.nm 600.0<nm>) None 0.0 r2SweepMaxDegrees n
        Assert.Equal(n, List.length curve)
        let xs = curve |> List.map fst
        Assert.True(close 0.0 (List.head xs))
        Assert.True(close r2SweepMaxDegrees (List.last xs), $"last x = %g{(List.last xs)}, expected 89")
        Assert.True(close 89.0 (List.last xs))
        Assert.True((xs = List.sort xs), "incidence x-values must be sorted ascending")
        for (_, y) in curve do
            Assert.False(Double.IsNaN y)
            Assert.False(Double.IsInfinity y)

    [<Fact>]
    let ``intensityThroughAnalyzerOpt None equals S0 of the raw sample output`` () =
        let mm = sampleMuellerT glassResolved (WaveLength.nm 600.0<nm>) IncidenceAngle.normal
        let withNone = intensityThroughAnalyzerOpt linear45 mm None
        let direct = s0 (mm * linear45)
        Assert.True(close direct withNone)

    [<Fact>]
    let ``intensityThroughAnalyzerOpt Some passes through the analyzer Mueller matrix`` () =
        let mm = sampleMuellerT glassResolved (WaveLength.nm 600.0<nm>) IncidenceAngle.normal
        let analyzer = Some (IdealLinear, deg 90.0)
        let withAnalyzer = intensityThroughAnalyzerOpt linear45 mm analyzer
        let expected = intensity (propagate linear45 mm (analyzerMueller IdealLinear (deg 90.0)))
        Assert.True(close expected withAnalyzer)

    [<Fact>]
    let ``waveLengthSweepIntensity spans the chosen range with finite y`` () =
        let n = 21
        let curve = waveLengthSweepIntensity BranchTransmitted linear45 glassResolved IncidenceAngle.normal None 200.0 800.0 n
        Assert.Equal(n, List.length curve)
        let xs = curve |> List.map fst
        Assert.True(close 200.0 (List.head xs))
        Assert.True(close 800.0 (List.last xs))
        for (_, y) in curve do
            Assert.False(Double.IsNaN y)

    [<Fact>]
    let ``r2SweepPsiDelta returns two equal-length Psi / Delta curves`` () =
        let n = 19
        let psi, delta = r2SweepPsiDelta BranchTransmitted linear45 glassResolved (WaveLength.nm 600.0<nm>) 0.0 r2SweepMaxDegrees n
        Assert.Equal(n, List.length psi)
        Assert.Equal(List.length psi, List.length delta)
        Assert.True(close 89.0 (List.last (psi |> List.map fst)))

    [<Fact>]
    let ``waveLengthSweepPsiDelta returns two equal-length Psi / Delta curves over the range`` () =
        let n = 17
        let psi, delta = waveLengthSweepPsiDelta BranchTransmitted linear45 glassResolved IncidenceAngle.normal 300.0 700.0 n
        Assert.Equal(n, List.length psi)
        Assert.Equal(List.length psi, List.length delta)
        Assert.True(close 300.0 (List.head (psi |> List.map fst)))
        Assert.True(close 700.0 (List.last (delta |> List.map fst)))

    // ============================ Spec 0027 (028) — T / R branches + range rotate ============================

    [<Fact>]
    let ``sampleMuellerR reflects a physical (0 < S0 <= 1) intensity`` () =
        let mm = sampleMuellerR glassResolved (WaveLength.nm 600.0<nm>) IncidenceAngle.normal
        let out = s0 (mm * unpolarizedStokes)
        Assert.False(Double.IsNaN out)
        Assert.True(out >= -1.0e-9, $"reflected S0 was negative: %g{out}")
        Assert.True(out <= 1.0 + 1.0e-9, $"reflected S0 exceeded the input: %g{out}")

    [<Fact>]
    let ``sampleMueller selects the transmitted vs reflected engine matrix by branch`` () =
        let w = WaveLength.nm 600.0<nm>
        let byBranchT = sampleMueller BranchTransmitted glassResolved w IncidenceAngle.normal
        let byBranchR = sampleMueller BranchReflected glassResolved w IncidenceAngle.normal
        let t = sampleMuellerT glassResolved w IncidenceAngle.normal
        let r = sampleMuellerR glassResolved w IncidenceAngle.normal
        // The branch selector agrees with the dedicated T / R helpers (same S0 for unpolarized input).
        Assert.True(close (s0 (byBranchT * unpolarizedStokes)) (s0 (t * unpolarizedStokes)))
        Assert.True(close (s0 (byBranchR * unpolarizedStokes)) (s0 (r * unpolarizedStokes)))

    [<Fact>]
    let ``rotatingAnalyzerCurveRange over a sub-range starts and ends at the chosen angles`` () =
        let svIn = inputStokes IdealLinear (deg 0.0)
        let curve = rotatingAnalyzerCurveRange svIn identityMueller IdealLinear 30.0 150.0 25
        let xs = curve.points |> List.map fst
        Assert.True(close 30.0 (List.head xs))
        Assert.True(close 150.0 (List.last xs))
        Assert.True((xs = List.sort xs), "rotate x-values must be sorted ascending")

    [<Fact>]
    let ``rotatingAnalyzerCurve is the 0..360 special case of the range rotate`` () =
        let svIn = inputStokes IdealLinear (deg 20.0)
        let full = rotatingAnalyzerCurve svIn identityMueller IdealLinear 37
        let range = rotatingAnalyzerCurveRange svIn identityMueller IdealLinear 0.0 360.0 37
        Assert.Equal(List.length full.points, List.length range.points)
        for ((xa, ya), (xb, yb)) in List.zip full.points range.points do
            Assert.True(close xa xb)
            Assert.True(close ya yb)

    // ============================ Spec 0033 (001) — structural stacks, typed resolution ============================

    [<Fact>]
    let ``resolveSampleMaterials returns a typed Error for an unknown film material id`` () =
        let missing = MaterialLibrary.newMaterialId ()
        let sample : Sample =
            { glassSample with
                id = SampleId.create ()
                structure =
                    {
                        films = [ SingleLayer { materialId = MaterialLibrary.MaterialVersionId.firstOf missing; thickness = Thickness.nm 100.0<nm>; orientation = PrimaryAxes } ]
                        substrate = None
                        lower = None
                    } }
        match resolveSampleMaterials standardProxy sample with
        | Error (MaterialLibrary.UnknownMaterialId reason) -> Assert.Contains(string missing.value, reason)
        | other -> Assert.Fail($"expected UnknownMaterialId, got %A{other}")

    [<Fact>]
    let ``resolveSampleMaterials returns a typed Error for an unknown lower half-space id`` () =
        let missing = MaterialLibrary.newMaterialId ()
        let sample : Sample =
            { glassSample with
                id = SampleId.create ()
                structure =
                    {
                        films = [ SingleLayer { materialId = MaterialLibrary.MaterialVersionId.firstOf MaterialLibrary.MaterialIds.glass152; thickness = Thickness.nm 100.0<nm>; orientation = PrimaryAxes } ]
                        substrate = None
                        lower = Some (MaterialLibrary.MaterialVersionId.firstOf missing)
                    } }
        match resolveSampleMaterials standardProxy sample with
        | Error (MaterialLibrary.UnknownMaterialId reason) -> Assert.Contains(string missing.value, reason)
        | other -> Assert.Fail($"expected UnknownMaterialId, got %A{other}")

    [<Fact>]
    let ``every seeded sample resolves against the standard material library`` () =
        for s in seededSamples do
            match resolveSampleMaterials standardProxy s with
            | Ok _ -> ()
            | Error e -> Assert.Fail($"%s{s.name} did not resolve: %A{e}")

    /// The 41-layer λ/4 films exactly as the PRE-0033 hand-built branch constructed them.
    let private legacyQwFilms : Layer list =
        let thickness1 = Thickness.nm ((600.0 / 1.52 / 4.0) * oneNanometer)
        let thickness2 = Thickness.nm ((600.0 / 1.00 / 4.0) * oneNanometer)
        let pairs =
            [ for _ in 1 .. 20 ->
                [ { properties = OpticalProperties.transparentGlass; thickness = thickness1 }
                  { properties = OpticalProperties.vacuum; thickness = thickness2 } ] ]
            |> List.concat
        pairs @ [ { properties = OpticalProperties.transparentGlass; thickness = thickness1 } ]

    /// The engine system the PRE-0033 `sampleToSystem` hand-built per sample id (replicated here
    /// verbatim, descriptions dropped) — the acceptance harness the structurally-built systems must
    /// match layer for layer, substrate for substrate, tensor for tensor.
    let private legacyExpectedSystem (s : Sample) (w : WaveLength) : OpticalSystem =
        let film (properties : OpticalProperties) (thickness : Thickness) : OpticalSystem =
            {
                description = None
                upper = OpticalProperties.vacuum
                films = [ { properties = properties; thickness = thickness } ]
                substrate = None
                lower = OpticalProperties.vacuum
            }
        let plate (properties : OpticalProperties) (thickness : Thickness) : OpticalSystem =
            {
                description = None
                upper = OpticalProperties.vacuum
                films = []
                substrate = Some (Substrate.Plate { properties = properties; thickness = thickness })
                lower = OpticalProperties.vacuum
            }
        if s.id = SeedSamples.glassPlate1mm.id then plate OpticalProperties.transparentGlass (Thickness.mm 1.0<mm>)
        elif s.id = SeedSamples.glassPlate2mm.id then plate OpticalProperties.transparentGlass (Thickness.mm 2.0<mm>)
        elif s.id = SeedSamples.glassFilm600.id then film OpticalProperties.transparentGlass175 (Thickness.nm 600.0<nm>)
        elif s.id = SeedSamples.glassVacuum.id then plate OpticalProperties.transparentGlass150 (Thickness.mm 1.0<mm>)
        elif s.id = SeedSamples.glassFilm200.id then film OpticalProperties.transparentGlass (Thickness.nm 200.0<nm>)
        elif s.id = SeedSamples.multilayerQw.id then
            {
                description = None
                upper = OpticalProperties.vacuum
                films = legacyQwFilms
                substrate = None
                lower = OpticalProperties.vacuum
            }
        elif s.id = SeedSamples.euvMoSi.id then
            let thickness = Thickness.nm (10.6 / 4.0 * oneNanometer)
            let films =
                [ { properties = OpticalProperties.euvMolybdenum; thickness = thickness }
                  { properties = OpticalProperties.euvSilicon; thickness = thickness } ]
                |> List.replicate 100
                |> List.concat
            {
                description = None
                upper = OpticalProperties.vacuum
                films = films
                substrate = None
                lower = OpticalProperties.vacuum
            }
        elif s.id = SeedSamples.uniaxial.id then film OpticalProperties.uniaxialCrystal (Thickness.nm 1000.0<nm>)
        elif s.id = SeedSamples.biaxial.id then film OpticalProperties.biaxialCrystal (Thickness.nm 1000.0<nm>)
        elif s.id = SeedSamples.activeCrystal.id then
            let e11 = RefractionIndex 2.315 |> EpsValue.fromRefractionIndex
            let e33 = RefractionIndex 2.226 |> EpsValue.fromRefractionIndex
            plate (OpticalProperties.planarCrystal e11 e33 (RhoValue 1.5e-6)) Thickness.oneCentiMeter
        elif s.id = SeedSamples.langasiteSilicon.id then
            {
                description = None
                upper = OpticalProperties.vacuum
                films = [ { properties = langasiteOpticalProperties.getProperties w; thickness = Thickness.mm 0.01<mm> } ]
                substrate = None
                lower = siliconOpticalProperties.getProperties w
            }
        else failwith ($"no legacy expectation for sample %s{s.name}")

    [<Fact>]
    let ``every seeded sample's structurally-built system equals the previously hand-built system`` () =
        for s in seededSamples do
            let w = runWaveLengthFor s
            let actual = sampleToSystem (resolveOrFail s) w
            Assert.Equal<OpticalSystem>(legacyExpectedSystem s w, { actual with description = None })

    // ============================ Spec 0033 (020) — crystal orientation ============================

    let private orientationRunW : WaveLength = WaveLength.nm 600.0<nm>

    /// A single uniaxial-crystal film sample at the given orientation (an ANISOTROPIC material, so a
    /// rotation is observable — an isotropic layer would rotate onto itself).
    let private uniaxialFilm (orientation : CrystalOrientation) : Sample =
        {
            id = SampleId.create ()
            name = "Uniaxial film (oriented)"
            structure =
                {
                    films = [ SingleLayer { materialId = MaterialLibrary.MaterialVersionId.firstOf MaterialLibrary.MaterialIds.uniaxialCrystal; thickness = Thickness.nm 1000.0<nm>; orientation = orientation } ]
                    substrate = None
                    lower = None
                }
            substrate = Library.ThinFilm
            description = "Uniaxial crystal thin film at the given crystal orientation."
        }

    /// A uniaxial-crystal PLATE sample (the substrate is a `SampleLayer` too, so it carries an
    /// orientation of its own).
    let private uniaxialPlate (orientation : CrystalOrientation) : Sample =
        {
            id = SampleId.create ()
            name = "Uniaxial plate (oriented)"
            structure =
                {
                    films = []
                    substrate = Some { materialId = MaterialLibrary.MaterialVersionId.firstOf MaterialLibrary.MaterialIds.uniaxialCrystal; thickness = Thickness.mm 1.0<mm>; orientation = orientation }
                    lower = None
                }
            substrate = Library.Plate
            description = "Uniaxial crystal plate at the given crystal orientation."
        }

    [<Fact>]
    let ``an EulerRotation layer's built system equals rotating the same layer directly`` () =
        // Spec 0033 step 020 acceptance: the system built from an EulerRotation-oriented layer MUST
        // equal applying `Layer.rotate` directly to the same (PrimaryAxes-built) layer.
        let phi, theta, psi = deg 30.0, deg 40.0, deg 50.0
        let built = sampleToSystem (resolveOrFail (uniaxialFilm (EulerRotation (ZmXpZm, phi, theta, psi)))) orientationRunW
        let unrotated = sampleToSystem (resolveOrFail (uniaxialFilm PrimaryAxes)) orientationRunW
        let rotation = Rotation.create ZmXpZm phi theta psi |> Rotation
        let expected = unrotated.films |> List.map (fun (l : Layer) -> l.rotate rotation)
        Assert.Equal<Layer list>(expected, built.films)
        // The rotation is non-trivial: the oriented tensors must actually differ from the stored ones.
        Assert.False((unrotated.films = built.films), "the Euler rotation left the uniaxial tensors unchanged")

    [<Fact>]
    let ``PrimaryAxes builds unrotated tensors`` () =
        // Spec 0033 step 020 acceptance: `PrimaryAxes` MUST leave the tensors exactly as stored —
        // the built film is the engine's own uniaxial-crystal properties, bit for bit.
        let system = sampleToSystem (resolveOrFail (uniaxialFilm PrimaryAxes)) orientationRunW
        Assert.Equal<Layer list>(
            [ { properties = OpticalProperties.uniaxialCrystal; thickness = Thickness.nm 1000.0<nm> } ],
            system.films)

    [<Fact>]
    let ``an EulerRotation spelling rotatePiX's angles equals the named shortcut applied directly`` () =
        // The named engine shortcuts (Rotation.rotatePiX / rotateHalfPiY) remain available for tests:
        // an EulerRotation carrying rotatePiX's own angles (ZmXpZm, 0, π, 0) builds the same system
        // as `Layer.rotatePiX` on the unrotated layer.
        let built = sampleToSystem (resolveOrFail (uniaxialFilm (EulerRotation (ZmXpZm, Angle.zero, Angle.pi, Angle.zero)))) orientationRunW
        let unrotated = sampleToSystem (resolveOrFail (uniaxialFilm PrimaryAxes)) orientationRunW
        let expected = unrotated.films |> List.map (fun (l : Layer) -> l.rotatePiX)
        Assert.Equal<Layer list>(expected, built.films)

    [<Fact>]
    let ``an EulerRotation substrate plate rotates the substrate tensors the same way`` () =
        // The substrate is a `SampleLayer` too — its orientation goes through the same
        // `Layer.rotate` path when the plate is assembled.
        let phi, theta, psi = deg 30.0, deg 40.0, deg 50.0
        let built = sampleToSystem (resolveOrFail (uniaxialPlate (EulerRotation (ZmXpZm, phi, theta, psi)))) orientationRunW
        let unrotated = sampleToSystem (resolveOrFail (uniaxialPlate PrimaryAxes)) orientationRunW
        let rotation = Rotation.create ZmXpZm phi theta psi |> Rotation
        match built.substrate, unrotated.substrate with
        | Some (Substrate.Plate b), Some (Substrate.Plate u) ->
            Assert.Equal<Layer>(u.rotate rotation, b)
            Assert.False((u = b), "the Euler rotation left the substrate tensors unchanged")
        | other -> Assert.Fail($"expected two substrate plates, got %A{other}")
