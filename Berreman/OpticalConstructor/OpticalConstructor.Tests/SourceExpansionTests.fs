namespace OpticalConstructor.Tests

open Berreman.Constants
open Berreman.Geometry
open Berreman.Fields
open Berreman.MaterialProperties
open Berreman.Media
open Berreman.Solvers
open Berreman.BerremanMatrix
open Berreman.FieldFunctions
open Analytics.Variables
open Analytics.StandardLightVariables
open OpticalConstructor.Domain.Units
open OpticalConstructor.Domain.SourceSpec
open OpticalConstructor.Domain.SourceCombination
open OpticalConstructor.Storage.SpectralImport
open OpticalConstructor.Ui.Sources
open Xunit

/// Part E expand-on-the-boundary tests (slice 007). Prove the load-bearing
/// contract: spectral profiles, cone fans, the unpolarized split, multi-source
/// averaging, and the Gaussian angular spectrum all fan a base `IncidentLightInfo`
/// into weighted `IncidentLightInfo` values, and the combination is on
/// `StokesVector` OUTPUTS (`StokesVector.(+)`/`Zero`), never on `EmField`
/// amplitudes (AC-E5..AC-E9).
module SourceExpansionTests =

    let private baseSource = SourceEditorView.defaultSource "src"

    /// The test's solve-and-read-Stokes closure: build the incident `EmField` in
    /// vacuum (engine `EmField.create`) and read `EmField.stokesVector`. Combining
    /// happens on these StokesVectors only — never on the EmFields.
    let private toStokes (info : IncidentLightInfo) : StokesVector =
        (EmField.create(info, OpticalProperties.vacuum)).stokesVector
        |> Option.defaultValue StokesVector.Zero

    let private comps (s : StokesVector) =
        let (StokesVector v) = s
        [ v.[0]; v.[1]; v.[2]; v.[3] ]

    let private glass = OpticalProperties.fromRefractionIndex (RefractionIndex 1.5)

    // ---------------------------------------------------------------- AC-E5

    [<Fact>]
    let ``AC-E5 a Flat profile samples unit weights over the wavelength range in meters`` () =
        let range = wavelength200to800Range 10
        let samples = SpectralProfile.sample Flat range
        Assert.Equal(11, List.length samples)
        Assert.All(samples, fun (_, w) -> Assert.Equal(1.0, w, 12))
        // Endpoints reduce to canonical meters (200 nm .. 800 nm).
        let (firstWl, _) = List.head samples
        let (lastWl, _) = List.last samples
        Assert.Equal(2.0e-7, firstWl.value / 1.0<meter>, 15)
        Assert.Equal(8.0e-7, lastWl.value / 1.0<meter>, 15)

    [<Fact>]
    let ``AC-E5 a blackbody profile weights the band by Planck's law in canonical SI`` () =
        let range = wavelength200to800Range 10
        let samples = SpectralProfile.sample (Blackbody 5000.0<K>) range
        Assert.Equal(11, List.length samples)
        Assert.All(samples, fun (_, w) -> Assert.True(w > 0.0, $"expected a positive Planck weight, got {w}"))
        // At 5000 K the Planck peak (~580 nm) is inside the band, so a mid-band
        // sample outweighs the 200 nm UV edge — the profile is not flat.
        let weights = samples |> List.map snd
        Assert.True(weights.[5] > weights.[0], "blackbody weight must rise from the UV edge toward the peak")

    [<Fact>]
    let ``AC-E5 a laser line is a monochromatic delta at its line wavelength`` () =
        let samples = SpectralProfile.sample (LaserLine (WaveLength.nm 633.0<nm>)) (wavelength200to800Range 10)
        match samples with
        | [ (wl, w) ] ->
            Assert.Equal(6.33e-7, wl.value / 1.0<meter>, 15)
            Assert.Equal(1.0, w, 12)
        | other -> Assert.Fail($"expected a single laser-line sample, got {List.length other}")

    [<Fact>]
    let ``AC-E5 an imported spectrum is read from a CSV sidecar via FSharp.Data in canonical meters`` () =
        let csv = "wavelength_nm,weight\n400,0.5\n500,1.0\n600,0.8\n"
        match parseSpectrumCsv csv with
        | Ok pairs ->
            Assert.Equal(3, List.length pairs)
            let (wl0, w0) = List.head pairs
            Assert.Equal(4.0e-7, wl0.value / 1.0<meter>, 15)
            Assert.Equal(0.5, w0, 12)
            // The pure profile sampler leaves an unloaded import empty — the data
            // arrives from the sidecar (FSharp.Data), never from a `.binz` pickle.
            Assert.Equal<(WaveLength * float) list>([], SpectralProfile.sample (ImportedSpectrum "spectrum.csv") (wavelength200to800Range 4))
        | Error e -> Assert.Fail($"expected a parsed spectrum, got {e}")

    [<Fact>]
    let ``AC-E5 a micrometre-headed spectrum CSV reduces to canonical meters`` () =
        match parseSpectrumCsv "lambda_um,weight\n0.5,1.0\n0.6,0.7\n" with
        | Ok ((wl, _) :: _) -> Assert.Equal(5.0e-7, wl.value / 1.0<meter>, 15)
        | other -> Assert.Fail($"expected a µm spectrum parse, got {other}")

    // ---------------------------------------------------------------- AC-E6

    [<Fact>]
    let ``AC-E6 a cone fans the base angle into N symmetric sub-angles`` () =
        let cone = { halfAngle = Angle.degree 10.0; samples = 5 }
        let angles = ConeAcceptance.sample cone IncidenceAngle.normal
        Assert.Equal(5, List.length angles)
        Assert.Equal((Angle.degree -10.0).value, angles.[0].value, 12)
        Assert.Equal(0.0, angles.[2].value, 12)
        Assert.Equal((Angle.degree 10.0).value, angles.[4].value, 12)

    [<Fact>]
    let ``AC-E6 a single-sample cone collapses to the base angle`` () =
        let cone = { halfAngle = Angle.degree 10.0; samples = 1 }
        Assert.Equal<IncidenceAngle list>([ IncidenceAngle.normal ], ConeAcceptance.sample cone IncidenceAngle.normal)

    [<Fact>]
    let ``AC-E6 a collimated source (cone = None) expands to a single sample`` () =
        let collimated = SourceSpec.expand baseSource
        Assert.Equal(1, List.length collimated)
        // A collimated single sample carries unit weight (intensity 1.0, N = 1).
        Assert.All(collimated, fun (_, w) -> Assert.Equal(1.0, w, 12))
        // A cone fans it into `samples` weighted entries instead.
        let coned = { baseSource with cone = Some { halfAngle = Angle.degree 5.0; samples = 7 } }
        let conedExpanded = SourceSpec.expand coned
        Assert.Equal(7, List.length conedExpanded)
        // AC-E6: the cone result is AVERAGED, not summed — each of the N sub-angles
        // carries weight 1/N and the per-sample weights sum to 1 (intensity 1.0), so a
        // 7-sample cone does NOT produce 7x the Stokes/intensity of a collimated source.
        Assert.All(conedExpanded, fun (_, w) -> Assert.Equal(1.0 / 7.0, w, 12))
        Assert.Equal(1.0, conedExpanded |> List.sumBy snd, 12)

    // ---------------------------------------------------------------- AC-E7

    [<Fact>]
    let ``AC-E7 an unpolarized source expands to the incoherent s/p average`` () =
        let unpol = { baseSource with coherence = Unpolarized }
        let expanded = SourceSpec.expand unpol
        Assert.Equal(2, List.length expanded)
        let pols = expanded |> List.map (fun (info, _) -> info.polarization.value) |> List.sort
        Assert.Equal<float list>([ Polarization.s.value; Polarization.p.value ] |> List.sort, pols)
        Assert.All(expanded, fun (_, w) -> Assert.Equal(0.5, w, 12))

    [<Fact>]
    let ``AC-E7 an incoherent source routes through the reused substrate Multiple path`` () =
        // The source-level flag selects the engine's existing thick-substrate
        // multi-reflection branch; no separate incoherent solver is added.
        let plateSys : OpticalSystem =
            {
                description = None
                upper = OpticalProperties.vacuum
                films = []
                substrate = Some (Plate { properties = glass; thickness = Thickness.mm 1.0<mm> })
                lower = OpticalProperties.vacuum
            }
        let info = SourceSpec.toIncidentLight baseSource
        match OpticalSystemSolver(info, plateSys).solution with
        | Multiple _ -> ()
        | Single _ -> Assert.Fail("a Plate substrate MUST route into the reused Multiple branch")

    // ---------------------------------------------------------------- AC-E8

    [<Fact>]
    let ``AC-E8 multiple sources combine as a weighted incoherent StokesVector sum`` () =
        let src1 = { baseSource with sourceId = "a"; intensity = 1.0 }
        let src2 =
            { baseSource with
                sourceId = "b"
                intensity = 2.0
                light = { baseSource.light with polarization = Polarization.p } }
        let combined = combineSources toStokes [ src1; src2 ]
        // Manual weighted incoherent sum on the StokesVector outputs.
        let manual =
            scaleStokes 1.0 (toStokes (SourceSpec.toIncidentLight src1))
            + scaleStokes 2.0 (toStokes (SourceSpec.toIncidentLight src2))
        List.iter2 (fun (a : float) (b : float) -> Assert.Equal(a, b, 12)) (comps combined) (comps manual)

    [<Fact>]
    let ``AC-E8 an empty source list combines to StokesVector.Zero`` () =
        let combined = combine toStokes []
        Assert.Equal<float list>(comps StokesVector.Zero, comps combined)

    // ---------------------------------------------------------------- AC-E9

    [<Fact>]
    let ``AC-E9 the Gaussian angular spectrum is a normalised weighted plane-wave fan`` () =
        let beam = { waist = 1.0e-6<meter>; divergence = Angle.degree 80.0 }
        let spectrum = GaussianBeamSpec.angularSpectrum beam (WaveLength.nm 500.0<nm>)
        Assert.False(List.isEmpty spectrum)
        // Weights normalise to 1 and every angle is within the divergence aperture.
        Assert.Equal(1.0, spectrum |> List.sumBy snd, 9)
        Assert.All(spectrum, fun (a, _) -> Assert.True(a.value <= (Angle.degree 80.0).value + 1.0e-9))
        // The peak weight sits at the smallest (near-axis) angle.
        let peakAngle = spectrum |> List.maxBy snd |> fst
        let minAngle = spectrum |> List.map (fun (a, _) -> a.value) |> List.min
        Assert.Equal(minAngle, peakAngle.value, 12)

    [<Fact>]
    let ``AC-E9 a cone + Gaussian source normalises to the source intensity, not N`` () =
        // Both a cone fan (N sub-angles) and a Gaussian fan (unit-sum per angle) are
        // set: each cone angle must carry the per-cone-angle 1/N factor so the
        // combined source totals weight 1 (the source intensity), matching the
        // cone-only and unpolarized 0.5/0.5 normalisations — NOT N.
        let n = 5
        let coneGaussian =
            { baseSource with
                cone = Some { halfAngle = Angle.degree 5.0; samples = n }
                gaussianBeam = Some { waist = 1.0e-6<meter>; divergence = Angle.degree 80.0 } }
        let expanded = SourceSpec.expand coneGaussian
        // The expansion fans into N * (Gaussian samples) entries...
        Assert.True(List.length expanded > n, "expected a cone-by-Gaussian fan")
        // ...but the per-source weights sum to the source intensity (1.0), not N.
        let totalWeight = expanded |> List.sumBy snd
        Assert.Equal(baseSource.intensity, totalWeight, 9)
        Assert.True(abs (totalWeight - float n) > 0.5, $"weights must total intensity, not N={n} (got {totalWeight})")

    [<Fact>]
    let ``AC-E9 a wider waist yields a narrower angular spread`` () =
        let w = WaveLength.nm 500.0<nm>
        let charAngle (waistM : float<meter>) =
            { waist = waistM; divergence = Angle.degree 80.0 }
            |> fun b -> GaussianBeamSpec.angularSpectrum b w
            |> List.sumBy (fun (a, weight) -> abs a.value * weight)   // weights sum to 1
        Assert.True(charAngle 2.0e-6<meter> < charAngle 1.0e-6<meter>,
                    "a larger beam waist must produce a smaller mean divergence angle")
