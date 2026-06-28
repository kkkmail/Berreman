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
    let ``the multilayer sample maps to a real three-film stack`` () =
        let multilayer : Sample =
            {
                id = "sample-multilayer-qw"
                name = "Quarter-wave glass multilayer"
                materialId = "glass-1.52"
                thickness = Thickness.nm 100.0<nm>
                substrate = Library.ThinFilm
            }
        let system = sampleToSystem multilayer
        Assert.Equal(3, List.length system.films)
