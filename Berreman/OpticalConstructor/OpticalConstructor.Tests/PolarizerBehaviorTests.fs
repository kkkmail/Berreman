namespace OpticalConstructor.Tests

open Xunit
open Berreman.Geometry
open Berreman.Fields
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.Propagation
open OpticalConstructor.Domain.Facets
open OpticalConstructor.Domain.LibraryFacets

/// Spec 0038 Part F step 014; spec 0050 S1.7: ideal parity, ordered compound rotation,
/// and polarizer facets. `BuiltinCatalogue.presets` are protected;
/// `BuiltinCatalogue.samples` are user-managed.
module PolarizerBehaviorTests =

    let private deg (d : float) : Angle = Angle.degree (d * 1.0<Berreman.Constants.deg>)

    let private stokesComponents (sv : StokesVector) : float * float * float * float = sv.s0, sv.s1, sv.s2, sv.s3

    /// The reference-orientation ideal LP (θ = 0) every compound test builds components from.
    let private lp0 : MuellerMatrix = analyzerMueller IdealLinear (deg 0.0)

    /// Element-wise Mueller equality through the `MuellerMatrix.element` read seam.
    let private assertMuellerEqual (expected : MuellerMatrix) (actual : MuellerMatrix) : unit =
        for i in 0 .. 3 do
            for j in 0 .. 3 do
                let e = expected.element i j
                let a = actual.element i j
                Assert.True(abs (e - a) <= 1.0e-9, $"element ({i},{j}): expected %g{e}, got %g{a}")

    let private sampledDegrees : float list =
        [ 0.0; 15.0; 30.0; 45.0; 60.0; 90.0; 120.0; 135.0; 180.0; 225.0; 270.0; 315.0; 330.0 ]

    // ============================ ComputedIdeal parity (acceptance 1) ============================

    [<Fact>]
    let ``behaviorMueller of a ComputedIdeal equals analyzerMueller across kinds and orientations`` () =
        for kind in [ IdealLinear; IdealCircularLeft; IdealCircularRight ] do
            for d in sampledDegrees do
                assertMuellerEqual (analyzerMueller kind (deg d)) (behaviorMueller (ComputedIdeal kind) (deg d))

    [<Fact>]
    let ``behaviorInputStokes of a ComputedIdeal equals inputStokes across kinds and orientations`` () =
        for kind in [ IdealLinear; IdealCircularLeft; IdealCircularRight ] do
            for d in sampledDegrees do
                let e0, e1, e2, e3 = stokesComponents (inputStokes kind (deg d))
                let a0, a1, a2, a3 = stokesComponents (behaviorInputStokes (ComputedIdeal kind) (deg d))
                Assert.True(abs (e0 - a0) <= 1.0e-9 && abs (e1 - a1) <= 1.0e-9 && abs (e2 - a2) <= 1.0e-9 && abs (e3 - a3) <= 1.0e-9,
                            $"%A{kind} at %g{d}°: expected (%g{e0}, %g{e1}, %g{e2}, %g{e3}), got (%g{a0}, %g{a1}, %g{a2}, %g{a3})")

    // ============================ the component rotation R(−θ)·M·R(θ) ============================

    [<Fact>]
    let ``rotateMueller carries the reference LP exactly onto analyzerMueller at theta`` () =
        // Pins the rotation sign convention: R(−θ)·LP₀·R(θ) IS the ideal LP at θ.
        for d in sampledDegrees do
            assertMuellerEqual (analyzerMueller IdealLinear (deg d)) (rotateMueller (deg d) lp0)

    // ============================ the ordered compound product (acceptance 2) ============================

    [<Fact>]
    let ``a two-component ConstantMueller compound is the ordered product of its offset-rotated components`` () =
        // Two linear polarizers at different offsets (light hits the 20° one first): the compound is
        // M(65°)·M(20°) — each component the reference matrix rotated by its own offset, multiplied
        // in traversal order.
        let c1 = { matrix = lp0; offset = deg 20.0 }
        let c2 = { matrix = lp0; offset = deg 65.0 }
        let expected = analyzerMueller IdealLinear (deg 65.0) * analyzerMueller IdealLinear (deg 20.0)
        assertMuellerEqual expected (compoundMueller [ c1; c2 ])

    [<Fact>]
    let ``the compound product order matters — reversing the components changes the matrix`` () =
        let c1 = { matrix = lp0; offset = deg 20.0 }
        let c2 = { matrix = lp0; offset = deg 65.0 }
        let forward = compoundMueller [ c1; c2 ]
        let reversed = compoundMueller [ c2; c1 ]
        let anyElementDiffers =
            [
                for i in 0 .. 3 do
                    for j in 0 .. 3 -> abs (forward.element i j - reversed.element i j)
            ]
            |> List.exists (fun d -> d > 1.0e-3)
        Assert.True(anyElementDiffers, "projectors onto different lines do not commute")

    [<Fact>]
    let ``a crossed two-component compound extinguishes any input`` () =
        let crossed = compoundMueller [ { matrix = lp0; offset = deg 0.0 }; { matrix = lp0; offset = deg 90.0 } ]
        Assert.True(abs ((crossed * inputStokes IdealLinear (deg 30.0)).s0) <= 1.0e-9)
        Assert.True(abs ((crossed * unpolarizedStokes).s0) <= 1.0e-9)

    [<Fact>]
    let ``unpolarized light through an LP-LP compound follows Malus over the offset difference`` () =
        // ½ through the first LP, then cos²(45°) through the second: S0 = ½·½ = 0.25.
        let compound = compoundMueller [ { matrix = lp0; offset = deg 0.0 }; { matrix = lp0; offset = deg 45.0 } ]
        Assert.True(abs ((compound * unpolarizedStokes).s0 - 0.25) <= 1.0e-9)

    // ============================ run-time orientation over a compound ============================

    [<Fact>]
    let ``behaviorMueller rotates the whole compound like shifting every component offset by theta`` () =
        // Single component: the compound at θ IS the ideal LP at (θ + offset).
        let single = ConstantMueller [ { matrix = lp0; offset = deg 25.0 } ]
        assertMuellerEqual (analyzerMueller IdealLinear (deg 55.0)) (behaviorMueller single (deg 30.0))
        // Two components: R(−θ)·[M₂·M₁]·R(θ) = M(θ+o₂)·M(θ+o₁) (R(a)·R(b) = R(a+b)).
        let compound = ConstantMueller [ { matrix = lp0; offset = deg 20.0 }; { matrix = lp0; offset = deg 65.0 } ]
        let expected = analyzerMueller IdealLinear (deg 95.0) * analyzerMueller IdealLinear (deg 50.0)
        assertMuellerEqual expected (behaviorMueller compound (deg 30.0))

    [<Fact>]
    let ``behaviorInputStokes applies a constant compound to unpolarized light un-normalized`` () =
        // One LP component at reference, element rotated to 30°: ½·[1; cos60°; sin60°; 0].
        let behavior = ConstantMueller [ { matrix = lp0; offset = deg 0.0 } ]
        let a0, a1, a2, a3 = stokesComponents (behaviorInputStokes behavior (deg 30.0))
        Assert.True(abs (a0 - 0.5) <= 1.0e-9)
        Assert.True(abs (a1 - 0.5 * cos (60.0 * System.Math.PI / 180.0)) <= 1.0e-9)
        Assert.True(abs (a2 - 0.5 * sin (60.0 * System.Math.PI / 180.0)) <= 1.0e-9)
        Assert.True(abs a3 <= 1.0e-9)

    // ============================ EntryProtection (acceptance 3) ============================

    [<Fact>]
    let ``every seeded preset is ProtectedBuiltIn and every seeded sample UserManaged`` () =
        let samples, presets =
            ((BuiltinCatalogue.samples () |> List.map SampleItem) @ BuiltinCatalogue.presets ())
            |> List.partition (fun e ->
                match e with
                | SampleItem _ -> true
                | SourceItem _ | DetectorItem _ | PolarizerItem _ -> false)
        Assert.NotEmpty samples
        Assert.NotEmpty presets
        Assert.All(presets, fun e -> Assert.Equal<EntryProtection>(ProtectedBuiltIn, e.protection))
        Assert.All(samples, fun e -> Assert.Equal<EntryProtection>(UserManaged, e.protection))
        // Spec 0038 F.0; spec 0046; spec 0050 S1.7: BuiltinCatalogue.presets protects
        // the ideal elements and every BuiltinCatalogue.sources entry.
        Assert.Equal<string list>(
            [ PresetIds.detectorEllipsometer; PresetIds.detectorIntensity; PresetIds.polarizerCpLeft; PresetIds.polarizerCpRight; PresetIds.polarizerLp ]
            @ (BuiltinCatalogue.sources () |> List.map (fun s -> s.id))
            |> List.map (fun i -> i.value)
            |> List.sort,
            presets |> List.map (fun e -> e.entryId.value) |> List.sort)

    [<Fact>]
    let ``the three seeded ideals are ComputedIdeal entries with their Lp and Cp categories`` () =
        let polarizer (id : Placement.ElementValueId) : PolarizerPreset =
            ((BuiltinCatalogue.samples () |> List.map SampleItem) @ BuiltinCatalogue.presets ())
            |> List.pick (fun e ->
                match e with
                | PolarizerItem p when p.id = id -> Some p
                | _ -> None)
        Assert.Equal<PolarizerBehavior>(ComputedIdeal IdealLinear, (polarizer PresetIds.polarizerLp).behavior)
        Assert.Equal<PolarizerCategory>(LpCategory, (polarizer PresetIds.polarizerLp).category)
        Assert.Equal<PolarizerBehavior>(ComputedIdeal IdealCircularLeft, (polarizer PresetIds.polarizerCpLeft).behavior)
        Assert.Equal<PolarizerCategory>(CpCategory, (polarizer PresetIds.polarizerCpLeft).category)
        Assert.Equal<PolarizerBehavior>(ComputedIdeal IdealCircularRight, (polarizer PresetIds.polarizerCpRight).behavior)
        Assert.Equal<PolarizerCategory>(CpCategory, (polarizer PresetIds.polarizerCpRight).category)

    // ============================ the polarizer-category facet ============================

    [<Fact>]
    let ``the polarizer-category facet applies only to polarizers and classifies the seeded ideals`` () =
        let def =
            libraryFacets (BuiltinCatalogue.categories ()) (BuiltinCatalogue.materials ())
            |> List.find (fun d -> d.key = polarizerCategoryFacetKey)
        for entry in ((BuiltinCatalogue.samples () |> List.map SampleItem) @ BuiltinCatalogue.presets ()) do
            let expected =
                match entry with
                | PolarizerItem _ -> ApplicableAttribute
                | SampleItem _ | SourceItem _ | DetectorItem _ -> InapplicableAttribute
            Assert.Equal<Applicability>(expected, def.appliesTo entry)
        let extractedLabels (id : Placement.ElementValueId) : string list =
            ((BuiltinCatalogue.samples () |> List.map SampleItem) @ BuiltinCatalogue.presets ())
            |> List.find (fun e -> e.entryId = id)
            |> def.extract
            |> List.map (fun v -> v.label)
        Assert.Equal<string list>([ "Linear" ], extractedLabels PresetIds.polarizerLp)
        Assert.Equal<string list>([ "Circular" ], extractedLabels PresetIds.polarizerCpLeft)
        Assert.Equal<string list>([ "Circular" ], extractedLabels PresetIds.polarizerCpRight)
