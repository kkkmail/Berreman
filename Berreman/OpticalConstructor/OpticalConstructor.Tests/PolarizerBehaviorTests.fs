namespace OpticalConstructor.Tests

open Xunit
open Berreman.Geometry
open Berreman.Fields
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.Propagation
open OpticalConstructor.Domain.Facets
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.LibraryFacets

/// Spec 0038 Part F (step 014) — ideal elements become ordinary protected entries and polarizer
/// behaviour becomes data. Pins: `ComputedIdeal` parity with the EXISTING `analyzerMueller` /
/// `inputStokes` synthesis across orientations; the `R(−θ)·M·R(θ)` component rotation; the ORDERED
/// rotated product of a `ConstantMueller` compound (list in light-traversal order — first component
/// applied first); whole-compound rotation ≡ per-component offset shift; `ProtectedBuiltIn` on every
/// seeded preset with every seeded sample `UserManaged`; and the polarizer-category facet.
module PolarizerBehaviorTests =

    let private deg (d : float) : Angle = Angle.degree d

    /// The reference-orientation ideal LP (θ = 0) every compound test builds components from.
    let private lp0 : MuellerMatrix = analyzerMueller IdealLinear (deg 0.0)

    /// Element-wise Mueller equality through the `muellerElement` read seam.
    let private assertMuellerEqual (expected : MuellerMatrix) (actual : MuellerMatrix) : unit =
        for i in 0 .. 3 do
            for j in 0 .. 3 do
                let e = muellerElement expected i j
                let a = muellerElement actual i j
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
                    for j in 0 .. 3 -> abs (muellerElement forward i j - muellerElement reversed i j)
            ]
            |> List.exists (fun d -> d > 1.0e-3)
        Assert.True(anyElementDiffers, "projectors onto different lines do not commute")

    [<Fact>]
    let ``a crossed two-component compound extinguishes any input`` () =
        let crossed = compoundMueller [ { matrix = lp0; offset = deg 0.0 }; { matrix = lp0; offset = deg 90.0 } ]
        Assert.True(abs (s0 (crossed * inputStokes IdealLinear (deg 30.0))) <= 1.0e-9)
        Assert.True(abs (s0 (crossed * unpolarizedStokes)) <= 1.0e-9)

    [<Fact>]
    let ``unpolarized light through an LP-LP compound follows Malus over the offset difference`` () =
        // ½ through the first LP, then cos²(45°) through the second: S0 = ½·½ = 0.25.
        let compound = compoundMueller [ { matrix = lp0; offset = deg 0.0 }; { matrix = lp0; offset = deg 45.0 } ]
        Assert.True(abs (s0 (compound * unpolarizedStokes) - 0.25) <= 1.0e-9)

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
            seedEntries
            |> List.partition (fun e ->
                match e with
                | SampleItem _ -> true
                | SourceItem _ | DetectorItem _ | PolarizerItem _ -> false)
        Assert.NotEmpty samples
        Assert.NotEmpty presets
        Assert.All(presets, fun e -> Assert.Equal<EntryProtection>(ProtectedBuiltIn, e.protection))
        Assert.All(samples, fun e -> Assert.Equal<EntryProtection>(UserManaged, e.protection))
        // The six protected built-ins are exactly the seeded presets the spec names (F.0).
        Assert.Equal<string list>(
            [ "det-ellipsometer"; "det-intensity"; "pol-cp-left"; "pol-cp-right"; "pol-lp"; "src-600" ],
            presets |> List.map (fun e -> e.entryId) |> List.sort)

    [<Fact>]
    let ``the three seeded ideals are ComputedIdeal entries with their Lp and Cp categories`` () =
        let polarizer (id : string) : PolarizerPreset =
            seedEntries
            |> List.pick (fun e ->
                match e with
                | PolarizerItem p when p.id = id -> Some p
                | _ -> None)
        Assert.Equal<PolarizerBehavior>(ComputedIdeal IdealLinear, (polarizer "pol-lp").behavior)
        Assert.Equal<PolarizerCategory>(LpCategory, (polarizer "pol-lp").category)
        Assert.Equal<PolarizerBehavior>(ComputedIdeal IdealCircularLeft, (polarizer "pol-cp-left").behavior)
        Assert.Equal<PolarizerCategory>(CpCategory, (polarizer "pol-cp-left").category)
        Assert.Equal<PolarizerBehavior>(ComputedIdeal IdealCircularRight, (polarizer "pol-cp-right").behavior)
        Assert.Equal<PolarizerCategory>(CpCategory, (polarizer "pol-cp-right").category)

    // ============================ the polarizer-category facet ============================

    [<Fact>]
    let ``the polarizer-category facet applies only to polarizers and classifies the seeded ideals`` () =
        let def =
            libraryFacets builtInEntries
            |> List.find (fun d -> d.key = polarizerCategoryFacetKey)
        for entry in seedEntries do
            let expected =
                match entry with
                | PolarizerItem _ -> ApplicableAttribute
                | SampleItem _ | SourceItem _ | DetectorItem _ -> InapplicableAttribute
            Assert.Equal<Applicability>(expected, def.appliesTo entry)
        let extractedLabels (id : string) : string list =
            seedEntries
            |> List.find (fun e -> e.entryId = id)
            |> def.extract
            |> List.map (fun v -> v.label)
        Assert.Equal<string list>([ "Linear" ], extractedLabels "pol-lp")
        Assert.Equal<string list>([ "Circular" ], extractedLabels "pol-cp-left")
        Assert.Equal<string list>([ "Circular" ], extractedLabels "pol-cp-right")
