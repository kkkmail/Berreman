namespace OpticalConstructor.Tests

open System.Numerics
open Berreman.Constants
open Berreman.MathNetNumericsMath
open Berreman.Fields
open Berreman.MaterialProperties
open Berreman.Dispersion
open OpticalProperties.Standard
open OpticalProperties.Active
open OpticalConstructor.Domain.MaterialLibrary
open Xunit

/// Spec 0033 step 013 (AC-B7) — the `MaterialComplexity` edit-model option tree.
/// `toProperties` MUST default absent magnetic/active options to the vacuum μ/ρ and
/// assemble present options through `toMuWithDisp` / `toRhoWithDisp`; every
/// re-expressed built-in's `complexity.toProperties` MUST reproduce the original
/// engine preset's tensors at reference wavelengths; silicon / langasite (dispersion
/// coded in `OpticalProperties/Dispersive.fs:98-99`) and the vacuum spacer stay
/// `None` (engine-preset, view-only).
module MaterialComplexityTests =

    let private closeC (tol : float) (a : Complex) (b : Complex) =
        abs (a.Real - b.Real) <= tol && abs (a.Imaginary - b.Imaginary) <= tol

    let private epsClose (tol : float) (a : Eps) (b : Eps) =
        seq { for i in 0..2 do for j in 0..2 -> closeC tol a.[i, j] b.[i, j] } |> Seq.forall id

    let private muClose (tol : float) (a : Mu) (b : Mu) =
        seq { for i in 0..2 do for j in 0..2 -> closeC tol a.[i, j] b.[i, j] } |> Seq.forall id

    let private rhoClose (tol : float) (a : Rho) (b : Rho) =
        seq { for i in 0..2 do for j in 0..2 -> closeC tol a.[i, j] b.[i, j] } |> Seq.forall id

    let private propertiesClose (tol : float) (a : OpticalProperties) (b : OpticalProperties) =
        epsClose tol a.eps b.eps && muClose tol a.mu b.mu && rhoClose tol a.rho b.rho

    /// Reference wavelengths for the visible-range built-ins.
    let private visibleGrid : WaveLength list =
        [ 400.0<nm>; 600.0<nm>; 800.0<nm> ] |> List.map WaveLength.nm

    /// Reference wavelengths for the EUV built-ins (the presets' documented band).
    let private euvGrid : WaveLength list =
        [ 10.0<nm>; 13.5<nm> ] |> List.map WaveLength.nm

    let private entryOf (id : MaterialId) : MaterialEntry =
        builtInEntries |> List.find (fun e -> e.id = id)

    [<Fact>]
    let ``AC-B7 toProperties defaults absent magnetic and active options to vacuum mu and rho`` () =
        let c =
            {
                eps = EpsWithoutDispValue (IsotropicTransparent (RefractionIndex 1.4))
                magnetic = None
                active = None
            }
        let p = c.toProperties
        // The defaults short-circuit to the engine's non-dispersive vacuum cases
        // (Mu.vacuum.dispersive / Rho.vacuum.dispersive) — no closure overhead.
        match p.muWithDisp with
        | MuWithoutDisp _ -> ()
        | MuWithDisp _ -> Assert.Fail("default mu must be the non-dispersive MuWithoutDisp vacuum case")
        match p.rhoWithDisp with
        | RhoWithoutDisp _ -> ()
        | RhoWithDisp _ -> Assert.Fail("default rho must be the non-dispersive RhoWithoutDisp vacuum case")
        let w = WaveLength.nm 600.0<nm>
        Assert.True(muClose 1e-12 (p.muWithDisp.getMu w) Mu.vacuum, "default mu must be Mu.vacuum (identity)")
        Assert.True(rhoClose 1e-12 (p.rhoWithDisp.getRho w) Rho.vacuum, "default rho must be Rho.vacuum (zero)")
        Assert.True(epsClose 1e-12 (p.epsWithDisp.getEps w) (Eps.fromRefractionIndex (RefractionIndex 1.4)), "eps composes through toEpsWithDisp")

    [<Fact>]
    let ``AC-B7 present magnetic and active options assemble through toMuWithDisp and toRhoWithDisp`` () =
        let polder =
            {
                muDiagonal = MuValue 1.1
                muParallel = MuValue 1.05
                gyration = MuValue 0.2
                axis = GyrationAxis.defaultValue
            }
        let c =
            {
                eps = EpsWithoutDispValue (IsotropicTransparent (RefractionIndex 1.4))
                magnetic = Some (MuWithoutDispValue (GyromagneticMu polder))
                active = Some (RhoWithoutDispValue { gyration = PlanarActive (RhoValue 2.0e-6); hand = RightHanded })
            }
        let p = c.toProperties
        let w = WaveLength.nm 600.0<nm>
        let mu = p.muWithDisp.getMu w
        let rho = p.rhoWithDisp.getRho w
        Assert.True(muClose 1e-12 mu (GyromagneticMu polder).toMu, "Polder mu must assemble through the engine's own toMu path")
        Assert.True(rhoClose 1e-12 rho (Rho.planarCrystal (RhoValue 2.0e-6)), "PlanarActive rho must assemble through Rho.planarCrystal")
        Assert.False(muClose 1e-12 mu Mu.vacuum, "assembled mu must not be the vacuum default")
        Assert.False(rhoClose 1e-12 rho Rho.vacuum, "assembled rho must not be the vacuum default")

    [<Fact>]
    let ``AC-B7 every re-expressed built-in's toProperties reproduces the original preset at reference wavelengths`` () =
        // The original preset expressions, restated verbatim from the engine
        // (Standard.fs / Active.fs) — NOT read back from the entries under test.
        let activeCrystalPreset =
            (OpticalProperties.planarCrystal
                (RefractionIndex 2.315 |> EpsValue.fromRefractionIndex)
                (RefractionIndex 2.226 |> EpsValue.fromRefractionIndex)
                (RhoValue 1.5e-6)).dispersive
        let cases : (string * MaterialId * OpticalPropertiesWithDisp * WaveLength list) list =
            [
                "glass152", MaterialIds.glass152, OpticalProperties.transparentGlass.dispersive, visibleGrid
                "glass150", MaterialIds.glass150, OpticalProperties.transparentGlass150.dispersive, visibleGrid
                "glass175", MaterialIds.glass175, OpticalProperties.transparentGlass175.dispersive, visibleGrid
                "glass200", MaterialIds.glass200, OpticalProperties.transparentGlass200.dispersive, visibleGrid
                "uniaxialCrystal", MaterialIds.uniaxialCrystal, OpticalProperties.uniaxialCrystal.dispersive, visibleGrid
                "biaxialCrystal", MaterialIds.biaxialCrystal, OpticalProperties.biaxialCrystal.dispersive, visibleGrid
                "euvMolybdenum", MaterialIds.euvMolybdenum, OpticalProperties.euvMolybdenum.dispersive, euvGrid
                "euvSilicon", MaterialIds.euvSilicon, OpticalProperties.euvSilicon.dispersive, euvGrid
                "activeCrystal", MaterialIds.activeCrystal, activeCrystalPreset, visibleGrid
            ]
        for (label, id, original, grid) in cases do
            let entry = entryOf id
            match entry.complexity with
            | None -> Assert.Fail($"{label}: a re-expressed built-in must carry Some complexity")
            | Some c ->
                let rebuilt = c.toProperties
                for w in grid do
                    let expected = original.getProperties w
                    Assert.True(propertiesClose 1e-12 (rebuilt.getProperties w) expected, $"{label}: toProperties must reproduce the preset at λ={w}")
                    // The seeded entry's properties ARE the complexity's composition
                    // (source-of-truth invariant, held by construction at seed time).
                    Assert.True(propertiesClose 1e-12 (entry.properties.getProperties w) expected, $"{label}: seeded properties must equal the preset at λ={w}")

    [<Fact>]
    let ``silicon, langasite and the vacuum spacer keep complexity None; the nine re-expressed entries carry Some`` () =
        for id in [ MaterialIds.silicon; MaterialIds.langasite; MaterialIds.vacuum ] do
            let e = entryOf id
            match e.complexity with
            | None -> ()
            | Some _ -> Assert.Fail($"{e.name}: expected None complexity (engine-preset entry, view-only)")
        let someCount =
            builtInEntries
            |> List.filter (fun e ->
                match e.complexity with
                | Some _ -> true
                | None -> false)
            |> List.length
        Assert.Equal(9, someCount)
