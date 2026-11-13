namespace OpticalConstructor.Tests

open System.Numerics
open Berreman.Constants
open Berreman.MathNetNumericsMath
open Berreman.Fields
open Berreman.MaterialProperties
open Berreman.Dispersion
open OpticalProperties.Standard
open OpticalProperties.Active
open OpticalProperties.Dispersive
open OpticalConstructor.Domain.CrystalPhysics
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.DispersionModels
open OpticalConstructor.Domain.MaterialComplexityEditor
open OpticalConstructor.Domain.LibraryFacets
open Xunit

/// Spec 0033 step 013 (AC-B7) — the `MaterialComplexity` edit-model option tree.
/// `toProperties` MUST default absent magnetic/active options to the vacuum μ/ρ and
/// assemble present options through `toMuWithDisp` / `toRhoWithDisp`; every
/// re-expressed built-in's physics `toProperties` MUST reproduce the original
/// engine preset's tensors at reference wavelengths.
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
        BuiltinCatalogue.material id

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
            let rebuilt =
                match entry.physics with
                | ComplexityPhysics complexity -> complexity.toProperties
            for w in grid do
                let expected = original.getProperties w
                Assert.True(propertiesClose 1e-12 (rebuilt.getProperties w) expected, $"{label}: toProperties must reproduce the preset at λ={w}")
                // BuiltinCatalogue.material properties must match the independent preset.
                Assert.True(propertiesClose 1e-12 (entry.properties.getProperties w) expected, $"{label}: seeded properties must equal the preset at λ={w}")

    [<Fact>]
    let ``the re-seeded silicon and langasite eps reproduce the engine presets`` () =
        // BuiltinCatalogue.material preserves the silicon / langasite ε laws (spec 0040 step 004;
        // spec 0050 steps 10, 51). Langasite keeps constant ρ unlike the engine's published law,
        // so only ε is compared with the engine presets.
        for w in visibleGrid do
            let siliconEps = (entryOf MaterialIds.silicon).properties.epsWithDisp.getEps w
            Assert.True(epsClose 1e-12 siliconEps (siliconOpticalProperties.epsWithDisp.getEps w), $"silicon eps at λ={w}")
            let langasiteEps = (entryOf MaterialIds.langasite).properties.epsWithDisp.getEps w
            Assert.True(epsClose 1e-12 langasiteEps (langasiteOpticalProperties.epsWithDisp.getEps w), $"langasite eps at λ={w}")

    // =====================================================================
    // Spec 0035 Part C (slice 010) — dispersive gyration ρ and Polder μ editing.
    // The editor lifts the func-valued cases: toComplexity of an on-Dispersive
    // gyration / Polder builds RhoWithDispValue / MuWithDispValue whose engine
    // toRhoWithDisp / toMuWithDisp assembly equals the engine builders per
    // wavelength; ofComplexity then toComplexity round-trips a dispersive ρ / μ
    // by sampled tensor value.
    // =====================================================================

    /// A constant real formula: value at every reduced wavelength (µm-scaled).
    let private constFormula (value : double) : DispersionFormula =
        {
            terms = [ { lambda = 0.0; coefficients = [| value |]; power = 1; multiplier = 1.0 } ]
            wavelengthScale = 1.0e-6<meter>
        }

    /// A linear real formula a + b·x over the reduced wavelength x (µm).
    let private linearFormula (a : double) (b : double) : DispersionFormula =
        {
            terms = [ { lambda = 0.0; coefficients = [| a; b |]; power = 1; multiplier = 1.0 } ]
            wavelengthScale = 1.0e-6<meter>
        }

    let private applyOk (msg : MaterialComplexityMsg) (s : MaterialComplexityEditState) : MaterialComplexityEditState =
        match applyMaterialComplexityMsg msg s with
        | Ok next -> next
        | Error e -> failwith $"unexpected edit rejection: %A{e}"

    let private applied (msgs : MaterialComplexityMsg list) : MaterialComplexityEditState =
        msgs |> List.fold (fun s m -> applyOk m s) defaultState

    let private derived (s : MaterialComplexityEditState) : MaterialComplexity =
        match s.toComplexity with
        | Ok c -> c
        | Error e -> failwith $"expected a derivable complexity, got %A{e}"

    /// The gyration tensor a complexity's active option assembles at a wavelength
    /// through the engine's own toRhoWithDisp (Rho.vacuum when there is none).
    let private activeRhoAt (c : MaterialComplexity) (w : WaveLength) : Rho =
        match c.active with
        | Some rv -> rv.toRhoWithDisp.getRho w
        | None -> Rho.vacuum

    /// The Polder μ a complexity's magnetic option assembles at a wavelength
    /// through the engine's own toMuWithDisp (Mu.vacuum when there is none).
    let private magneticMuAt (c : MaterialComplexity) (w : WaveLength) : Mu =
        match c.magnetic with
        | Some mv -> mv.toMuWithDisp.getMu w
        | None -> Mu.vacuum

    [<Fact>]
    let ``toComplexity of an on-Dispersive gyration builds RhoWithDispValue matching the engine toRhoWithDisp`` () =
        // A uniaxial dispersive gyration: g11(x) = 5.9e-5 + 1.0e-5·x, g33(x) = −10.1e-5.
        let fG11 = linearFormula 5.9e-5 1.0e-5
        let fG33 = constFormula -10.1e-5
        let st =
            applied
                [
                    ChooseAnisotropy Uniaxial
                    SetActivity ActivityOn
                    SetActivityDispersion DispersiveComponents
                    SetGyrationComponentDispersion (G11, fG11)
                    SetGyrationComponentDispersion (G33, fG33)
                    SetHandedness LeftHanded
                ]
        match (derived st).active with
        | Some (RhoWithDispValue gv as rhoValue) ->
            // The built value is exactly the uniaxial dispersive gyration entered.
            Assert.Equal<GyrationClass<DispersionFormula>>(UniaxialActive { g11 = fG11; g33 = fG33 }, gv.gyration)
            Assert.Equal(LeftHanded, gv.hand)
            // Its per-wavelength assembly equals the engine's own toRhoWithDisp path:
            // the class builder over each formula evaluated at the wavelength, one
            // overall sign for the enantiomorph.
            for w in visibleGrid do
                let s = LeftHanded.sign
                let expected = Rho.type_3_4_6_Crystal (RhoValue (s * (fG11.evaluate w).value)) (RhoValue (s * (fG33.evaluate w).value))
                Assert.True(rhoClose 1e-12 (rhoValue.toRhoWithDisp.getRho w) expected, $"gyration assembly at λ={w}")
        | other -> Assert.Fail($"expected a dispersive gyration rho, got %A{other}")

    [<Fact>]
    let ``toComplexity of an on-Dispersive Polder builds MuWithDispValue matching the engine toMuWithDisp`` () =
        // A dispersive Polder μ on the transverse (Voigt) X axis.
        let fMuDiag = linearFormula 1.0 0.05
        let fMuPar = constFormula 1.1
        let fMuGyr = linearFormula 0.1 0.02
        let st =
            applied
                [
                    SetMagnetic MagneticOn
                    SetMagneticDispersion DispersiveComponents
                    SetMuDiagonalDispersion fMuDiag
                    SetMuParallelDispersion fMuPar
                    SetMuGyrationDispersion fMuGyr
                    ChooseGyrationAxis AlongX
                ]
        match (derived st).magnetic with
        | Some (MuWithDispValue polderF as muValue) ->
            // The built value is exactly the dispersive Polder facet entered.
            let expectedPolder : PolderValue<DispersionFormula> =
                { muDiagonal = fMuDiag; muParallel = fMuPar; gyration = fMuGyr; axis = AlongX }
            Assert.Equal<PolderValue<DispersionFormula>>(expectedPolder, polderF)
            // Its per-wavelength assembly equals the engine's own toMuWithDisp path:
            // the SAME Polder tensor assembly the constant GyromagneticMu case uses,
            // over each formula evaluated at the wavelength.
            for w in visibleGrid do
                let expected = (GyromagneticMu (polderF.map (fun f -> (f.evaluate w).toMuValue))).toMu
                Assert.True(muClose 1e-12 (muValue.toMuWithDisp.getMu w) expected, $"Polder assembly at λ={w}")
        | other -> Assert.Fail($"expected a dispersive Polder mu, got %A{other}")

    [<Fact>]
    let ``ofComplexity then toComplexity round-trips a dispersive rho and mu by sampled tensor value`` () =
        // A uniaxial dispersive gyration (quartz-like, class 32) and a Voigt-Y
        // dispersive Polder μ — assembled straight into the engine value trees.
        let dispersiveGyration : GyrationClass<DispersionFormula> =
            UniaxialActive { g11 = linearFormula 5.9e-5 1.0e-5; g33 = constFormula -10.1e-5 }
        let dispersivePolder : PolderValue<DispersionFormula> =
            { muDiagonal = linearFormula 1.0 0.05; muParallel = constFormula 1.1; gyration = linearFormula 0.1 0.02; axis = AlongY }
        let c =
            {
                eps = EpsWithoutDispValue (UniaxialTransparent { ordinary = RefractionIndex 1.5; extraordinary = RefractionIndex 1.55 })
                magnetic = Some (MuWithDispValue dispersivePolder)
                active = Some (RhoWithDispValue { gyration = dispersiveGyration; hand = LeftHanded })
            }
        match ofComplexity c with
        | Ok st ->
            let back = derived st
            for w in visibleGrid do
                Assert.True(rhoClose 1e-12 (activeRhoAt back w) (activeRhoAt c w), $"round-trip gyration at λ={w}")
                Assert.True(muClose 1e-12 (magneticMuAt back w) (magneticMuAt c w), $"round-trip Polder at λ={w}")
        | Error e -> Assert.Fail($"a dispersive rho/mu must seed the editor, got %A{e}")

    [<Fact>]
    let ``unchecking the Dispersive sub-toggle restores the constant gyration and Polder losslessly`` () =
        // Activity: constant baseline, flip Dispersive on with an edited formula, back off
        // — the constant returns; re-check — the dispersive edit returns.
        let activeConstant = applied [ ChooseAnisotropy Uniaxial; SetActivity ActivityOn ]
        let baselineActive = derived activeConstant
        let activeDispersive =
            applied
                [
                    ChooseAnisotropy Uniaxial
                    SetActivity ActivityOn
                    SetActivityDispersion DispersiveComponents
                    SetGyrationComponentDispersion (G11, constFormula 7.0e-5)
                ]
        Assert.NotEqual<MaterialComplexity>(baselineActive, derived activeDispersive)
        let activeBack = applyOk (SetActivityDispersion ConstantComponents) activeDispersive
        Assert.Equal<MaterialComplexity>(baselineActive, derived activeBack)
        Assert.Equal<MaterialComplexity>(derived activeDispersive, derived (applyOk (SetActivityDispersion DispersiveComponents) activeBack))
        // Magnetic: same lossless mechanic over the Polder sub-toggle.
        let magneticConstant = applied [ SetMagnetic MagneticOn; SetMuKind GyromagneticMuKind ]
        let baselineMagnetic = derived magneticConstant
        let magneticDispersive = applyOk (SetMagneticDispersion DispersiveComponents) (applyOk (SetMuGyrationDispersion (constFormula 0.3)) magneticConstant)
        Assert.NotEqual<MaterialComplexity>(baselineMagnetic, derived magneticDispersive)
        let magneticBack = applyOk (SetMagneticDispersion ConstantComponents) magneticDispersive
        Assert.Equal<MaterialComplexity>(baselineMagnetic, derived magneticBack)
        Assert.Equal<MaterialComplexity>(derived magneticDispersive, derived (applyOk (SetMagneticDispersion DispersiveComponents) magneticBack))

    [<Fact>]
    let ``step 015: a transcendental dispersive segment derives and evaluates without rejection`` () =
        // A dispersive segment whose model is transcendental (ForouhiBloomer, taken
        // from the segment picker's default choices) LOWERS to its law (spec 0050
        // step 7) — toComplexity DERIVES it (no SegmentNotLowerable), the derived eps
        // reproduces `evaluate` across the grid, and the entry stays view-only.
        let model = defaultModelChoices |> List.find (fun m -> m.kindCode = "ForouhiBloomer")
        let st = applied [ SetDispersion DispersiveSegments; ChooseSegmentModel (SegmentIndex 0, model) ]
        let c = derived st
        match c.eps with
        | EpsWithDispValue (IsotropicDispersive [ seg ]) ->
            match seg.dispersion with
            | ForouhiBloomerAxis _ -> ()
            | other -> Assert.Fail($"the transcendental segment must lower to its law, got %A{other}")
        | other -> Assert.Fail($"expected one isotropic dispersive segment, got %A{other}")
        Assert.False(c.isEditable)
        let f = model.evaluate
        for w in visibleGrid do
            let expected = Eps.fromComplexRefractionIndex (f w)
            Assert.True(epsClose 1e-12 (c.toProperties.epsWithDisp.getEps w) expected, $"derived eps at λ={w}")

    [<Fact>]
    let ``a published gyration law keeps a complexity view-only and refuses to seed the editor`` () =
        // Spec 0050 step 10: a published gyration law has no class or components the ladder can seed.
        let c =
            {
                eps = EpsWithoutDispValue (IsotropicTransparent (RefractionIndex 1.4))
                magnetic = None
                active = Some (PublishedLawRho LangasiteGyrationLaw)
            }
        Assert.False(c.isEditable)
        match ofComplexity c with
        | Error (PublishedGyrationNotEditableErr _) -> ()
        | other -> Assert.Fail($"a published gyration law must refuse to seed the editor, got %A{other}")

    [<Fact>]
    let ``each crystal record's builder composes its permittivity from its own fields`` () =
        // Spec 0050 S11.1: the records ahead of the material library keep steps 332-334's builders.
        let uniaxial : UniaxialParameters =
            {
                ordinaryIndex = RefractionIndex 1.544
                extraordinaryIndex = RefractionIndex 1.553
                g11 = RhoValue 5.8e-5
                g33 = RhoValue -1.0e-4
            }
        let triclinic : TriclinicParameters =
            {
                index1 = RefractionIndex 1.52
                index2 = RefractionIndex 1.56
                index3 = RefractionIndex 1.61
                g11 = RhoValue 3.0e-5
                g22 = RhoValue 4.0e-5
                g33 = RhoValue 5.0e-5
                g23 = RhoValue 1.0e-5
                g13 = RhoValue 2.0e-5
                g12 = RhoValue 6.0e-6
            }
        let absorbing : AbsorbingTriclinicParameters =
            {
                index1 = RefractionIndex 1.71
                index2 = RefractionIndex 1.74
                index3 = RefractionIndex 1.78
                epsIm11 = EpsValue 2.0e-3
                epsIm22 = EpsValue 3.0e-3
                epsIm33 = EpsValue 4.0e-3
                epsIm23 = EpsValue 5.0e-4
                epsIm13 = EpsValue 6.0e-4
                epsIm12 = EpsValue 7.0e-4
                g11 = RhoValue 3.0e-5
                g22 = RhoValue 4.0e-5
                g33 = RhoValue 5.0e-5
                g23 = RhoValue 1.0e-5
                g13 = RhoValue 2.0e-5
                g12 = RhoValue 6.0e-6
            }
        let squared (n : RefractionIndex) : float = n.value * n.value
        let assertDiagonal (label : string) (eps : Eps) (diagonal : float list) : unit =
            for i in 0..2 do
                for j in 0..2 do
                    let actual = eps.[i, j]
                    let expected = if i = j then Complex(diagonal.[i], 0.0) else Complex.Zero
                    Assert.True(closeC 1e-15 actual expected, $"{label}: eps[{i},{j}] = {actual}, expected {expected}")
        assertDiagonal "uniaxial" uniaxial.opticalProperties.eps [ squared uniaxial.ordinaryIndex; squared uniaxial.ordinaryIndex; squared uniaxial.extraordinaryIndex ]
        assertDiagonal "triclinic" triclinic.opticalProperties.eps [ squared triclinic.index1; squared triclinic.index2; squared triclinic.index3 ]
        let absorbingEps = absorbing.opticalProperties.eps
        let actual11 = absorbingEps.[0, 0]
        let actual12 = absorbingEps.[0, 1]
        let expected11 = Complex(squared absorbing.index1, absorbing.epsIm11.value)
        let expected12 = Complex(0.0, absorbing.epsIm12.value)
        Assert.True(closeC 1e-15 actual11 expected11, $"absorbing: eps[0,0] = {actual11}, expected n1² + i·e11 = {expected11}")
        Assert.True(closeC 1e-15 actual12 expected12, $"absorbing: eps[0,1] = {actual12}, expected i·e12 = {expected12}")

    [<Fact>]
    let ``a complexity physics composes, classifies and offers its complexity as the complexity does`` () =
        // Spec 0050 S11.1: each shared member of a complexity physics answers what its complexity answers.
        let constantIsotropic =
            {
                eps = EpsWithoutDispValue (IsotropicTransparent (RefractionIndex 1.4))
                magnetic = None
                active = None
            }
        let uniaxialDispersive = derived (applied [ ChooseAnisotropy Uniaxial; SetDispersion DispersiveSegments ])
        match uniaxialDispersive.eps with
        | EpsWithDispValue (UniaxialDispersive _) -> ()
        | other -> Assert.Fail($"expected a uniaxial dispersive eps, got %A{other}")
        let w = WaveLength.nm 600.0<nm>
        for complexity, anisotropy in [ constantIsotropic, Isotropic; uniaxialDispersive, Uniaxial ] do
            let physics = ComplexityPhysics complexity
            Assert.True(
                propertiesClose 0.0 (complexity.toProperties.getProperties w) (physics.toProperties.getProperties w),
                $"the physics must compose its complexity's properties at λ={w}")
            Assert.Equal(anisotropy, complexity.anisotropy)
            Assert.Equal(complexity.anisotropy, physics.anisotropy)
            Assert.Equal<MaterialComplexity option>(Some complexity, physics.editableComplexityOpt)
        let publishedGyration = { constantIsotropic with active = Some (PublishedLawRho LangasiteGyrationLaw) }
        Assert.Equal<MaterialComplexity option>(None, (ComplexityPhysics publishedGyration).editableComplexityOpt)
