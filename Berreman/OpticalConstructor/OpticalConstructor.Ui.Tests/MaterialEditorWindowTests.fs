namespace OpticalConstructor.Ui.Tests

open System
open Avalonia
open Avalonia.Controls
open Avalonia.Headless
open Avalonia.Threading
open Avalonia.VisualTree
open Xunit
open Berreman.Constants
open Berreman.Fields
open Berreman.MathNetNumericsMath
open Berreman.MaterialProperties
open Berreman.Dispersion
open OpticalConstructor.Domain.Units
open OpticalConstructor.Domain.DispersionModels
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.MaterialComplexityEditor
open OpticalConstructor.TestWindows
open OpticalConstructor.TestWindows.MaterialEditorView

/// Spec 0033 (023) — the MaterialEditorWindow component (UICOMP_XDUO_0004): the Material editor
/// window in OpticalConstructor.TestWindows over the pure Domain `MaterialComplexityEditor`
/// (the progressive-unlock ladder). Two layers, the 022 precedent: pure contract tests for the
/// windowless edit model / view update, and headless semantic-tree proofs that DRIVE THE REAL
/// WINDOW BY ITS UiIds — the slice acceptance: choosing biaxial exposes three principal-index
/// fields, enabling activity on a uniaxial medium offers only the uniaxial gyration classes,
/// unchecking a toggle restores the default model losslessly, and Save round-trips the entry
/// through `MaterialProxy` with `complexity = Some`. Declared, not wired.
module MaterialEditorWindowTests =

    /// A control matches `id` by its `Name` OR its `AutomationProperties.AutomationId` (the
    /// ladder panels are variable-membership subtrees, so their controls carry AutomationIds —
    /// the SampleEditorWindow precedent).
    let private matchesId (id : string) (c : Control) : bool =
        c.Name = id || Avalonia.Automation.AutomationProperties.GetAutomationId(c) = id

    let private tryFindControl (window : Window) (id : string) : Control option =
        window.GetVisualDescendants()
        |> Seq.tryPick (function :? Control as c when matchesId id c -> Some c | _ -> None)

    let private isPresent (window : Window) (id : string) : bool =
        match tryFindControl window id with
        | Some _ -> true
        | None -> false

    /// Click the centre of the clickable Border carrying `id` (by Name or AutomationId).
    let private clickOn (window : Window) (id : string) : unit =
        let found =
            window.GetVisualDescendants()
            |> Seq.tryPick (function :? Border as b when matchesId id b && b.IsEffectivelyVisible -> Some b | _ -> None)
        match found with
        | None -> Assert.Fail($"%s{id} was not found (or not visible)")
        | Some b ->
            let c = b.TranslatePoint(Point(b.Bounds.Width / 2.0, b.Bounds.Height / 2.0), window)
            if c.HasValue then
                window.MouseDown(c.Value, Avalonia.Input.MouseButton.Left, Avalonia.Input.RawInputModifiers.None)
                Dispatcher.UIThread.RunJobs()
                // A Save / Cancel click closes the window during the press — skip the release then.
                if window.IsVisible then
                    window.MouseUp(c.Value, Avalonia.Input.MouseButton.Left, Avalonia.Input.RawInputModifiers.None)
                    Dispatcher.UIThread.RunJobs()
            else Assert.Fail($"%s{id} has no on-screen position")

    /// Set the text of the TextBox carrying `id` (fires the property-change subscription the
    /// view's `onTextChanged` binds — still driving the control found by its UiId).
    let private setText (window : Window) (id : string) (text : string) : unit =
        match tryFindControl window id with
        | Some (:? TextBox as tb) ->
            tb.Text <- text
            Dispatcher.UIThread.RunJobs()
        | Some c -> Assert.Fail($"%s{id} is a %s{c.GetType().Name}, not a TextBox")
        | None -> Assert.Fail($"%s{id} was not found")

    /// The text of the TextBlock carrying `id`.
    let private textOf (window : Window) (id : string) : string =
        match tryFindControl window id with
        | Some (:? TextBlock as tb) -> tb.Text
        | Some c -> failwith $"%s{id} is a %s{c.GetType().Name}, not a TextBlock"
        | None -> failwith $"%s{id} was not found in the visual tree"

    let private close (a : float) (b : float) : bool = abs (a - b) <= 1.0e-9

    /// Fresh, isolated in-memory stores per test (the composition the App would perform).
    let private freshProxies () : MaterialProxy * SampleProxy =
        let samples = SampleProxy.createInMemory ()
        let materials = MaterialProxy.createInMemory (samplesReferencing samples)
        materials, samples

    let private builtIn (id : MaterialId) : MaterialEntry =
        builtInEntries |> List.find (fun e -> e.id = id)

    /// A recording stub context (the functional-proxy seam — the test substitutes in-memory
    /// stubs of the SAME shape and observes which proxy function a Save/Cancel reached, and
    /// with WHAT entry).
    let private recordingContext () : ResizeArray<string> * ResizeArray<MaterialEntry> * MaterialEditorContext =
        let calls = ResizeArray<string>()
        let saved = ResizeArray<MaterialEntry>()
        let stub : MaterialProxy =
            {
                listMaterials = fun () -> Ok []
                searchMaterials = fun _ -> Ok []
                tryGetMaterial = fun _ -> Ok None
                addMaterial = fun e -> calls.Add("add:" + e.name); saved.Add e; Ok ()
                updateMaterial = fun e -> calls.Add("update:" + e.name); saved.Add e; Ok ()
                removeMaterial = fun _ -> Ok ()
            }
        calls, saved, { materials = stub; categories = CategoryProxy.createInMemory (fun _ -> []); requestClose = fun () -> calls.Add "close" }

    /// Apply one edit message, failing the test on an unexpected typed rejection.
    let private applyOk (msg : MaterialComplexityMsg) (s : MaterialComplexityEditState) : MaterialComplexityEditState =
        match applyMaterialComplexityMsg msg s with
        | Ok next -> next
        | Error e -> failwith $"unexpected edit rejection: %A{e}"

    /// Fold a message list over the default edit state.
    let private applied (msgs : MaterialComplexityMsg list) : MaterialComplexityEditState =
        msgs |> List.fold (fun s m -> applyOk m s) defaultState

    /// Derive the complexity, failing the test on an unexpected typed rejection.
    let private derived (s : MaterialComplexityEditState) : MaterialComplexity =
        match toComplexity s with
        | Ok c -> c
        | Error e -> failwith $"expected a derivable complexity, got %A{e}"

    let private modelOfKind (code : string) : DispersionModel =
        defaultModelChoices |> List.find (fun m -> modelKindCode m = code)

    let private newModel () : Model =
        let _, _, context = recordingContext ()
        init context None

    // ============================ pure control contract ============================

    [<Fact>]
    let ``the Material editor UiIds are the slice-mandated stable ids`` () =
        Assert.Equal("MaterialEditorWindow", UiIds.window)
        Assert.Equal("MaterialNameBox", UiIds.nameBox)
        Assert.Equal("AnisotropyToggle", UiIds.anisotropyToggle)
        Assert.Equal("AbsorbingToggle", UiIds.absorbingToggle)
        Assert.Equal("DispersiveToggle", UiIds.dispersiveToggle)
        Assert.Equal("ActiveToggle", UiIds.activeToggle)
        Assert.Equal("MagneticToggle", UiIds.magneticToggle)
        Assert.Equal("GyrationClassPicker", UiIds.gyrationClassPicker)
        Assert.Equal("HandednessSwitch", UiIds.handednessSwitch)
        Assert.Equal("DispersionModelPicker", UiIds.dispersionModelPicker)
        Assert.Equal("AddSegmentButton", UiIds.addSegmentButton)
        Assert.Equal("MaterialEditorSaveButton", UiIds.saveButton)
        Assert.Equal("MaterialEditorCancelButton", UiIds.cancelButton)
        // The derived id families are prefixed so they cannot collide; segment 0's model
        // picker carries the mandated literal (the 022 RepeatCountStepper precedent).
        Assert.Equal("PrincipalIndexBox_2", UiIds.indexBox 2)
        Assert.Equal("AbsorptionIndexBox_1", UiIds.absorptionBox 1)
        Assert.Equal("DispersionModelPicker", UiIds.segmentModelPicker 0)
        Assert.Equal("DispersionModelPicker_1", UiIds.segmentModelPicker 1)
        Assert.Equal("GyrationClassOption_Uniaxial", UiIds.gyrationClassOption "Uniaxial")
        Assert.Equal("AnisotropyOption_Biaxial", UiIds.anisotropyOption (anisotropyCode Biaxial))
        // Spec 0035 (011): the two Constant/Dispersive component sub-toggles and the
        // per-component dispersion-formula editor / box id families.
        Assert.Equal("ActivityDispersiveToggle", UiIds.activityDispersiveToggle)
        Assert.Equal("MagneticDispersiveToggle", UiIds.magneticDispersiveToggle)
        Assert.Equal("GyrationFormulaEditor_g11", UiIds.gyrationComponentFormulaEditor "g11")
        Assert.Equal("GyrationFormulaBox_g33_nt0c0", UiIds.gyrationComponentFormulaBox "g33" "nt0c0")
        Assert.Equal("PolderFormulaEditor_muGyration", UiIds.polderComponentFormulaEditor "muGyration")
        Assert.Equal("PolderFormulaBox_muDiagonal_nt0c0", UiIds.polderComponentFormulaBox "muDiagonal" "nt0c0")

    [<Fact>]
    let ``the default edit state derives the simplest material: transparent, isotropic, non-dispersive`` () =
        let c = derived defaultState
        Assert.Equal<MaterialComplexity>(defaultComplexity, c)
        match c.eps with
        | EpsWithoutDispValue (IsotropicTransparent (RefractionIndex n)) -> Assert.True(close n 1.5, $"n = %g{n}")
        | other -> Assert.Fail($"expected the isotropic transparent constant case, got %A{other}")
        match c.magnetic with
        | None -> ()
        | Some m -> Assert.Fail($"the default magnetic aspect must be absent, got %A{m}")
        match c.active with
        | None -> ()
        | Some a -> Assert.Fail($"the default active aspect must be absent, got %A{a}")

    [<Fact>]
    let ``choosing biaxial with absorbing derives BiaxialAbsorbing over the entered indices`` () =
        let st =
            applied
                [
                    ChooseAnisotropy Biaxial
                    SetTransparency Absorbing
                    SetPrincipalIndex (FirstAxis, ComplexRefractionIndex (createComplex 1.6 0.1))
                    SetPrincipalIndex (SecondAxis, ComplexRefractionIndex (createComplex 1.7 0.2))
                    SetPrincipalIndex (ThirdAxis, ComplexRefractionIndex (createComplex 1.8 0.3))
                ]
        match (derived st).eps with
        | EpsWithoutDispValue (BiaxialAbsorbing (n1, n2, n3)) ->
            Assert.Equal(createComplex 1.6 0.1, n1.value)
            Assert.Equal(createComplex 1.7 0.2, n2.value)
            Assert.Equal(createComplex 1.8 0.3, n3.value)
        | other -> Assert.Fail($"expected BiaxialAbsorbing, got %A{other}")

    [<Fact>]
    let ``uniaxial transparent derives the (ordinary, extraordinary) constant case`` () =
        let st =
            applied
                [
                    ChooseAnisotropy Uniaxial
                    SetPrincipalIndex (FirstAxis, ComplexRefractionIndex (createComplex 1.5 0.0))
                    SetPrincipalIndex (SecondAxis, ComplexRefractionIndex (createComplex 1.65 0.0))
                ]
        match (derived st).eps with
        | EpsWithoutDispValue (UniaxialTransparent (RefractionIndex nO, RefractionIndex nE)) ->
            Assert.True(close nO 1.5 && close nE 1.65, $"nO = %g{nO}, nE = %g{nE}")
        | other -> Assert.Fail($"expected UniaxialTransparent, got %A{other}")

    [<Fact>]
    let ``the dispersive toggle swaps the constant eps for the segment tree and back losslessly`` () =
        let baseline = derived defaultState
        let st = applied [ SetDispersion DispersiveSegments ]
        match (derived st).eps with
        | EpsWithDispValue (IsotropicDispersive [ seg ]) ->
            // The default segment carries the ConstantNK default: a flat n = 1.5, k = 0 line.
            let (ComplexRefractionIndex nk) = seg.dispersion.complexIndex (WaveLength.nm 550.0<nm>)
            Assert.True(close nk.Real 1.5 && close nk.Imaginary 0.0, $"n+ik = %A{nk}")
        | other -> Assert.Fail($"expected one isotropic dispersive segment, got %A{other}")
        let back = applyOk (SetDispersion NonDispersive) st
        Assert.Equal<MaterialComplexity>(baseline, derived back)

    [<Fact>]
    let ``AddSegment appends, segment edits target by index, and a bad index is a typed error`` () =
        let st = applied [ SetDispersion DispersiveSegments; AddSegment ]
        Assert.Equal(2, List.length st.segments)
        let interval = { lower = toWaveLength Nanometer 700.0; upper = toWaveLength Nanometer 1000.0 }
        let edited = applyOk (SetSegmentInterval (1, interval)) st
        Assert.Equal<WaveLengthInterval>(interval, edited.segments.[1].interval)
        match applyMaterialComplexityMsg (SetSegmentInterval (5, interval)) st with
        | Error (NoSuchSegment reason) -> Assert.Contains("5", reason)
        | other -> Assert.Fail($"expected NoSuchSegment, got %A{other}")
        match applyMaterialComplexityMsg (RemoveSegment 0) (applied [ SetDispersion DispersiveSegments ]) with
        | Error (LastSegmentNotRemovable _) -> ()
        | other -> Assert.Fail($"expected LastSegmentNotRemovable, got %A{other}")
        let removed = applyOk (RemoveSegment 1) st
        Assert.Equal(1, List.length removed.segments)

    [<Fact>]
    let ``picking Sellmeier lowers it via toEpsAxis into the segment tree`` () =
        let sellmeier = modelOfKind "Sellmeier"
        let st = applied [ SetDispersion DispersiveSegments; ChooseSegmentModel (0, sellmeier) ]
        match (derived st).eps with
        | EpsWithDispValue (IsotropicDispersive [ seg ]) ->
            match toEpsAxis sellmeier with
            | Ok expected -> Assert.Equal<EpsAxisDispersion>(expected, seg.dispersion)
            | Error e -> failwith $"the Sellmeier default must lower, got %A{e}"
        | other -> Assert.Fail($"expected one isotropic dispersive segment, got %A{other}")

    [<Fact>]
    let ``ForouhiBloomer and BrendelBormann are pickable but surface the typed NotAFiniteTermSum reason`` () =
        for code in [ "ForouhiBloomer"; "BrendelBormann" ] do
            let st = applied [ SetDispersion DispersiveSegments; ChooseSegmentModel (0, modelOfKind code) ]
            Assert.Equal(code, modelKindCode st.segments.[0].model1)
            match toComplexity st with
            | Error (SegmentNotLowerable reason) -> Assert.Contains(code, reason)
            | other -> Assert.Fail($"expected the typed lowering rejection for %s{code}, got %A{other}")

    [<Fact>]
    let ``the raw SumOfTerms escape hatch is the identity under lowering`` () =
        let sumOfTerms = modelOfKind "SumOfTerms"
        let st = applied [ SetDispersion DispersiveSegments; ChooseSegmentModel (0, sumOfTerms) ]
        match (derived st).eps, sumOfTerms with
        | EpsWithDispValue (IsotropicDispersive [ seg ]), SumOfTerms axis ->
            Assert.Equal<EpsAxisDispersion>(axis, seg.dispersion)
        | other, _ -> Assert.Fail($"expected the raw axis data verbatim, got %A{other}")

    [<Fact>]
    let ``availableGyrationClasses is constrained by the anisotropy choice`` () =
        Assert.Equal<string list>([ "Cubic" ], availableGyrationClasses Isotropic |> List.map gyrationClassCode)
        Assert.Equal<string list>([ "Uniaxial" ], availableGyrationClasses Uniaxial |> List.map gyrationClassCode)
        // spec 0033 gap G9: biaxial optics ⇐ orthorhombic (222 AND mm2/Planar) / monoclinic /
        // triclinic. `Planar` (mm2) was previously unreachable from every anisotropy.
        Assert.Equal<string list>(
            [ "Orthorhombic222"; "Planar"; "Monoclinic2"; "MonoclinicM"; "Triclinic1" ],
            availableGyrationClasses Biaxial |> List.map gyrationClassCode)

    [<Fact>]
    let ``enabling activity snaps an un-offered class to the first offered and derives the gyration rho`` () =
        let st = applied [ ChooseAnisotropy Uniaxial; SetActivity ActivityOn ]
        Assert.Equal("Uniaxial", gyrationClassCode st.gyration)
        match (derived st).active with
        | Some (RhoWithoutDispValue g) ->
            Assert.Equal(RightHanded, g.hand)
            match g.gyration with
            | UniaxialActive u ->
                Assert.Equal<RhoValue>(defaultGyrationComponent, u.g11)
                Assert.Equal<RhoValue>(defaultGyrationComponent, u.g33)
            | other -> Assert.Fail($"expected the uniaxial diagonal class, got %A{other}")
        | other -> Assert.Fail($"expected a constant gyration rho, got %A{other}")
        let left = applyOk (SetHandedness LeftHanded) st
        match (derived left).active with
        | Some (RhoWithoutDispValue g) -> Assert.Equal(LeftHanded, g.hand)
        | other -> Assert.Fail($"expected a constant gyration rho, got %A{other}")

    [<Fact>]
    let ``spec 0033 G9: SetGyrationComponent edits a symmetry-allowed component, others are no-ops`` () =
        // A uniaxial active medium's class carries exactly g11 and g33.
        let st = applied [ ChooseAnisotropy Uniaxial; SetActivity ActivityOn ]
        Assert.Equal<string list>([ "g11"; "g33" ], gyrationComponents st.gyration |> List.map (fst >> gyrationComponentCode))
        // Editing g33 changes only g33; g11 is untouched.
        let edited = applyOk (SetGyrationComponent (G33, RhoValue 7.0e-5)) st
        match edited.gyration with
        | UniaxialActive u ->
            Assert.Equal<RhoValue>(defaultGyrationComponent, u.g11)
            Assert.Equal<RhoValue>(RhoValue 7.0e-5, u.g33)
        | other -> Assert.Fail($"expected UniaxialActive, got %A{other}")
        // A component the class's symmetry does not admit is a no-op.
        let noop = applyOk (SetGyrationComponent (G12, RhoValue 1.0)) st
        Assert.Equal(st.gyration, noop.gyration)

    [<Fact>]
    let ``spec 0033 G9: PlanarActive (mm2) is reachable under biaxial and round-trips`` () =
        // The seeded active crystal is biaxial + PlanarActive; it must derive (and thus be
        // editable, not view-only) now that mm2 is offered under biaxial.
        let st = applied [ ChooseAnisotropy Biaxial; SetActivity ActivityOn; ChooseGyrationClass (PlanarActive (RhoValue 1.5e-6)) ]
        Assert.Equal("Planar", gyrationClassCode st.gyration)
        match (derived st).active with
        | Some (RhoWithoutDispValue g) ->
            match g.gyration with
            | PlanarActive (RhoValue v) -> Assert.Equal(1.5e-6, v)
            | other -> Assert.Fail($"expected PlanarActive, got %A{other}")
        | other -> Assert.Fail($"expected a constant gyration rho, got %A{other}")

    [<Fact>]
    let ``spec 0033 G7: SetSegmentModel stores new coefficients verbatim where a same-kind re-pick would not`` () =
        let st = applied [ SetDispersion DispersiveSegments ]
        let ninefold = ConstantNK { n = 9.0; k = 0.0; wavelengthUnit = Nanometer; thermoOptic = None }
        // A same-kind ChooseSegmentModel keeps the segment's current (default) coefficients…
        let repick = applyOk (ChooseSegmentModel (0, ninefold)) st
        // …so a coefficient edit must use SetSegmentModel, which applies them verbatim.
        let edited = applyOk (SetSegmentModel (0, ninefold)) st
        match (List.head repick.segments).model1, (List.head edited.segments).model1 with
        | ConstantNK a, ConstantNK b ->
            Assert.Equal(defaultIndexValue, a.n)
            Assert.Equal(9.0, b.n)
        | other -> Assert.Fail($"expected ConstantNK segments, got %A{other}")

    [<Fact>]
    let ``spec 0033 G7: modelParameters exposes and rebuilds each editable coefficient`` () =
        let model = ConstantNK { n = 1.5; k = 0.0; wavelengthUnit = Nanometer; thermoOptic = None }
        let ps = modelParameters model
        Assert.Equal<string list>([ "n"; "k" ], ps |> List.map (fun p -> p.key))
        match (ps |> List.find (fun p -> p.key = "n")).update 2.7 with
        | ConstantNK c -> Assert.Equal(2.7, c.n)
        | other -> Assert.Fail($"expected ConstantNK, got %A{other}")

    [<Fact>]
    let ``the magnetic unlock derives scalar and gyromagnetic Polder mu with the axis`` () =
        let scalar = applied [ SetMagnetic MagneticOn ]
        match (derived scalar).magnetic with
        | Some (MuWithoutDispValue (ScalarMu (MuValue m))) -> Assert.True(close m 1.0, $"mu = %g{m}")
        | other -> Assert.Fail($"expected the scalar constant mu, got %A{other}")
        let gyro =
            applied
                [
                    SetMagnetic MagneticOn
                    SetMuKind GyromagneticMuKind
                    SetMuDiagonal (MuValue 1.2)
                    SetMuParallel (MuValue 1.1)
                    SetMuGyration (MuValue 0.2)
                    ChooseGyrationAxis AlongX
                ]
        match (derived gyro).magnetic with
        | Some (MuWithoutDispValue (GyromagneticMu p)) ->
            Assert.Equal<MuValue>(MuValue 1.2, p.muDiagonal)
            Assert.Equal<MuValue>(MuValue 1.1, p.muParallel)
            Assert.Equal<MuValue>(MuValue 0.2, p.gyration)
            Assert.Equal(AlongX, p.axis)
        | other -> Assert.Fail($"expected the gyromagnetic Polder mu, got %A{other}")

    [<Fact>]
    let ``acceptance: unchecking a toggle restores the default model losslessly and re-checking restores the edits`` () =
        let baseline = derived defaultState
        // Absorbing: unlock, edit k, uncheck — the default returns; re-check — the edit returns.
        let absorbing = applied [ SetTransparency Absorbing; SetPrincipalIndex (FirstAxis, ComplexRefractionIndex (createComplex 1.5 0.2)) ]
        Assert.NotEqual<MaterialComplexity>(baseline, derived absorbing)
        let absorbingOff = applyOk (SetTransparency Transparent) absorbing
        Assert.Equal<MaterialComplexity>(baseline, derived absorbingOff)
        Assert.Equal<MaterialComplexity>(derived absorbing, derived (applyOk (SetTransparency Absorbing) absorbingOff))
        // Dispersive: unlock (segment list), uncheck — the constant default returns.
        let dispersive = applied [ SetDispersion DispersiveSegments; ChooseSegmentModel (0, modelOfKind "Sellmeier") ]
        Assert.NotEqual<MaterialComplexity>(baseline, derived dispersive)
        let dispersiveOff = applyOk (SetDispersion NonDispersive) dispersive
        Assert.Equal<MaterialComplexity>(baseline, derived dispersiveOff)
        Assert.Equal<MaterialComplexity>(derived dispersive, derived (applyOk (SetDispersion DispersiveSegments) dispersiveOff))
        // Optically active: unlock, flip handedness, uncheck, re-check.
        let active = applied [ SetActivity ActivityOn; SetHandedness LeftHanded ]
        Assert.NotEqual<MaterialComplexity>(baseline, derived active)
        let activeOff = applyOk (SetActivity ActivityOff) active
        Assert.Equal<MaterialComplexity>(baseline, derived activeOff)
        Assert.Equal<MaterialComplexity>(derived active, derived (applyOk (SetActivity ActivityOn) activeOff))
        // Magnetic: unlock gyromagnetic on a transverse axis, uncheck, re-check.
        let magnetic = applied [ SetMagnetic MagneticOn; SetMuKind GyromagneticMuKind; ChooseGyrationAxis AlongX ]
        Assert.NotEqual<MaterialComplexity>(baseline, derived magnetic)
        let magneticOff = applyOk (SetMagnetic MagneticOff) magnetic
        Assert.Equal<MaterialComplexity>(baseline, derived magneticOff)
        Assert.Equal<MaterialComplexity>(derived magnetic, derived (applyOk (SetMagnetic MagneticOn) magneticOff))

    [<Fact>]
    let ``ofComplexity round-trips the editable built-ins value-identically through toComplexity`` () =
        let editable =
            [
                MaterialIds.glass152
                MaterialIds.uniaxialCrystal
                MaterialIds.biaxialCrystal
                MaterialIds.euvMolybdenum
                MaterialIds.activeCrystal
            ]
        for id in editable do
            let entry = builtIn id
            match entry.complexity with
            | Some c ->
                match ofComplexity c with
                | Ok st -> Assert.Equal<Result<MaterialComplexity, MaterialComplexityEditError>>(Ok c, toComplexity st)
                | Error e -> Assert.Fail($"%s{entry.name} must seed the editor, got %A{e}")
            | None -> Assert.Fail($"%s{entry.name} must carry an edit model")

    [<Fact>]
    let ``the imaginaryIndexGainWarning rule flags a finite negative k only`` () =
        match imaginaryIndexGainWarning -0.1 with
        | Some message -> Assert.Contains("gain", message)
        | None -> Assert.Fail("a finite negative k implies gain and must warn")
        for k in [ 0.0; 0.1; nan; infinity; -infinity ] do
            match imaginaryIndexGainWarning k with
            | None -> ()
            | Some message -> Assert.Fail($"k = %g{k} must not warn, got '%s{message}'")

    [<Fact>]
    let ``init seeds a new material, an existing editable entry, and a view-only entry`` () =
        let m = newModel ()
        Assert.Equal(NewMaterial, m.target)
        Assert.Equal(EditableMaterial, m.mode)
        Assert.Equal("", m.name)
        Assert.Equal<MaterialComplexityEditState>(defaultState, m.editor)
        // An existing editable entry seeds the ladder from its complexity.
        let glass = builtIn MaterialIds.glass152
        let _, _, context = recordingContext ()
        let existing = init context (Some glass)
        Assert.Equal(ExistingMaterial glass.id, existing.target)
        Assert.Equal(glass.name, existing.name)
        Assert.Equal(EditableMaterial, existing.mode)
        match glass.complexity with
        | Some c -> Assert.Equal<Result<MaterialComplexity, MaterialComplexityEditError>>(Ok c, toComplexity existing.editor)
        | None -> Assert.Fail("glass must carry an edit model")
        // A complexity-less entry (engine-coded physics) opens view-only.
        let silicon = init context (Some (builtIn MaterialIds.silicon))
        match silicon.mode with
        | ViewOnlyMaterial _ -> ()
        | EditableMaterial -> Assert.Fail("silicon (complexity = None) must open view-only")

    [<Fact>]
    let ``Save adds a NEW material with complexity Some, updates an EXISTING one, and Cancel writes nothing`` () =
        // New → addMaterial minting a fresh id, then close.
        let calls, saved, context = recordingContext ()
        init context None
        |> update (SetName "Fresh")
        |> update SaveClicked
        |> ignore
        Assert.Equal<string list>([ "add:Fresh"; "close" ], List.ofSeq calls)
        match saved.[0].complexity with
        | Some c ->
            Assert.Equal<MaterialComplexity>(defaultComplexity, c)
            // properties IS complexity.toProperties — the sync invariant at save time.
            let eps = saved.[0].properties.epsWithDisp.getEps (WaveLength.nm 600.0<nm>)
            Assert.True(close (eps.[0, 0].Real) (1.5 * 1.5), $"eps11 = %A{eps.[0, 0]}")
        | None -> Assert.Fail("Save must store complexity = Some model")
        // Existing → updateMaterial under the SAME id, then close.
        let calls2, saved2, context2 = recordingContext ()
        let glass = builtIn MaterialIds.glass152
        init context2 (Some glass)
        |> update (SetName "Edited")
        |> update SaveClicked
        |> ignore
        Assert.Equal<string list>([ "update:Edited"; "close" ], List.ofSeq calls2)
        Assert.Equal(glass.id, saved2.[0].id)
        // Cancel → close only; the proxy is never reached.
        let calls3, _, context3 = recordingContext ()
        init context3 (Some glass)
        |> update (SetName "Discarded")
        |> update CancelClicked
        |> ignore
        Assert.Equal<string list>([ "close" ], List.ofSeq calls3)

    [<Fact>]
    let ``a failing save keeps the window open and surfaces the proxy's reason`` () =
        let closes = ResizeArray<string>()
        let failing : MaterialProxy =
            {
                listMaterials = fun () -> Ok []
                searchMaterials = fun _ -> Ok []
                tryGetMaterial = fun _ -> Ok None
                addMaterial = fun _ -> Error (InvalidMaterial "the name is blank")
                updateMaterial = fun _ -> Error (InvalidMaterial "the name is blank")
                removeMaterial = fun _ -> Ok ()
            }
        let context : MaterialEditorContext = { materials = failing; categories = CategoryProxy.createInMemory (fun _ -> []); requestClose = fun () -> closes.Add "close" }
        let m = init context None |> update SaveClicked
        Assert.Empty(closes)
        match m.status with
        | Some reason -> Assert.Equal("the name is blank", reason)
        | None -> Assert.Fail("expected the proxy's typed reason as the status")

    // ============================ headless semantic-tree proofs ============================

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the window mounts with every slice-mandated UiId present`` () =
        HeadlessSession.run (fun () ->
            let materials, _ = freshProxies ()
            let window = MaterialEditorWindow(materials, None)
            window.Show()
            Dispatcher.UIThread.RunJobs()
            Assert.True(matchesId UiIds.window window, "the window itself carries the MaterialEditorWindow id")
            for id in
                [
                    UiIds.nameBox
                    UiIds.anisotropyToggle
                    UiIds.absorbingToggle
                    UiIds.dispersiveToggle
                    UiIds.activeToggle
                    UiIds.magneticToggle
                    UiIds.saveButton
                    UiIds.cancelButton
                ] do
                Assert.True(isPresent window id, $"%s{id} is missing from the mounted window")
            // The segment editor's mandated ids appear once the dispersive rung unlocks…
            clickOn window UiIds.dispersiveToggle
            Assert.True(isPresent window UiIds.dispersionModelPicker, "the dispersive rung must expose the model picker")
            Assert.True(isPresent window UiIds.addSegmentButton, "the dispersive rung must expose the add-segment verb")
            // …and the gyration panel's once the activity rung unlocks.
            clickOn window UiIds.activeToggle
            Assert.True(isPresent window UiIds.gyrationClassPicker, "the activity rung must expose the class picker")
            Assert.True(isPresent window UiIds.handednessSwitch, "the activity rung must expose the handedness switch")
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``acceptance: choosing biaxial exposes three principal-index fields`` () =
        HeadlessSession.run (fun () ->
            let materials, _ = freshProxies ()
            let window = MaterialEditorWindow(materials, None)
            window.Show()
            Dispatcher.UIThread.RunJobs()
            // Isotropic (the default): one index field.
            Assert.True(isPresent window (UiIds.indexBox 1))
            Assert.False(isPresent window (UiIds.indexBox 2))
            Assert.False(isPresent window (UiIds.indexBox 3))
            // Uniaxial: ordinary + extraordinary.
            clickOn window (UiIds.anisotropyOption (anisotropyCode Uniaxial))
            Assert.True(isPresent window (UiIds.indexBox 2))
            Assert.False(isPresent window (UiIds.indexBox 3))
            // Biaxial: three principal-index fields.
            clickOn window (UiIds.anisotropyOption (anisotropyCode Biaxial))
            Assert.True(isPresent window (UiIds.indexBox 1))
            Assert.True(isPresent window (UiIds.indexBox 2))
            Assert.True(isPresent window (UiIds.indexBox 3))
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``acceptance: enabling activity on a uniaxial medium offers only the uniaxial gyration classes`` () =
        HeadlessSession.run (fun () ->
            let materials, _ = freshProxies ()
            let window = MaterialEditorWindow(materials, None)
            window.Show()
            Dispatcher.UIThread.RunJobs()
            clickOn window (UiIds.anisotropyOption (anisotropyCode Uniaxial))
            clickOn window UiIds.activeToggle
            Assert.True(isPresent window UiIds.gyrationClassPicker)
            Assert.True(isPresent window UiIds.handednessSwitch)
            Assert.True(isPresent window (UiIds.gyrationClassOption "Uniaxial"), "the uniaxial diagonal class must be offered")
            for offLimits in [ "Cubic"; "Planar"; "Orthorhombic222"; "Monoclinic2"; "MonoclinicM"; "Triclinic1" ] do
                Assert.False(isPresent window (UiIds.gyrationClassOption offLimits), $"%s{offLimits} must NOT be offered for a uniaxial medium")
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``acceptance: unchecking a toggle restores the default model losslessly`` () =
        HeadlessSession.run (fun () ->
            let materials, _ = freshProxies ()
            let window = MaterialEditorWindow(materials, None)
            window.Show()
            Dispatcher.UIThread.RunJobs()
            let initial = textOf window UiIds.summaryText
            // Absorbing on, k edited — the derived model changes; unchecking restores it.
            clickOn window UiIds.absorbingToggle
            setText window (UiIds.absorptionBox 1) "0.25"
            Assert.NotEqual<string>(initial, textOf window UiIds.summaryText)
            clickOn window UiIds.absorbingToggle
            Assert.Equal(initial, textOf window UiIds.summaryText)
            // The dispersive rung restores the same way.
            clickOn window UiIds.dispersiveToggle
            Assert.NotEqual<string>(initial, textOf window UiIds.summaryText)
            clickOn window UiIds.dispersiveToggle
            Assert.Equal(initial, textOf window UiIds.summaryText)
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``acceptance: Save round-trips a NEW entry through MaterialProxy with complexity Some`` () =
        HeadlessSession.run (fun () ->
            let materials, _ = freshProxies ()
            let seededCount =
                match materials.listMaterials () with
                | Ok all -> List.length all
                | Error e -> failwith $"seed listing failed: %A{e}"
            let window = MaterialEditorWindow(materials, None)
            window.Show()
            Dispatcher.UIThread.RunJobs()
            setText window UiIds.nameBox "Headless material"
            clickOn window (UiIds.anisotropyOption (anisotropyCode Biaxial))
            setText window (UiIds.indexBox 1) "1.6"
            setText window (UiIds.indexBox 2) "1.7"
            setText window (UiIds.indexBox 3) "1.8"
            clickOn window UiIds.saveButton
            Assert.False(window.IsVisible)
            match materials.listMaterials () with
            | Ok all ->
                Assert.Equal(seededCount + 1, List.length all)
                match all |> List.tryFind (fun e -> e.name = "Headless material") with
                | Some savedEntry ->
                    match savedEntry.complexity with
                    | Some c ->
                        match c.eps with
                        | EpsWithoutDispValue (BiaxialTransparent (RefractionIndex n1, RefractionIndex n2, RefractionIndex n3)) ->
                            Assert.True(close n1 1.6 && close n2 1.7 && close n3 1.8, $"n = %g{n1}, %g{n2}, %g{n3}")
                        | other -> Assert.Fail($"expected the biaxial transparent constant case, got %A{other}")
                        // properties IS complexity.toProperties: the stored tensors agree.
                        let eps = savedEntry.properties.epsWithDisp.getEps (WaveLength.nm 600.0<nm>)
                        Assert.True(close (eps.[0, 0].Real) (1.6 * 1.6), $"eps11 = %A{eps.[0, 0]}")
                        Assert.True(close (eps.[1, 1].Real) (1.7 * 1.7), $"eps22 = %A{eps.[1, 1]}")
                        Assert.True(close (eps.[2, 2].Real) (1.8 * 1.8), $"eps33 = %A{eps.[2, 2]}")
                    | None -> Assert.Fail("the saved entry must carry complexity = Some model")
                | None -> Assert.Fail("the new material was not persisted")
            | Error e -> Assert.Fail($"listMaterials failed: %A{e}"))

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``Save UPDATES an existing editable entry in place through MaterialProxy.updateMaterial`` () =
        HeadlessSession.run (fun () ->
            let materials, _ = freshProxies ()
            let existing =
                match materials.tryGetMaterial MaterialIds.glass152 with
                | Ok (Some e) -> e
                | other -> failwith $"glass152 must be seeded, got %A{other}"
            let seededCount =
                match materials.listMaterials () with
                | Ok all -> List.length all
                | Error e -> failwith $"seed listing failed: %A{e}"
            let window = MaterialEditorWindow(materials, Some existing)
            window.Show()
            Dispatcher.UIThread.RunJobs()
            setText window UiIds.nameBox "Renamed glass"
            clickOn window UiIds.saveButton
            Assert.False(window.IsVisible)
            match materials.tryGetMaterial MaterialIds.glass152 with
            | Ok (Some updated) ->
                Assert.Equal("Renamed glass", updated.name)
                match updated.complexity with
                | Some _ -> ()
                | None -> Assert.Fail("the updated entry must keep complexity = Some model")
            | Ok None -> Assert.Fail("the existing entry vanished")
            | Error e -> Assert.Fail($"tryGetMaterial failed: %A{e}")
            match materials.listMaterials () with
            | Ok all -> Assert.Equal(seededCount, List.length all)
            | Error e -> Assert.Fail($"listMaterials failed: %A{e}"))

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``a negative k surfaces the advisory gain warning and a non-negative k clears it`` () =
        HeadlessSession.run (fun () ->
            let materials, _ = freshProxies ()
            let window = MaterialEditorWindow(materials, None)
            window.Show()
            Dispatcher.UIThread.RunJobs()
            Assert.Equal("", textOf window UiIds.gainWarning)
            clickOn window UiIds.absorbingToggle
            setText window (UiIds.absorptionBox 1) "-0.1"
            Assert.Contains("gain", textOf window UiIds.gainWarning)
            setText window (UiIds.absorptionBox 1) "0.1"
            Assert.Equal("", textOf window UiIds.gainWarning)
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``a complexity-less entry opens view-only with no Save affordance and no ladder`` () =
        HeadlessSession.run (fun () ->
            let materials, _ = freshProxies ()
            let window = MaterialEditorWindow(materials, Some (builtIn MaterialIds.silicon))
            window.Show()
            Dispatcher.UIThread.RunJobs()
            Assert.True(isPresent window UiIds.viewOnlyNote, "a view-only entry must say why it cannot be edited")
            Assert.False(isPresent window UiIds.saveButton, "a view-only entry offers NO Save affordance")
            Assert.False(isPresent window UiIds.absorbingToggle, "the ladder is removed, not greyed")
            Assert.False(isPresent window UiIds.activeToggle, "the ladder is removed, not greyed")
            clickOn window UiIds.cancelButton
            Assert.False(window.IsVisible))

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the segment editor adds segments and surfaces the typed reason for a non-lowerable pick`` () =
        HeadlessSession.run (fun () ->
            let materials, _ = freshProxies ()
            let window = MaterialEditorWindow(materials, None)
            window.Show()
            Dispatcher.UIThread.RunJobs()
            clickOn window UiIds.dispersiveToggle
            Assert.True(isPresent window (UiIds.segmentLowerBox 0))
            Assert.False(isPresent window (UiIds.segmentLowerBox 1))
            clickOn window UiIds.addSegmentButton
            Assert.True(isPresent window (UiIds.segmentLowerBox 1), "AddSegment must append a second segment row")
            // A non-lowerable pick is accepted (the entry is preserved) and the typed
            // NotAFiniteTermSum reason surfaces through the derivation readout.
            clickOn window (UiIds.modelOption 0 "ForouhiBloomer")
            Assert.Contains("ForouhiBloomer", textOf window UiIds.summaryText)
            clickOn window (UiIds.modelOption 0 "SumOfTerms")
            Assert.DoesNotContain("ForouhiBloomer", textOf window UiIds.summaryText)
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``acceptance: the activity Dispersive sub-toggle swaps each gyration component for a formula editor and back`` () =
        HeadlessSession.run (fun () ->
            let materials, _ = freshProxies ()
            let window = MaterialEditorWindow(materials, None)
            window.Show()
            Dispatcher.UIThread.RunJobs()
            // A uniaxial active medium's class carries exactly g11 and g33.
            clickOn window (UiIds.anisotropyOption (anisotropyCode Uniaxial))
            clickOn window UiIds.activeToggle
            Assert.True(isPresent window UiIds.activityDispersiveToggle, "the activity rung must expose the Dispersive sub-toggle")
            // Constant (the default sub-branch): the per-component constant boxes, no formula editor.
            for code in [ "g11"; "g33" ] do
                Assert.True(isPresent window (UiIds.gyrationComponentBox code), $"the constant %s{code} box must be present")
                Assert.False(isPresent window (UiIds.gyrationComponentFormulaEditor code), $"%s{code} must have no formula editor while constant")
            // Enabling Dispersive exposes a dispersion-formula editor per symmetry-allowed
            // component; the constant boxes are gone.
            clickOn window UiIds.activityDispersiveToggle
            for code in [ "g11"; "g33" ] do
                Assert.True(isPresent window (UiIds.gyrationComponentFormulaEditor code), $"%s{code} must expose a dispersion-formula editor under Dispersive")
                Assert.False(isPresent window (UiIds.gyrationComponentBox code), $"the constant %s{code} box must be removed under Dispersive")
            // Unchecking restores the constant component boxes.
            clickOn window UiIds.activityDispersiveToggle
            for code in [ "g11"; "g33" ] do
                Assert.True(isPresent window (UiIds.gyrationComponentBox code), $"unchecking must restore the constant %s{code} box")
                Assert.False(isPresent window (UiIds.gyrationComponentFormulaEditor code), $"%s{code} formula editor must be gone again")
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``acceptance: the magnetic Dispersive sub-toggle swaps the Polder components for formula editors and back`` () =
        HeadlessSession.run (fun () ->
            let materials, _ = freshProxies ()
            let window = MaterialEditorWindow(materials, None)
            window.Show()
            Dispatcher.UIThread.RunJobs()
            clickOn window UiIds.magneticToggle
            Assert.True(isPresent window UiIds.magneticDispersiveToggle, "the magnetic rung must expose the Dispersive sub-toggle")
            // Constant (default, scalar kind): the diagonal μ box, no formula editor.
            Assert.True(isPresent window UiIds.muDiagonalBox, "the constant diagonal μ box must be present")
            Assert.False(isPresent window (UiIds.polderComponentFormulaEditor "muDiagonal"), "no Polder formula editor while constant")
            // Enabling Dispersive exposes the full-tensor Polder component formula editors;
            // the constant box is gone.
            clickOn window UiIds.magneticDispersiveToggle
            for code in [ "muDiagonal"; "muParallel"; "muGyration" ] do
                Assert.True(isPresent window (UiIds.polderComponentFormulaEditor code), $"%s{code} must expose a dispersion-formula editor under Dispersive")
            Assert.False(isPresent window UiIds.muDiagonalBox, "the constant diagonal μ box must be removed under Dispersive")
            // Unchecking restores the constant component box.
            clickOn window UiIds.magneticDispersiveToggle
            Assert.True(isPresent window UiIds.muDiagonalBox, "unchecking must restore the constant diagonal μ box")
            for code in [ "muDiagonal"; "muParallel"; "muGyration" ] do
                Assert.False(isPresent window (UiIds.polderComponentFormulaEditor code), $"%s{code} formula editor must be gone again")
            window.Close())
