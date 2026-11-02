namespace OpticalConstructor.Ui.Tests

open System.Numerics
open System
open OpticalConstructor.Controls
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
open OpticalConstructor.Domain.Lifecycle
open OpticalConstructor.Domain.MaterialStore
open OpticalConstructor.Domain.SampleStore
open OpticalConstructor.Domain.MaterialComplexityEditor
open OpticalConstructor.Ui
open OpticalConstructor.Ui.MaterialEditorView

/// Spec 0033 (023) — the MaterialEditorWindow component (UICOMP_XDUO_0004): the Material editor
/// window in OpticalConstructor.Ui over the pure Domain `MaterialComplexityEditor`
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
            // Spec 0046: scroll the box into view first (the facet rows now WRAP, so the tree can sit below
            // the visible area of its scroller, where a click at the box's centre lands nowhere).
            b.BringIntoView() |> ignore
            Dispatcher.UIThread.RunJobs()
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
        let samples = SampleProxy.createInMemory VersionsInUse.empty
        let materials = MaterialProxy.createInMemory (samplesReferencing samples) VersionsInUse.empty
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
                listMaterials = fun _ -> Ok []
                searchMaterials = fun _ -> Ok []
                tryGetMaterial = fun _ -> Ok None
                resolveVersion = fun _ -> Ok None
                saveMaterial = fun e -> calls.Add("save:" + e.name.value); saved.Add e; Ok ()
                markMaterialInactive = fun _ -> Ok ()
                markMaterialActive = fun _ -> Ok ()
                supersedeMaterial = fun _ -> Ok ()
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
        match s.toComplexity with
        | Ok c -> c
        | Error e -> failwith $"expected a derivable complexity, got %A{e}"

    let private modelOfKind (code : string) : DispersionModel =
        defaultModelChoices |> List.find (fun m -> m.kindCode = code)

    let private newModel () : MaterialEditorModel =
        let _, _, context = recordingContext ()
        // The Add-open shape (spec 0038 step 008): the id is minted AT WINDOW OPEN.
        initMaterialEditor context (NewMaterial (newMaterialId ()))

    // -- spec 0038 (032): the tabbed preview's embedded-chart probes (the EmbeddedChartTests precedent) --

    /// The live `AvaPlot` hosted under the control carrying `id`, or `None` if the host degraded.
    let private avaUnder (window : Window) (id : string) : ScottPlot.Avalonia.AvaPlot option =
        match tryFindControl window id with
        | Some host -> host.GetVisualDescendants() |> Seq.tryPick (function :? ScottPlot.Avalonia.AvaPlot as a -> Some a | _ -> None)
        | None -> None

    /// Force ScottPlot's real Skia rasterization — a construction- or render-time throw surfaces here
    /// (headless Avalonia never rasterizes on its own), so this proves the embedded chart renders one
    /// frame without throwing.
    let private rasterizes (ava : ScottPlot.Avalonia.AvaPlot) : bool =
        ava.Plot.GetImage(400, 200).GetImageBytes().Length > 0

    /// Mount a bare `IView` in a sized headless window and render one frame (the `EmbeddedChartTests`
    /// precedent, so the embedded AvaPlot realizes into the visual tree).
    let private mount (view : Avalonia.FuncUI.Types.IView) : Window =
        let window = Window(Width = 700.0, Height = 400.0)
        window.Content <- Avalonia.FuncUI.Component(fun _ctx -> view)
        window.Show()
        Dispatcher.UIThread.RunJobs()
        window

    /// The engine properties derived from a fold of ladder edits (for the per-tab render proofs).
    let private propsFrom (msgs : MaterialComplexityMsg list) : Berreman.Dispersion.OpticalPropertiesWithDisp =
        let st = applied msgs
        match st.toComplexity with
        | Ok c -> c.toProperties
        | Error e -> failwith $"expected a derivable complexity, got %A{e}"

    /// Whether series `i` is visible in a lowered chart-style seed (the `applyHidden` proof). Reads back
    /// the REAL `ChartStyle` visibility, fully qualified: the file must NOT `open` `ChartStyle`, whose
    /// `defaultState` would shadow the `MaterialComplexityEditor.defaultState` used throughout (the
    /// attempt-02 trap). `ChartStyle` is a top-level module in `OpticalConstructor.Controls`.
    let private seriesVisibleAt (i : int) (style : OpticalConstructor.Controls.ChartStyle.ChartStyleState) : bool =
        (OpticalConstructor.Controls.ChartStyle.seriesStyleOf (OpticalConstructor.Controls.ChartStyle.ChartSeriesIndex i) style).visible.isDrawn

    // ============================ pure control contract ============================

    [<Fact>]
    let ``the Material editor UiIds are the slice-mandated stable ids`` () =
        Assert.Equal("MaterialEditorWindow", ConstructorUiId.materialEditorWindow.value)
        Assert.Equal("MaterialNameBox", ConstructorUiId.materialNameBox.value)
        Assert.Equal("AnisotropyToggle", ConstructorUiId.anisotropyToggle.value)
        Assert.Equal("AbsorbingToggle", ConstructorUiId.absorbingToggle.value)
        // Spec 0035 (017): the eps branch is two mutually-exclusive options (Constant / Dispersive).
        Assert.Equal("ConstantToggle", ConstructorUiId.constantToggle.value)
        Assert.Equal("DispersiveToggle", ConstructorUiId.dispersiveToggle.value)
        Assert.Equal("ActiveToggle", ConstructorUiId.activeToggle.value)
        Assert.Equal("MagneticToggle", ConstructorUiId.magneticToggle.value)
        Assert.Equal("GyrationClassPicker", ConstructorUiId.gyrationClassPicker.value)
        Assert.Equal("HandednessSwitch", ConstructorUiId.handednessSwitch.value)
        Assert.Equal("DispersionModelPicker", ConstructorUiId.dispersionModelPicker.value)
        Assert.Equal("AddSegmentButton", ConstructorUiId.addSegmentButton.value)
        Assert.Equal("MaterialEditorSaveButton", ConstructorUiId.materialEditorSaveButton.value)
        Assert.Equal("MaterialEditorCancelButton", ConstructorUiId.materialEditorCancelButton.value)
        // The derived id families are prefixed so they cannot collide; segment 0's model
        // picker carries the mandated literal (the 022 RepeatCountStepper precedent).
        Assert.Equal("PrincipalIndexBox_2", (ConstructorUiId.principalIndexBox (ConstructorUiPosition 2)).value)
        Assert.Equal("AbsorptionIndexBox_1", (ConstructorUiId.absorptionIndexBox (ConstructorUiPosition 1)).value)
        Assert.Equal("DispersionModelPicker", (ConstructorUiId.dispersionModelPickerOfSegment (ConstructorUiPosition 0)).value)
        Assert.Equal("DispersionModelPicker_1", (ConstructorUiId.dispersionModelPickerOfSegment (ConstructorUiPosition 1)).value)
        Assert.Equal("GyrationClassOption_Uniaxial", (ConstructorUiId.gyrationClassOption (ConstructorUiKey "Uniaxial")).value)
        Assert.Equal("AnisotropyOption_Biaxial", (ConstructorUiId.anisotropyOption (ConstructorUiKey (anisotropyCode Biaxial))).value)
        // Spec 0035 (011): the two Constant/Dispersive component sub-toggles and the
        // per-component dispersion-formula editor / box id families.
        Assert.Equal("ActivityDispersiveToggle", ConstructorUiId.activityDispersiveToggle.value)
        Assert.Equal("MagneticDispersiveToggle", ConstructorUiId.magneticDispersiveToggle.value)
        Assert.Equal("GyrationFormulaEditor_g11", (ConstructorUiId.gyrationFormulaEditor (ConstructorUiKey "g11")).value)
        Assert.Equal("GyrationFormulaBox_g33_nt0c0", (ConstructorUiId.gyrationFormulaBox (ConstructorUiKey "g33") (ConstructorUiKey "nt0c0")).value)
        Assert.Equal("PolderFormulaEditor_muGyration", (ConstructorUiId.polderFormulaEditor (ConstructorUiKey "muGyration")).value)
        Assert.Equal("PolderFormulaBox_muDiagonal_nt0c0", (ConstructorUiId.polderFormulaBox (ConstructorUiKey "muDiagonal") (ConstructorUiKey "nt0c0")).value)

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
                    SetPrincipalIndex (FirstAxis, ComplexRefractionIndex (Complex.create 1.6 0.1))
                    SetPrincipalIndex (SecondAxis, ComplexRefractionIndex (Complex.create 1.7 0.2))
                    SetPrincipalIndex (ThirdAxis, ComplexRefractionIndex (Complex.create 1.8 0.3))
                ]
        match (derived st).eps with
        | EpsWithoutDispValue (BiaxialAbsorbing { nxComplex = n1; nyComplex = n2; nzComplex = n3 }) ->
            Assert.Equal(Complex.create 1.6 0.1, n1.value)
            Assert.Equal(Complex.create 1.7 0.2, n2.value)
            Assert.Equal(Complex.create 1.8 0.3, n3.value)
        | other -> Assert.Fail($"expected BiaxialAbsorbing, got %A{other}")

    [<Fact>]
    let ``uniaxial transparent derives the (ordinary, extraordinary) constant case`` () =
        let st =
            applied
                [
                    ChooseAnisotropy Uniaxial
                    SetPrincipalIndex (FirstAxis, ComplexRefractionIndex (Complex.create 1.5 0.0))
                    SetPrincipalIndex (SecondAxis, ComplexRefractionIndex (Complex.create 1.65 0.0))
                ]
        match (derived st).eps with
        | EpsWithoutDispValue (UniaxialTransparent { ordinary = RefractionIndex nO; extraordinary = RefractionIndex nE }) ->
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
        let interval = { lower = toWaveLength Nanometer (UnitMagnitude 700.0); upper = toWaveLength Nanometer (UnitMagnitude 1000.0) }
        let edited = applyOk (SetSegmentInterval (SegmentIndex 1, interval)) st
        Assert.Equal<WaveLengthInterval>(interval, edited.segments.[1].interval)
        match applyMaterialComplexityMsg (SetSegmentInterval (SegmentIndex 5, interval)) st with
        | Error (NoSuchSegmentErr reason) -> Assert.Contains("5", reason)
        | other -> Assert.Fail($"expected NoSuchSegmentErr, got %A{other}")
        match applyMaterialComplexityMsg (RemoveSegment (SegmentIndex 0)) (applied [ SetDispersion DispersiveSegments ]) with
        | Error (LastSegmentNotRemovableErr _) -> ()
        | other -> Assert.Fail($"expected LastSegmentNotRemovableErr, got %A{other}")
        let removed = applyOk (RemoveSegment (SegmentIndex 1)) st
        Assert.Equal(1, List.length removed.segments)

    [<Fact>]
    let ``picking Sellmeier lowers it via toEpsAxis into the segment tree`` () =
        let sellmeier = modelOfKind "Sellmeier"
        let st = applied [ SetDispersion DispersiveSegments; ChooseSegmentModel (SegmentIndex 0, sellmeier) ]
        match (derived st).eps with
        | EpsWithDispValue (IsotropicDispersive [ seg ]) ->
            Assert.Equal<EpsAxisDispersion>(sellmeier.toEpsAxis, seg.dispersion)
        | other -> Assert.Fail($"expected one isotropic dispersive segment, got %A{other}")

    [<Fact>]
    let ``ForouhiBloomer and BrendelBormann are pickable and lower to the evaluated segment`` () =
        for code in [ "ForouhiBloomer"; "BrendelBormann" ] do
            let st = applied [ SetDispersion DispersiveSegments; ChooseSegmentModel (SegmentIndex 0, modelOfKind code) ]
            Assert.Equal(code, st.segments.[0].model1.kindCode)
            // The transcendental pick now DERIVES (no rejection): one isotropic
            // dispersive segment carrying the evaluated case.
            match (derived st).eps with
            | EpsWithDispValue (IsotropicDispersive [ seg ]) ->
                match seg.dispersion with
                | EpsAxisEvaluated _ -> ()
                | other -> Assert.Fail($"expected the evaluated case for %s{code}, got %A{other}")
            | other -> Assert.Fail($"expected one isotropic dispersive segment for %s{code}, got %A{other}")

    [<Fact>]
    let ``the raw SumOfTerms escape hatch is the identity under lowering`` () =
        let sumOfTerms = modelOfKind "SumOfTerms"
        let st = applied [ SetDispersion DispersiveSegments; ChooseSegmentModel (SegmentIndex 0, sumOfTerms) ]
        match (derived st).eps, sumOfTerms with
        | EpsWithDispValue (IsotropicDispersive [ seg ]), SumOfTerms axis ->
            Assert.Equal<EpsAxisDispersion>(axis, seg.dispersion)
        | other, _ -> Assert.Fail($"expected the raw axis data verbatim, got %A{other}")

    [<Fact>]
    let ``availableGyrationClasses is constrained by the anisotropy choice`` () =
        Assert.Equal<string list>([ "Cubic" ], availableGyrationClasses Isotropic |> List.map (fun g -> g.classCode))
        Assert.Equal<string list>([ "Uniaxial" ], availableGyrationClasses Uniaxial |> List.map (fun g -> g.classCode))
        // spec 0033 gap G9: biaxial optics ⇐ orthorhombic (222 AND mm2/Planar) / monoclinic /
        // triclinic. `Planar` (mm2) was previously unreachable from every anisotropy.
        Assert.Equal<string list>(
            [ "Orthorhombic222"; "Planar"; "Monoclinic2"; "MonoclinicM"; "Triclinic1" ],
            availableGyrationClasses Biaxial |> List.map (fun g -> g.classCode))

    [<Fact>]
    let ``enabling activity snaps an un-offered class to the first offered and derives the gyration rho`` () =
        let st = applied [ ChooseAnisotropy Uniaxial; SetActivity ActivityOn ]
        Assert.Equal("Uniaxial", st.gyration.classCode)
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
        Assert.Equal<string list>([ "g11"; "g33" ], st.gyration.components |> List.map (fun (comp, _) -> comp.code))
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
        Assert.Equal("Planar", st.gyration.classCode)
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
        let repick = applyOk (ChooseSegmentModel (SegmentIndex 0, ninefold)) st
        // …so a coefficient edit must use SetSegmentModel, which applies them verbatim.
        let edited = applyOk (SetSegmentModel (SegmentIndex 0, ninefold)) st
        match (List.head repick.segments).model1, (List.head edited.segments).model1 with
        | ConstantNK a, ConstantNK b ->
            Assert.Equal(defaultRefractionIndex.value, a.n)
            Assert.Equal(9.0, b.n)
        | other -> Assert.Fail($"expected ConstantNK segments, got %A{other}")

    [<Fact>]
    let ``spec 0033 G7: modelParameters exposes and rebuilds each editable coefficient`` () =
        let model = ConstantNK { n = 1.5; k = 0.0; wavelengthUnit = Nanometer; thermoOptic = None }
        let ps = model.parameters
        Assert.Equal<ModelParameterKey list>([ ModelParameterKey "n"; ModelParameterKey "k" ], ps |> List.map (fun p -> p.key))
        match (ps |> List.find (fun p -> p.key = ModelParameterKey "n")).update (CoefficientValue 2.7) with
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
        let absorbing = applied [ SetTransparency Absorbing; SetPrincipalIndex (FirstAxis, ComplexRefractionIndex (Complex.create 1.5 0.2)) ]
        Assert.NotEqual<MaterialComplexity>(baseline, derived absorbing)
        let absorbingOff = applyOk (SetTransparency Transparent) absorbing
        Assert.Equal<MaterialComplexity>(baseline, derived absorbingOff)
        Assert.Equal<MaterialComplexity>(derived absorbing, derived (applyOk (SetTransparency Absorbing) absorbingOff))
        // Dispersive: unlock (segment list), uncheck — the constant default returns.
        let dispersive = applied [ SetDispersion DispersiveSegments; ChooseSegmentModel (SegmentIndex 0, modelOfKind "Sellmeier") ]
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
                | Ok st -> Assert.Equal<Result<MaterialComplexity, MaterialComplexityEditError>>(Ok c, st.toComplexity)
                | Error e -> Assert.Fail($"%s{entry.name.value} must seed the editor, got %A{e}")
            | None -> Assert.Fail($"%s{entry.name.value} must carry an edit model")

    [<Fact>]
    let ``the imaginaryIndexGainWarning rule flags a finite negative k only`` () =
        match (ExtinctionCoefficient -0.1).gainWarning with
        | Some message -> Assert.Contains("gain", message)
        | None -> Assert.Fail("a finite negative k implies gain and must warn")
        for k in [ 0.0; 0.1; nan; infinity; -infinity ] do
            match (ExtinctionCoefficient k).gainWarning with
            | None -> ()
            | Some message -> Assert.Fail($"k = %g{k} must not warn, got '%s{message}'")

    [<Fact>]
    let ``init seeds a new material, an existing editable entry, and a view-only entry`` () =
        // The Add path (spec 0038 step 008): the upfront-minted id rides the target as a
        // NewUnsaved entry — the SAME id the launcher's registry keys the window by.
        let minted = newMaterialId ()
        let _, _, newContext = recordingContext ()
        let m = initMaterialEditor newContext (NewMaterial minted)
        Assert.Equal({ materialId = minted; freshness = WindowLauncher.NewUnsaved }, m.target)
        Assert.Equal(EditableMaterial, m.mode)
        Assert.Equal("", m.name)
        Assert.Equal<MaterialComplexityEditState>(defaultState, m.editor)
        // An existing editable entry seeds the ladder from its complexity.
        let glass = builtIn MaterialIds.glass152
        let _, _, context = recordingContext ()
        let existing = initMaterialEditor context (EditMaterial glass)
        Assert.Equal({ materialId = glass.id; freshness = WindowLauncher.Persisted }, existing.target)
        Assert.Equal(glass.name.value, existing.name)
        Assert.Equal(EditableMaterial, existing.mode)
        match glass.complexity with
        | Some c -> Assert.Equal<Result<MaterialComplexity, MaterialComplexityEditError>>(Ok c, existing.editor.toComplexity)
        | None -> Assert.Fail("glass must carry an edit model")
        // A complexity-less entry (engine-coded physics) opens view-only.
        let silicon = initMaterialEditor context (EditMaterial (builtIn MaterialIds.silicon))
        match silicon.mode with
        | ViewOnlyMaterial _ -> ()
        | EditableMaterial -> Assert.Fail("silicon (complexity = None) must open view-only")

    [<Fact>]
    let ``Save adds a NEW material with complexity Some, updates an EXISTING one, and Cancel writes nothing`` () =
        // NewUnsaved → saveMaterial under the UPFRONT-minted id (spec 0038 step 008 — the mint
        // happened at window open, not here; step 021 collapsed the add/update split into the ONE
        // versioned saveMaterial, which inserts version 1 for an id the store does not yet hold),
        // then close.
        let calls, saved, context = recordingContext ()
        let minted = newMaterialId ()
        initMaterialEditor context (NewMaterial minted)
        |> updateMaterialEditor (SetMaterialName "Fresh")
        |> updateMaterialEditor MaterialSaveClicked
        |> ignore
        Assert.Equal<string list>([ "save:Fresh"; "close" ], List.ofSeq calls)
        Assert.Equal(minted, saved.[0].id)
        match saved.[0].complexity with
        | Some c ->
            Assert.Equal<MaterialComplexity>(defaultComplexity, c)
            // properties IS complexity.toProperties — the sync invariant at save time.
            let eps = saved.[0].properties.epsWithDisp.getEps (WaveLength.nm 600.0<nm>)
            Assert.True(close (eps.[0, 0].Real) (1.5 * 1.5), $"eps11 = %A{eps.[0, 0]}")
        | None -> Assert.Fail("Save must store complexity = Some model")
        // Persisted → saveMaterial under the SAME id (metadata-only edit → mutate in place),
        // then close.
        let calls2, saved2, context2 = recordingContext ()
        let glass = builtIn MaterialIds.glass152
        initMaterialEditor context2 (EditMaterial glass)
        |> updateMaterialEditor (SetMaterialName "Edited")
        |> updateMaterialEditor MaterialSaveClicked
        |> ignore
        Assert.Equal<string list>([ "save:Edited"; "close" ], List.ofSeq calls2)
        Assert.Equal(glass.id, saved2.[0].id)
        // Cancel on a DIRTY editor no longer closes silently (spec 0038 step 033): it shows the
        // discard confirm and reaches neither the write-seam nor requestClose; Discard then closes
        // WITHOUT saving.
        let calls3, saved3, context3 = recordingContext ()
        let confirming =
            initMaterialEditor context3 (EditMaterial glass)
            |> updateMaterialEditor (SetMaterialName "Discarded")
            |> updateMaterialEditor MaterialCancelClicked
        Assert.Empty(calls3)
        match confirming.exit with
        | MaterialConfirmingDiscard -> ()
        | MaterialEditing -> Assert.Fail("a dirty Cancel must show the discard confirm, not close")
        updateMaterialEditor MaterialDiscardConfirmed confirming |> ignore
        Assert.Equal<string list>([ "close" ], List.ofSeq calls3)
        Assert.Empty(saved3)

    [<Fact>]
    let ``a failing save keeps the window open and surfaces the proxy's reason`` () =
        let closes = ResizeArray<string>()
        let failing : MaterialProxy =
            {
                listMaterials = fun _ -> Ok []
                searchMaterials = fun _ -> Ok []
                tryGetMaterial = fun _ -> Ok None
                resolveVersion = fun _ -> Ok None
                saveMaterial = fun _ -> Error (InvalidMaterialErr "the name is blank")
                markMaterialInactive = fun _ -> Ok ()
                markMaterialActive = fun _ -> Ok ()
                supersedeMaterial = fun _ -> Ok ()
                removeMaterial = fun _ -> Ok ()
            }
        let context : MaterialEditorContext = { materials = failing; categories = CategoryProxy.createInMemory (fun _ -> []); requestClose = fun () -> closes.Add "close" }
        let m = initMaterialEditor context (NewMaterial (newMaterialId ())) |> updateMaterialEditor MaterialSaveClicked
        Assert.Empty(closes)
        match m.status with
        | Some reason -> Assert.Equal("the name is blank", reason)
        | None -> Assert.Fail("expected the proxy's typed reason as the status")

    [<Fact>]
    let ``spec 0038 (032): the per-series show/hide toggle round-trips through update and lowers onto the exact curve`` () =
        // Handler — ToggleSeriesVisibility flips the key into `hiddenSeries`; a second dispatch removes it.
        let key = (ConstructorUiId.previewSeriesToggle (ConstructorUiKey "nk") (ConstructorUiKey "n₁")).value
        let m0 = newModel ()
        Assert.False(Set.contains key m0.hiddenSeries, "nothing is hidden on a fresh model")
        let hiddenModel = updateMaterialEditor (ToggleSeriesVisibility key) m0
        Assert.True(Set.contains key hiddenModel.hiddenSeries, "toggling on records the key in hiddenSeries")
        let shownModel = updateMaterialEditor (ToggleSeriesVisibility key) hiddenModel
        Assert.False(Set.contains key shownModel.hiddenSeries, "toggling the same key again removes it")
        // Lowering — applyHidden flips EXACTLY the toggled curve invisible on the chart's style seed. The
        // n/k chart's series are n₁ n₂ n₃ (indices 0..2) then k₁ k₂ k₃ (3..5), so n₁ is index 0.
        let chart = NkDispersionChart.nkDispersionChart Biaxial (propsFrom []) Nanometer previewRange
        let style = NkDispersionChart.nkDispersionStyle Biaxial chart
        let loweredN1 = applyHidden (Set.singleton key) "nk" chart style
        Assert.False(seriesVisibleAt 0 loweredN1, "hiding n₁ must flip exactly index 0 invisible")
        for i in 1 .. 5 do
            Assert.True(seriesVisibleAt i loweredN1, $"index %d{i} must stay visible when only n₁ is hidden")
        // A DIFFERENT series name lowers onto a DIFFERENT index — the index is read from the series NAME,
        // never hard-coded, so a wrong index cannot pass silently. k₂ is the 5th series (index 4).
        let loweredK2 = applyHidden (Set.singleton (ConstructorUiId.previewSeriesToggle (ConstructorUiKey "nk") (ConstructorUiKey "k₂")).value) "nk" chart style
        Assert.False(seriesVisibleAt 4 loweredK2, "hiding k₂ must flip exactly index 4 invisible")
        for i in [ 0; 1; 2; 3; 5 ] do
            Assert.True(seriesVisibleAt i loweredK2, $"index %d{i} must stay visible when only k₂ is hidden")
        // The tabCode namespaces the key: the SAME n₁ key applied under the WRONG tab hides nothing, so a
        // wrong tabCode cannot pass silently either.
        let wrongTab = applyHidden (Set.singleton key) "gyration" chart style
        for i in 0 .. 5 do
            Assert.True(seriesVisibleAt i wrongTab, $"a mismatched tabCode must leave index %d{i} visible")

    // ============================ headless semantic-tree proofs ============================

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the window mounts with every slice-mandated UiId present`` () =
        HeadlessSession.run (fun () ->
            let materials, _ = freshProxies ()
            let window = MaterialEditorWindow(materials, NewMaterial (newMaterialId ()))
            window.Show()
            Dispatcher.UIThread.RunJobs()
            Assert.True(matchesId ConstructorUiId.materialEditorWindow.value window, "the window itself carries the MaterialEditorWindow id")
            for id in
                [
                    ConstructorUiId.materialNameBox.value
                    ConstructorUiId.anisotropyToggle.value
                    ConstructorUiId.absorbingToggle.value
                    ConstructorUiId.constantToggle.value
                    ConstructorUiId.dispersiveToggle.value
                    ConstructorUiId.activeToggle.value
                    ConstructorUiId.magneticToggle.value
                    ConstructorUiId.materialEditorSaveButton.value
                    ConstructorUiId.materialEditorCancelButton.value
                ] do
                Assert.True(isPresent window id, $"%s{id} is missing from the mounted window")
            // The segment editor's mandated ids appear once the dispersive rung unlocks…
            clickOn window ConstructorUiId.dispersiveToggle.value
            Assert.True(isPresent window ConstructorUiId.dispersionModelPicker.value, "the dispersive rung must expose the model picker")
            Assert.True(isPresent window ConstructorUiId.addSegmentButton.value, "the dispersive rung must expose the add-segment verb")
            // …and the gyration panel's once the activity rung unlocks.
            clickOn window ConstructorUiId.activeToggle.value
            Assert.True(isPresent window ConstructorUiId.gyrationClassPicker.value, "the activity rung must expose the class picker")
            Assert.True(isPresent window ConstructorUiId.handednessSwitch.value, "the activity rung must expose the handedness switch")
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``spec 0040 (007): the description box carries wrapping so it grows multiline rather than clipping`` () =
        // Part C.2: the material-editor description box must WRAP to multiple lines instead of
        // clipping against the splitter when the left pane narrows — headless-verified that the
        // wrapping property is enabled (and AcceptsReturn, so a long description grows downward).
        HeadlessSession.run (fun () ->
            let materials, _ = freshProxies ()
            let window = MaterialEditorWindow(materials, NewMaterial (newMaterialId ()))
            window.Show()
            Dispatcher.UIThread.RunJobs()
            match tryFindControl window ConstructorUiId.materialDescriptionBox.value with
            | Some (:? TextBox as tb) ->
                Assert.Equal(Avalonia.Media.TextWrapping.Wrap, tb.TextWrapping)
                Assert.True(tb.AcceptsReturn, "the description box must accept returns so it grows multiline")
            | Some c -> Assert.Fail($"the description box is a %s{c.GetType().Name}, not a TextBox")
            | None -> Assert.Fail("the description box was not found in the mounted editor")
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``acceptance: choosing biaxial exposes three principal-index fields`` () =
        HeadlessSession.run (fun () ->
            let materials, _ = freshProxies ()
            let window = MaterialEditorWindow(materials, NewMaterial (newMaterialId ()))
            window.Show()
            Dispatcher.UIThread.RunJobs()
            // Isotropic (the default): one index field.
            Assert.True(isPresent window (ConstructorUiId.principalIndexBox (ConstructorUiPosition 1)).value)
            Assert.False(isPresent window (ConstructorUiId.principalIndexBox (ConstructorUiPosition 2)).value)
            Assert.False(isPresent window (ConstructorUiId.principalIndexBox (ConstructorUiPosition 3)).value)
            // Uniaxial: ordinary + extraordinary.
            clickOn window (ConstructorUiId.anisotropyOption (ConstructorUiKey (anisotropyCode Uniaxial))).value
            Assert.True(isPresent window (ConstructorUiId.principalIndexBox (ConstructorUiPosition 2)).value)
            Assert.False(isPresent window (ConstructorUiId.principalIndexBox (ConstructorUiPosition 3)).value)
            // Biaxial: three principal-index fields.
            clickOn window (ConstructorUiId.anisotropyOption (ConstructorUiKey (anisotropyCode Biaxial))).value
            Assert.True(isPresent window (ConstructorUiId.principalIndexBox (ConstructorUiPosition 1)).value)
            Assert.True(isPresent window (ConstructorUiId.principalIndexBox (ConstructorUiPosition 2)).value)
            Assert.True(isPresent window (ConstructorUiId.principalIndexBox (ConstructorUiPosition 3)).value)
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``acceptance: enabling activity on a uniaxial medium offers only the uniaxial gyration classes`` () =
        HeadlessSession.run (fun () ->
            let materials, _ = freshProxies ()
            let window = MaterialEditorWindow(materials, NewMaterial (newMaterialId ()))
            window.Show()
            Dispatcher.UIThread.RunJobs()
            clickOn window (ConstructorUiId.anisotropyOption (ConstructorUiKey (anisotropyCode Uniaxial))).value
            clickOn window ConstructorUiId.activeToggle.value
            Assert.True(isPresent window ConstructorUiId.gyrationClassPicker.value)
            Assert.True(isPresent window ConstructorUiId.handednessSwitch.value)
            Assert.True(isPresent window (ConstructorUiId.gyrationClassOption (ConstructorUiKey "Uniaxial")).value, "the uniaxial diagonal class must be offered")
            for offLimits in [ "Cubic"; "Planar"; "Orthorhombic222"; "Monoclinic2"; "MonoclinicM"; "Triclinic1" ] do
                Assert.False(isPresent window (ConstructorUiId.gyrationClassOption (ConstructorUiKey offLimits)).value, $"%s{offLimits} must NOT be offered for a uniaxial medium")
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``acceptance: unchecking a toggle restores the default model losslessly`` () =
        HeadlessSession.run (fun () ->
            let materials, _ = freshProxies ()
            let window = MaterialEditorWindow(materials, NewMaterial (newMaterialId ()))
            window.Show()
            Dispatcher.UIThread.RunJobs()
            let initial = textOf window ConstructorUiId.materialComplexitySummary.value
            // Absorbing on, k edited — the derived model changes; unchecking restores it.
            clickOn window ConstructorUiId.absorbingToggle.value
            setText window (ConstructorUiId.absorptionIndexBox (ConstructorUiPosition 1)).value "0.25"
            Assert.NotEqual<string>(initial, textOf window ConstructorUiId.materialComplexitySummary.value)
            clickOn window ConstructorUiId.absorbingToggle.value
            Assert.Equal(initial, textOf window ConstructorUiId.materialComplexitySummary.value)
            // The eps branch is two mutually-exclusive options (spec 0035 step 017): choosing
            // Dispersive changes the derived model; choosing Constant restores it losslessly.
            clickOn window ConstructorUiId.dispersiveToggle.value
            Assert.NotEqual<string>(initial, textOf window ConstructorUiId.materialComplexitySummary.value)
            clickOn window ConstructorUiId.constantToggle.value
            Assert.Equal(initial, textOf window ConstructorUiId.materialComplexitySummary.value)
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``acceptance: the eps branch offers Constant and Dispersive as two mutually-exclusive options`` () =
        HeadlessSession.run (fun () ->
            let materials, _ = freshProxies ()
            let window = MaterialEditorWindow(materials, NewMaterial (newMaterialId ()))
            window.Show()
            Dispatcher.UIThread.RunJobs()
            // Both options are present from the start — not a sticky single toggle (spec 0035 step 017).
            Assert.True(isPresent window ConstructorUiId.constantToggle.value, "the Constant option must be offered")
            Assert.True(isPresent window ConstructorUiId.dispersiveToggle.value, "the Dispersive option must be offered")
            // The default is Constant: the constant index field shows, the segment editor does not.
            Assert.True(isPresent window (ConstructorUiId.principalIndexBox (ConstructorUiPosition 1)).value)
            Assert.False(isPresent window (ConstructorUiId.segmentLowerBox (ConstructorUiPosition 0)).value)
            Assert.DoesNotContain("dispersive", textOf window ConstructorUiId.materialComplexitySummary.value)
            // Choosing Dispersive selects DispersiveSegments (the segment editor appears)…
            clickOn window ConstructorUiId.dispersiveToggle.value
            Assert.Contains("dispersive", textOf window ConstructorUiId.materialComplexitySummary.value)
            Assert.True(isPresent window (ConstructorUiId.segmentLowerBox (ConstructorUiPosition 0)).value, "Dispersive selects the DispersiveSegments branch")
            // …and choosing Constant selects NonDispersive again (mutually exclusive, not toggled off).
            clickOn window ConstructorUiId.constantToggle.value
            Assert.DoesNotContain("dispersive", textOf window ConstructorUiId.materialComplexitySummary.value)
            Assert.True(isPresent window (ConstructorUiId.principalIndexBox (ConstructorUiPosition 1)).value, "Constant selects the NonDispersive branch")
            Assert.False(isPresent window (ConstructorUiId.segmentLowerBox (ConstructorUiPosition 0)).value)
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``acceptance: Save round-trips a NEW entry through MaterialProxy with complexity Some`` () =
        HeadlessSession.run (fun () ->
            let materials, _ = freshProxies ()
            let seededCount =
                match materials.listMaterials ActiveOnly with
                | Ok all -> List.length all
                | Error e -> failwith $"seed listing failed: %A{e}"
            let window = MaterialEditorWindow(materials, NewMaterial (newMaterialId ()))
            window.Show()
            Dispatcher.UIThread.RunJobs()
            setText window ConstructorUiId.materialNameBox.value "Headless material"
            clickOn window (ConstructorUiId.anisotropyOption (ConstructorUiKey (anisotropyCode Biaxial))).value
            setText window (ConstructorUiId.principalIndexBox (ConstructorUiPosition 1)).value "1.6"
            setText window (ConstructorUiId.principalIndexBox (ConstructorUiPosition 2)).value "1.7"
            setText window (ConstructorUiId.principalIndexBox (ConstructorUiPosition 3)).value "1.8"
            clickOn window ConstructorUiId.materialEditorSaveButton.value
            Assert.False(window.IsVisible)
            match materials.listMaterials ActiveOnly with
            | Ok all ->
                Assert.Equal(seededCount + 1, List.length all)
                match all |> List.tryFind (fun e -> e.name.value = "Headless material") with
                | Some savedEntry ->
                    match savedEntry.complexity with
                    | Some c ->
                        match c.eps with
                        | EpsWithoutDispValue (BiaxialTransparent { nx = RefractionIndex n1; ny = RefractionIndex n2; nz = RefractionIndex n3 }) ->
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
    let ``Save UPDATES an existing editable entry in place through MaterialProxy.saveMaterial (metadata-only mutate in place)`` () =
        HeadlessSession.run (fun () ->
            let materials, _ = freshProxies ()
            let existing =
                match materials.tryGetMaterial MaterialIds.glass152 with
                | Ok (Some e) -> e
                | other -> failwith $"glass152 must be seeded, got %A{other}"
            let seededCount =
                match materials.listMaterials ActiveOnly with
                | Ok all -> List.length all
                | Error e -> failwith $"seed listing failed: %A{e}"
            let window = MaterialEditorWindow(materials, EditMaterial existing)
            window.Show()
            Dispatcher.UIThread.RunJobs()
            setText window ConstructorUiId.materialNameBox.value "Renamed glass"
            clickOn window ConstructorUiId.materialEditorSaveButton.value
            Assert.False(window.IsVisible)
            match materials.tryGetMaterial MaterialIds.glass152 with
            | Ok (Some updated) ->
                Assert.Equal("Renamed glass", updated.name.value)
                match updated.complexity with
                | Some _ -> ()
                | None -> Assert.Fail("the updated entry must keep complexity = Some model")
            | Ok None -> Assert.Fail("the existing entry vanished")
            | Error e -> Assert.Fail($"tryGetMaterial failed: %A{e}")
            match materials.listMaterials ActiveOnly with
            | Ok all -> Assert.Equal(seededCount, List.length all)
            | Error e -> Assert.Fail($"listMaterials failed: %A{e}"))

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``a negative k surfaces the advisory gain warning and a non-negative k clears it`` () =
        HeadlessSession.run (fun () ->
            let materials, _ = freshProxies ()
            let window = MaterialEditorWindow(materials, NewMaterial (newMaterialId ()))
            window.Show()
            Dispatcher.UIThread.RunJobs()
            Assert.Equal("", textOf window ConstructorUiId.materialGainWarning.value)
            clickOn window ConstructorUiId.absorbingToggle.value
            setText window (ConstructorUiId.absorptionIndexBox (ConstructorUiPosition 1)).value "-0.1"
            Assert.Contains("gain", textOf window ConstructorUiId.materialGainWarning.value)
            setText window (ConstructorUiId.absorptionIndexBox (ConstructorUiPosition 1)).value "0.1"
            Assert.Equal("", textOf window ConstructorUiId.materialGainWarning.value)
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``a complexity-less entry opens view-only with no Save affordance and no ladder`` () =
        HeadlessSession.run (fun () ->
            let materials, _ = freshProxies ()
            let window = MaterialEditorWindow(materials, EditMaterial (builtIn MaterialIds.silicon))
            window.Show()
            Dispatcher.UIThread.RunJobs()
            Assert.True(isPresent window ConstructorUiId.materialViewOnlyNote.value, "a view-only entry must say why it cannot be edited")
            Assert.False(isPresent window ConstructorUiId.materialEditorSaveButton.value, "a view-only entry offers NO Save affordance")
            Assert.False(isPresent window ConstructorUiId.absorbingToggle.value, "the ladder is removed, not greyed")
            Assert.False(isPresent window ConstructorUiId.activeToggle.value, "the ladder is removed, not greyed")
            clickOn window ConstructorUiId.materialEditorCancelButton.value
            Assert.False(window.IsVisible))

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the segment editor adds segments and a transcendental pick derives a dispersive eps`` () =
        HeadlessSession.run (fun () ->
            let materials, _ = freshProxies ()
            let window = MaterialEditorWindow(materials, NewMaterial (newMaterialId ()))
            window.Show()
            Dispatcher.UIThread.RunJobs()
            clickOn window ConstructorUiId.dispersiveToggle.value
            Assert.True(isPresent window (ConstructorUiId.segmentLowerBox (ConstructorUiPosition 0)).value)
            Assert.False(isPresent window (ConstructorUiId.segmentLowerBox (ConstructorUiPosition 1)).value)
            clickOn window ConstructorUiId.addSegmentButton.value
            Assert.True(isPresent window (ConstructorUiId.segmentLowerBox (ConstructorUiPosition 1)).value, "AddSegment must append a second segment row")
            // A transcendental pick now LOWERS to the evaluated segment, so the entry
            // DERIVES a dispersive eps instead of surfacing a rejection.
            clickOn window (ConstructorUiId.dispersionModelOption (ConstructorUiPosition 0) (ConstructorUiKey "ForouhiBloomer")).value
            Assert.Contains("dispersive", textOf window ConstructorUiId.materialComplexitySummary.value)
            Assert.DoesNotContain("not derivable", textOf window ConstructorUiId.materialComplexitySummary.value)
            clickOn window (ConstructorUiId.dispersionModelOption (ConstructorUiPosition 0) (ConstructorUiKey "SumOfTerms")).value
            Assert.Contains("dispersive", textOf window ConstructorUiId.materialComplexitySummary.value)
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``acceptance: the activity Dispersive sub-toggle swaps each gyration component for a formula editor and back`` () =
        HeadlessSession.run (fun () ->
            let materials, _ = freshProxies ()
            let window = MaterialEditorWindow(materials, NewMaterial (newMaterialId ()))
            window.Show()
            Dispatcher.UIThread.RunJobs()
            // A uniaxial active medium's class carries exactly g11 and g33.
            clickOn window (ConstructorUiId.anisotropyOption (ConstructorUiKey (anisotropyCode Uniaxial))).value
            clickOn window ConstructorUiId.activeToggle.value
            Assert.True(isPresent window ConstructorUiId.activityDispersiveToggle.value, "the activity rung must expose the Dispersive sub-toggle")
            // Constant (the default sub-branch): the per-component constant boxes, no formula editor.
            for code in [ "g11"; "g33" ] do
                Assert.True(isPresent window (ConstructorUiId.gyrationComponentBox (ConstructorUiKey code)).value, $"the constant %s{code} box must be present")
                Assert.False(isPresent window (ConstructorUiId.gyrationFormulaEditor (ConstructorUiKey code)).value, $"%s{code} must have no formula editor while constant")
            // Enabling Dispersive exposes a dispersion-formula editor per symmetry-allowed
            // component; the constant boxes are gone.
            clickOn window ConstructorUiId.activityDispersiveToggle.value
            for code in [ "g11"; "g33" ] do
                Assert.True(isPresent window (ConstructorUiId.gyrationFormulaEditor (ConstructorUiKey code)).value, $"%s{code} must expose a dispersion-formula editor under Dispersive")
                Assert.False(isPresent window (ConstructorUiId.gyrationComponentBox (ConstructorUiKey code)).value, $"the constant %s{code} box must be removed under Dispersive")
            // Unchecking restores the constant component boxes.
            clickOn window ConstructorUiId.activityDispersiveToggle.value
            for code in [ "g11"; "g33" ] do
                Assert.True(isPresent window (ConstructorUiId.gyrationComponentBox (ConstructorUiKey code)).value, $"unchecking must restore the constant %s{code} box")
                Assert.False(isPresent window (ConstructorUiId.gyrationFormulaEditor (ConstructorUiKey code)).value, $"%s{code} formula editor must be gone again")
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``acceptance: the magnetic Dispersive sub-toggle swaps the Polder components for formula editors and back`` () =
        HeadlessSession.run (fun () ->
            let materials, _ = freshProxies ()
            let window = MaterialEditorWindow(materials, NewMaterial (newMaterialId ()))
            window.Show()
            Dispatcher.UIThread.RunJobs()
            clickOn window ConstructorUiId.magneticToggle.value
            Assert.True(isPresent window ConstructorUiId.magneticDispersiveToggle.value, "the magnetic rung must expose the Dispersive sub-toggle")
            // Constant (default, scalar kind): the diagonal μ box, no formula editor.
            Assert.True(isPresent window ConstructorUiId.muDiagonalBox.value, "the constant diagonal μ box must be present")
            Assert.False(isPresent window (ConstructorUiId.polderFormulaEditor (ConstructorUiKey "muDiagonal")).value, "no Polder formula editor while constant")
            // Enabling Dispersive exposes the full-tensor Polder component formula editors;
            // the constant box is gone.
            clickOn window ConstructorUiId.magneticDispersiveToggle.value
            for code in [ "muDiagonal"; "muParallel"; "muGyration" ] do
                Assert.True(isPresent window (ConstructorUiId.polderFormulaEditor (ConstructorUiKey code)).value, $"%s{code} must expose a dispersion-formula editor under Dispersive")
            Assert.False(isPresent window ConstructorUiId.muDiagonalBox.value, "the constant diagonal μ box must be removed under Dispersive")
            // Unchecking restores the constant component box.
            clickOn window ConstructorUiId.magneticDispersiveToggle.value
            Assert.True(isPresent window ConstructorUiId.muDiagonalBox.value, "unchecking must restore the constant diagonal μ box")
            for code in [ "muDiagonal"; "muParallel"; "muGyration" ] do
                Assert.False(isPresent window (ConstructorUiId.polderFormulaEditor (ConstructorUiKey code)).value, $"%s{code} formula editor must be gone again")
            window.Close())

    // ============================ spec 0038 (032): the two-pane split + tabbed preview ============================

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the editor is a two-pane split whose full-height n/k chart renders one frame`` () =
        HeadlessSession.run (fun () ->
            let materials, _ = freshProxies ()
            let window = MaterialEditorWindow(materials, NewMaterial (newMaterialId ()))
            window.Show()
            Dispatcher.UIThread.RunJobs()
            // The two panes are split by a vertical GridSplitter; the right pane is a tabbed preview.
            Assert.True(isPresent window ConstructorUiId.materialEditorSplitter.value, "the two-pane split must carry a vertical GridSplitter")
            Assert.True(isPresent window ConstructorUiId.materialPreviewTabs.value, "the preview pane must be a TabControl")
            Assert.True(isPresent window ConstructorUiId.materialPreviewNkTab.value, "the n/k tab is always present")
            // The n/k tab (the default selection) embeds the full-height chart; it rasterizes one frame.
            Assert.True(isPresent window ConstructorUiId.materialPreviewChart.value, "the n/k tab embeds the shared chart host")
            match avaUnder window ConstructorUiId.materialPreviewChart.value with
            | Some ava -> Assert.True(rasterizes ava, "the full-height n/k chart rendered no image bytes")
            | None -> Assert.Fail("no embedded AvaPlot under the n/k preview chart host")
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the Gyration tab appears exactly when optically active`` () =
        HeadlessSession.run (fun () ->
            let materials, _ = freshProxies ()
            let window = MaterialEditorWindow(materials, NewMaterial (newMaterialId ()))
            window.Show()
            Dispatcher.UIThread.RunJobs()
            Assert.False(isPresent window ConstructorUiId.materialPreviewGyrationTab.value, "no Gyration tab before activity is enabled")
            clickOn window ConstructorUiId.activeToggle.value
            Assert.True(isPresent window ConstructorUiId.materialPreviewGyrationTab.value, "the Gyration tab appears when optically active")
            clickOn window ConstructorUiId.activeToggle.value
            Assert.False(isPresent window ConstructorUiId.materialPreviewGyrationTab.value, "the Gyration tab disappears when activity is turned off")
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the mu tab appears exactly when magnetic`` () =
        HeadlessSession.run (fun () ->
            let materials, _ = freshProxies ()
            let window = MaterialEditorWindow(materials, NewMaterial (newMaterialId ()))
            window.Show()
            Dispatcher.UIThread.RunJobs()
            Assert.False(isPresent window ConstructorUiId.materialPreviewMuTab.value, "no μ tab before magnetic is enabled")
            clickOn window ConstructorUiId.magneticToggle.value
            Assert.True(isPresent window ConstructorUiId.materialPreviewMuTab.value, "the μ tab appears when magnetic")
            clickOn window ConstructorUiId.magneticToggle.value
            Assert.False(isPresent window ConstructorUiId.materialPreviewMuTab.value, "the μ tab disappears when magnetic is turned off")
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``one frame renders per preview tab (isotropic, biaxial, active, magnetic)`` () =
        HeadlessSession.run (fun () ->
            let render (autoId : ConstructorUiId) chart style : unit =
                let window = mount (OpticalConstructor.Controls.EmbeddedChart.embeddedChartView autoId chart style)
                match avaUnder window autoId.value with
                | Some ava -> Assert.True(rasterizes ava, $"%s{autoId.value} rendered no image bytes")
                | None -> Assert.Fail($"no embedded AvaPlot for %s{autoId.value}")
                window.Close()
            let range = previewRange
            // The n/k tab for an isotropic (default) and a biaxial entry.
            let isotropic = propsFrom []
            let biaxial =
                propsFrom
                    [
                        ChooseAnisotropy Biaxial
                        SetPrincipalIndex (FirstAxis, ComplexRefractionIndex (Complex.create 1.6 0.0))
                        SetPrincipalIndex (SecondAxis, ComplexRefractionIndex (Complex.create 1.7 0.0))
                        SetPrincipalIndex (ThirdAxis, ComplexRefractionIndex (Complex.create 1.8 0.0))
                    ]
            for props in [ isotropic; biaxial ] do
                let c = NkDispersionChart.nkDispersionChart Biaxial props Nanometer range
                render (ConstructorUiId "TabRenderNk") c (NkDispersionChart.nkDispersionStyle Biaxial c)
            // The Gyration tab for an active entry — the uniaxial class restricts it to g₁₁ / g₃₃.
            let active = propsFrom [ ChooseAnisotropy Uniaxial; SetActivity ActivityOn ]
            let gComponents = (UniaxialActive { g11 = defaultGyrationComponent; g33 = defaultGyrationComponent }).components |> List.map fst
            let g = NkDispersionChart.gyrationChart gComponents active Nanometer range
            render (ConstructorUiId "TabRenderGyration") g (NkDispersionChart.gyrationStyle g)
            // The μ tab for a gyromagnetic entry.
            let magnetic = propsFrom [ SetMagnetic MagneticOn; SetMuKind GyromagneticMuKind; ChooseGyrationAxis AlongZ ]
            let mu = NkDispersionChart.muChart GyromagneticMuKind magnetic Nanometer range
            render (ConstructorUiId "TabRenderMu") mu (NkDispersionChart.muStyle mu))

    // ============================ spec 0040 (006): the preview restricts the gyration / μ curves ============================

    [<Fact>]
    let ``spec 0040 006: an editable uniaxial-active entry threads only g₁₁ / g₃₃ to the gyration preview`` () =
        // ChooseAnisotropy Uniaxial then SetActivity ActivityOn snaps the class to UniaxialActive.
        let m =
            newModel ()
            |> updateMaterialEditor (EditorMsg (ChooseAnisotropy Uniaxial))
            |> updateMaterialEditor (EditorMsg (SetActivity ActivityOn))
        Assert.Equal<GyrationComponent list>([ G11; G33 ], previewGyrationComponents m)

    [<Fact>]
    let ``spec 0040 006: an editable scalar-magnetic entry threads ScalarMuKind, a gyromagnetic one GyromagneticMuKind`` () =
        // The default μ kind is scalar; turning magnetic on alone keeps it scalar.
        let scalar = newModel () |> updateMaterialEditor (EditorMsg (SetMagnetic MagneticOn))
        Assert.Equal(ScalarMuKind, previewMuKind scalar)
        let gyromagnetic =
            newModel ()
            |> updateMaterialEditor (EditorMsg (SetMagnetic MagneticOn))
            |> updateMaterialEditor (EditorMsg (SetMuKind GyromagneticMuKind))
        Assert.Equal(GyromagneticMuKind, previewMuKind gyromagnetic)

    [<Fact>]
    let ``spec 0040 006: a view-only entry reads the class off its stored complexity tree — Langasite carries g₁₁ / g₃₃`` () =
        // Langasite opens VIEW-ONLY (its ε is an opaque evaluated closure) yet its stored complexity
        // tree carries a UniaxialActive gyration class — so the preview restricts to g₁₁ / g₃₃.
        let _, _, context = recordingContext ()
        let m = initMaterialEditor context (EditMaterial (builtIn MaterialIds.langasite))
        match m.mode with
        | ViewOnlyMaterial _ -> ()
        | EditableMaterial -> Assert.Fail("Langasite must open view-only")
        Assert.Equal<GyrationComponent list>([ G11; G33 ], previewGyrationComponents m)

    [<Fact>]
    let ``spec 0040 006: a coded preset with no stored tree still renders all components`` () =
        // Simulate the pre-step-004 coded preset: a view-only entry whose complexity is absent. The
        // preview keeps the full six-component gyration and the full gyromagnetic μ rendering.
        let _, _, context = recordingContext ()
        let codedPreset = { builtIn MaterialIds.silicon with complexity = None }
        let m = initMaterialEditor context (EditMaterial codedPreset)
        match m.mode with
        | ViewOnlyMaterial _ -> ()
        | EditableMaterial -> Assert.Fail("a complexity-less entry must open view-only")
        Assert.Equal<GyrationComponent list>(NkDispersionChart.allGyrationComponents, previewGyrationComponents m)
        Assert.Equal(GyromagneticMuKind, previewMuKind m)

    // ============================ spec 0038 (033): the unsaved-edit exit confirm ============================

    [<Fact>]
    let ``spec 0038 (033): a pristine editor closes silently, a dirty one gates Cancel behind the discard confirm`` () =
        // Pristine: a freshly opened editor is not dirty and Cancel closes silently as before.
        let calls, _, context = recordingContext ()
        let fresh = initMaterialEditor context (NewMaterial (newMaterialId ()))
        Assert.False(isMaterialEditorDirty fresh, "a freshly opened editor is pristine")
        let afterCancel = updateMaterialEditor MaterialCancelClicked fresh
        Assert.Equal<string list>([ "close" ], List.ofSeq calls)
        Assert.Equal(MaterialEditing, afterCancel.exit)
        // Dirty: a name edit makes it dirty; Cancel shows the confirm and reaches nothing.
        let calls2, saved2, context2 = recordingContext ()
        let edited = initMaterialEditor context2 (NewMaterial (newMaterialId ())) |> updateMaterialEditor (SetMaterialName "X")
        Assert.True(isMaterialEditorDirty edited, "a name edit makes the editor dirty")
        let confirming = updateMaterialEditor MaterialCancelClicked edited
        Assert.Equal(MaterialConfirmingDiscard, confirming.exit)
        Assert.Empty(calls2)
        // Keep editing dismisses the confirm without closing.
        let kept = updateMaterialEditor MaterialKeepEditing confirming
        Assert.Equal(MaterialEditing, kept.exit)
        Assert.Empty(calls2)
        // Discard closes without ever reaching the write-seam.
        updateMaterialEditor MaterialDiscardConfirmed confirming |> ignore
        Assert.Equal<string list>([ "close" ], List.ofSeq calls2)
        Assert.Empty(saved2)

    [<Fact>]
    let ``spec 0038 (033): reverting an edit to its captured value returns the editor to pristine`` () =
        // Snapshot equality, no dirty flag: editing then restoring the exact loaded value clears
        // dirtiness, so Cancel then closes silently.
        let calls, _, context = recordingContext ()
        let glass = builtIn MaterialIds.glass152
        let m = initMaterialEditor context (EditMaterial glass)
        Assert.False(isMaterialEditorDirty m, "an unedited existing entry is pristine")
        let renamed = updateMaterialEditor (SetMaterialName "Other") m
        Assert.True(isMaterialEditorDirty renamed)
        let reverted = updateMaterialEditor (SetMaterialName glass.name.value) renamed
        Assert.False(isMaterialEditorDirty reverted, "restoring the captured name clears dirtiness")
        updateMaterialEditor MaterialCancelClicked reverted |> ignore
        Assert.Equal<string list>([ "close" ], List.ofSeq calls)

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``spec 0038 (033): a pristine editor's Cancel closes immediately`` () =
        HeadlessSession.run (fun () ->
            let materials, _ = freshProxies ()
            let window = MaterialEditorWindow(materials, NewMaterial (newMaterialId ()))
            window.Show()
            Dispatcher.UIThread.RunJobs()
            Assert.False(isPresent window ConstructorUiId.materialEditorExitConfirm.value, "no confirm surface before any edit")
            clickOn window ConstructorUiId.materialEditorCancelButton.value
            Assert.False(window.IsVisible, "a pristine Cancel closes immediately"))

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``spec 0038 (033): a dirty Cancel shows the confirm; Keep editing returns; Discard closes without saving`` () =
        HeadlessSession.run (fun () ->
            let materials, _ = freshProxies ()
            let seededCount =
                match materials.listMaterials ActiveOnly with
                | Ok all -> List.length all
                | Error e -> failwith $"seed listing failed: %A{e}"
            let window = MaterialEditorWindow(materials, NewMaterial (newMaterialId ()))
            window.Show()
            Dispatcher.UIThread.RunJobs()
            // A name edit makes it dirty; Cancel shows the confirm instead of closing.
            setText window ConstructorUiId.materialNameBox.value "Unsaved material"
            clickOn window ConstructorUiId.materialEditorCancelButton.value
            Assert.True(window.IsVisible, "a dirty Cancel must not close the window")
            Assert.True(isPresent window ConstructorUiId.materialEditorExitConfirm.value, "the discard confirm must appear")
            Assert.True(isPresent window ConstructorUiId.materialEditorDiscardButton.value, "Discard changes must be offered")
            Assert.True(isPresent window ConstructorUiId.materialEditorKeepEditingButton.value, "Keep editing must be offered")
            Assert.False(isPresent window ConstructorUiId.materialEditorSaveButton.value, "Save/Cancel are replaced by the confirm")
            // Keep editing returns to the editor.
            clickOn window ConstructorUiId.materialEditorKeepEditingButton.value
            Assert.True(window.IsVisible)
            Assert.False(isPresent window ConstructorUiId.materialEditorExitConfirm.value, "Keep editing dismisses the confirm")
            Assert.True(isPresent window ConstructorUiId.materialEditorSaveButton.value, "the Save action returns")
            // Cancel again → confirm → Discard closes WITHOUT persisting.
            clickOn window ConstructorUiId.materialEditorCancelButton.value
            Assert.True(isPresent window ConstructorUiId.materialEditorExitConfirm.value)
            clickOn window ConstructorUiId.materialEditorDiscardButton.value
            Assert.False(window.IsVisible, "Discard closes the window")
            match materials.listMaterials ActiveOnly with
            | Ok all -> Assert.Equal(seededCount, List.length all)
            | Error e -> Assert.Fail($"listMaterials failed: %A{e}"))

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``spec 0038 (033): the window chrome (OnClosing) is equally gated`` () =
        HeadlessSession.run (fun () ->
            let materials, _ = freshProxies ()
            let window = MaterialEditorWindow(materials, NewMaterial (newMaterialId ()))
            window.Show()
            Dispatcher.UIThread.RunJobs()
            // The OS title-bar X routes through ChromeCloseIntercepted — the OnClosing override
            // delegates to it (the OS chrome is not reachable through the headless input surface).
            // A pristine editor's chrome close proceeds: nothing intercepted, no confirm.
            Assert.False(window.ChromeCloseIntercepted(), "a pristine chrome close proceeds")
            Assert.False(isPresent window ConstructorUiId.materialEditorExitConfirm.value, "no confirm on a pristine chrome close")
            // A dirty editor's chrome close is intercepted and shows the SAME discard confirm the
            // Cancel button raises — the window stays open.
            setText window ConstructorUiId.materialNameBox.value "Chrome edit"
            Assert.True(window.ChromeCloseIntercepted(), "a dirty chrome close must be intercepted")
            Dispatcher.UIThread.RunJobs()
            Assert.True(window.IsVisible, "an intercepted chrome close leaves the window open")
            Assert.True(isPresent window ConstructorUiId.materialEditorExitConfirm.value, "the chrome close shows the discard confirm")
            // Discard from the confirm then closes for real.
            clickOn window ConstructorUiId.materialEditorDiscardButton.value
            Assert.False(window.IsVisible, "Discard closes the chrome-gated window"))
