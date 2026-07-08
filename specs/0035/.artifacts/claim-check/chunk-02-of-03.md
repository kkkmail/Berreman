{
  "claims": [
    {
      "id": 1,
      "phrase": "`Ribbon.view` renders EVERY bay's content pane in one vertical `StackPanel` with `IsVisible` toggles (`…Controls/Ribbon.fs:90,95-103`)",
      "spec_location": "Part B §B.0 Problem statement",
      "evidence": "[inferred existing-artefact claim; present-tense description, no explicit trigger word] Read Controls/Ribbon.fs:90 -> `let view (state : State) (onSelect : string -> unit) : IView`; lines 95-103 build `panes` mapping every bay to a Border with `Border.isVisible (b.name = activeName)`; enclosing StackPanel at 104-107 is Vertical. Cited symbol/lines present.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 2,
      "phrase": "the module comment there (`:84-89`) records why: swapping one shared content node between two different controls makes FuncUI recycle a styled, named control across bays",
      "spec_location": "Part B §B.0 Problem statement",
      "evidence": "[inferred existing-artefact claim] Read Controls/Ribbon.fs:84-89 -> comment explaining every pane is kept present because swapping one node between two DIFFERENT controls makes FuncUI recycle a styled, named control ('Cannot set Name : styled element already styled'). Matches the claim.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 3,
      "phrase": "the \"Details stays LAST\" pin and the deferred reorder still stand (`…TestWindows/TableAndElementRotationView.fs:235-239`)",
      "spec_location": "Part B §B.0 Problem statement",
      "evidence": "[inferred 'still present' phrasing] Read TableAndElementRotationView.fs:235-238 -> comment 'spec 0033 gap G2 (deferred) ... The original order stands until the Ribbon pane-hosting is made order-independent. Details stays LAST (the 0027/026 pin).'; line 239 `let all = [ ...; details ]` (details last). Confirmed.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 4,
      "phrase": "`mainView` (`…TestWindows/TableAndElementRotationView.fs:2298`) wires the table canvas and pointer/wheel handlers ONLY for a table bay",
      "spec_location": "Part B §B.0 Full-surface bays bullet",
      "evidence": "[inferred existing-artefact claim] Read TableAndElementRotationView.fs:2298 -> `let mainView (model : Model) (dispatch : Msg -> unit) : IView =`; its handlers onPointerPressed/Moved/Released at 2305-2307, onPointerWheelChanged at 2308-2310, and `Border.child (mainTableCanvas model)` at 2311. Cited line now resolves to mainView exactly (prior spec cited :2281; corrected).",
      "verdict": "CONFIRMED"
    },
    {
      "id": 5,
      "phrase": "make **Materials and Library the last two** bays in `mainBays` (`:2265`)",
      "spec_location": "Part B §B.0 Full-surface bays bullet",
      "evidence": "[inferred existing-artefact claim; symbol reference where change lands] Read TableAndElementRotationView.fs:2265 -> `let mainBays (model : Model) (dispatch : Msg -> unit) : Ribbon.Bay list =`. Cited symbol/line present.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 6,
      "phrase": "make **Materials and Library the last two** bays in ... `BayNames.all` (`:239`)",
      "spec_location": "Part B §B.0 Full-surface bays bullet",
      "evidence": "[inferred existing-artefact claim] `module BayNames =` is at TableAndElementRotationView.fs:211; `let all = [ rotation; move; add; render; selector; materials; library; experiments; details ]` is at line 239 (same file, the bare pointer's implied file). BayNames.all resolves at :239.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 7,
      "phrase": "`MaterialComplexityEditor.ofComplexity` returns the typed `UnsupportedComplexity` for a formula-valued `RhoWithDispValue` / `MuWithDispValue` (`…Domain/MaterialComplexityEditor.fs:784,790`)",
      "spec_location": "Part C §C.0 Problem statement",
      "evidence": "[inferred existing-artefact claim] Read MaterialComplexityEditor.fs:784 -> `| Some (MuWithDispValue _) ->` then `Error (UnsupportedComplexity ...)` (785); line 790 -> `| Some (RhoWithDispValue _) ->` then `Error (UnsupportedComplexity ...)` (791). Both inside ofComplexity. Confirmed.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 8,
      "phrase": "the edit state carries only the CONSTANT gyration (`gyration : GyrationClass<RhoValue>`, `:117`)",
      "spec_location": "Part C §C.0 Problem statement",
      "evidence": "[inferred existing-artefact claim] Read MaterialComplexityEditor.fs:117 -> `gyration : GyrationClass<RhoValue>`. Cited field/line present.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 9,
      "phrase": "and Polder (`polder : PolderValue<MuValue>`, `:123`)",
      "spec_location": "Part C §C.0 Problem statement",
      "evidence": "[inferred existing-artefact claim] Read MaterialComplexityEditor.fs:123 -> `polder : PolderValue<MuValue>`. Cited field/line present.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 10,
      "phrase": "`toComplexity` builds only the `RhoWithoutDispValue` / `MuWithoutDispValue` constant cases (`:713,718`)",
      "spec_location": "Part C §C.0 Problem statement",
      "evidence": "[inferred existing-artefact claim] Read MaterialComplexityEditor.fs:713 -> `ScalarMuKind -> Some (MuWithoutDispValue (ScalarMu state.polder.muDiagonal))`; line 718 -> `ActivityOn -> Some (RhoWithoutDispValue { gyration = state.gyration; hand = state.hand })`. Both inside the toComplexity (:699) result-map block. Confirmed.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 11,
      "phrase": "The engine assembly for the dispersive cases already exists and is UNCHANGED by this spec: `RhoWithDispValue.toRhoWithDisp` evaluates each component's `DispersionFormula` per wavelength (`…OpticalProperties/Active.fs:152`)",
      "spec_location": "Part C §C.0 Problem statement",
      "evidence": "Trigger 'already exists'/'UNCHANGED'. Read OpticalProperties/Active.fs:152 -> `member this.toRhoWithDisp : RhoWithDisp =`; line 156 the dispersive case maps each gyration component's `DispersionFormula` via `f.evaluate w` per call. Confirmed.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 12,
      "phrase": "`MuWithDispValue.toMuWithDisp` does the same for the Polder tensor (`Berreman/Berreman/Dispersion.fs:644`)",
      "spec_location": "Part C §C.0 Problem statement",
      "evidence": "Trigger 'already exists and is UNCHANGED' (shared with claim 11). Read Berreman/Berreman/Dispersion.fs:644 -> `member this.toMuWithDisp : MuWithDisp =` on `type MuWithDispValue` (:640); line 647 maps each Polder component's formula per wavelength. Confirmed.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 13,
      "phrase": "each symmetry-allowed gyration component (the components `gyrationComponents` already exposes, `…Domain/MaterialComplexityEditor.fs:385`)",
      "spec_location": "Part C, dispersive sub-branch paragraph",
      "evidence": "Trigger 'already exposes'. Read MaterialComplexityEditor.fs:385 -> `let gyrationComponents (gyration : GyrationClass<RhoValue>) : (GyrationComponent * RhoValue) list =` returning the symmetry-admitted components. Confirmed.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 14,
      "phrase": "edited through the SAME `DispersionModels.modelParameters` + raw `SumOfTerms` coefficient surface built in 0033 (`…Domain/DispersionModels.fs:291`)",
      "spec_location": "Part C, dispersive sub-branch paragraph",
      "evidence": "'the SAME ... surface built in 0033' (project 'same as before'-shape). Read DispersionModels.fs:291 -> `let modelParameters (model : DispersionModel) : ModelParameter list =`; comment 287-290 references the raw `SumOfTerms` escape hatch and spec 0033 gap G7. Cited symbol/line present.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 15,
      "phrase": "the edit state generalises so unchecking Dispersive restores the constant losslessly (the ladder's lossless-uncheck mechanic, commented on `toComplexity` at `…Domain/MaterialComplexityEditor.fs:697`)",
      "spec_location": "Part C, dispersive sub-branch paragraph",
      "evidence": "Trigger 'lossless-uncheck mechanic' (existing discipline). Read MaterialComplexityEditor.fs:696-699 -> the `toComplexity` doc comment 'Each toggle off means its aspect's DEFAULT (constant eps / absent option) regardless of the stored facet — the lossless-uncheck mechanic.'; pointer :697 lands inside that comment. Now anchored (prior spec left this un-anchored; corrected).",
      "verdict": "CONFIRMED"
    },
    {
      "id": 16,
      "phrase": "`validateSample` checks the name only (`…Domain/ElementId.fs:596`)",
      "spec_location": "Part D §D.0, Empty-sample validation bullet",
      "evidence": "[inferred existing-artefact claim] Read ElementId.fs:596 -> `let private validateSample (s : Sample) : Result<unit, SampleError> =`; body (597-599) tests only `String.IsNullOrWhiteSpace s.name`. Confirmed 'checks the name only'.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 17,
      "phrase": "The new rule is proven by a negative case in `…OpticalConstructor.Tests/SampleProxyTests.fs`, beside the blank-name rejection (`:120`)",
      "spec_location": "Part D §D.0, Empty-sample validation bullet",
      "evidence": "[inferred existing-artefact claim] Read SampleProxyTests.fs:120 -> `let ``addSample rejects a blank name as InvalidSample`` () =`, the existing blank-name negative case. Now anchored (prior spec left the tests un-anchored; corrected).",
      "verdict": "CONFIRMED"
    },
    {
      "id": 18,
      "phrase": "every seed builds `filmStructure` (a film, no substrate, `…Domain/ElementId.fs:329`) or `plateStructure` (a substrate, no films, `:337`), so none is structurally empty",
      "spec_location": "Part D §D.0, Empty-sample validation bullet",
      "evidence": "[inferred existing-artefact claim] Read ElementId.fs:329 -> `let private filmStructure ... = { films = [ SingleLayer ... ]; substrate = None; ... }` (a film, no substrate); line 337 -> `let private plateStructure ... = { films = []; substrate = Some ...; ... }` (a substrate, no films). Both cited pointers resolve.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 19,
      "phrase": "The picker in `SampleEditorView.materialRow` (`…TestWindows/SampleEditorView.fs:647`) is a flat snapshot of `MaterialProxy.listMaterials`",
      "spec_location": "Part D §D.0, Searchable material picker bullet",
      "evidence": "[inferred existing-artefact claim] Read SampleEditorView.fs:647 -> `let private materialRow (m : Model) (dispatch : Msg -> unit) : IView =`. Cited symbol/line present (the 'flat snapshot of MaterialProxy.listMaterials' detail is descriptive of the resolved function).",
      "verdict": "CONFIRMED"
    },
    {
      "id": 20,
      "phrase": "the by-`MaterialId` selection contract (`ChooseMaterial of MaterialId`, `:190`) is kept",
      "spec_location": "Part D §D.0, Searchable material picker bullet",
      "evidence": "'is kept' (project 'no change'-shape). Read SampleEditorView.fs:190 -> `| ChooseMaterial of MaterialId` in the Msg DU. Cited case/line present.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 21,
      "phrase": "`SmpAdd` and `SmpMakeMultilayer` both open a blank editor today (`…TestWindows/TableAndElementRotationView.fs:428,855`)",
      "spec_location": "Part D §D.0, Distinct Make multilayer bullet",
      "evidence": "Trigger 'today' (project 'as today'-shape). Read TableAndElementRotationView.fs:428 -> `| SmpAdd` (Msg case; `| SmpMakeMultilayer` at 429); line 855 -> `| SmpAdd | SmpMakeMultilayer ->` handler calling `openSampleEditor model.materials model.samples None` (blank; comment 856-857 confirms both open a NEW sample). Confirmed.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 22,
      "phrase": "a foldable starter period (a 2-layer `Repeated` cell ready for the K-stepper over `SampleStackEditor`, `…Domain/SampleStackEditor.fs:79`)",
      "spec_location": "Part D §D.0, Distinct Make multilayer bullet",
      "evidence": "[inferred existing-artefact claim] Read SampleStackEditor.fs:79 -> `| MakeRepeatBlock of count : int` (comment 77-78: 'Fold the selected contiguous top-level single layers into ONE `Repeated` period group of `count` repetitions'). The count-fold / K-stepper at the cited line is present.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 23,
      "phrase": "`DispersionModels.toEpsAxis` returns `Error (NotAFiniteTermSum _)` for Tauc–Lorentz, Gaussian, Forouhi–Bloomer and Brendel–Bormann (`…Domain/DispersionModels.fs:666-673`)",
      "spec_location": "Part E §E.0, transcendental-models bullet",
      "evidence": "[inferred existing-artefact claim] `let toEpsAxis` at DispersionModels.fs:605 and `toEpsValue` at 700, so 666-673 are inside toEpsAxis. Read 666-673 -> TaucLorentz (666-667), GaussianOscillator (668-669), ForouhiBloomer (670-671), BrendelBormann (672-673), each `Error (NotAFiniteTermSum ...)`. All four confirmed.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 24,
      "phrase": "so a dispersive segment carrying one fails `MaterialComplexityEditor.toComplexity` with `SegmentNotLowerable` (`:634,699,132`)",
      "spec_location": "Part E §E.0, transcendental-models bullet",
      "evidence": "[inferred existing-artefact claim] Read MaterialComplexityEditor.fs:132 -> `| SegmentNotLowerable of reason : string` (def); line 634 -> `let private lowerAxis ...` which returns `Error (SegmentNotLowerable ...)` at 637; line 699 -> `let toComplexity (state : MaterialComplexityEditState) : Result<MaterialComplexity, MaterialComplexityEditError> =`. All three cited lines resolve.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 25,
      "phrase": "`DispersionModels.evaluate` returns `WaveLength -> ComplexRefractionIndex` for every model (`…Domain/DispersionModels.fs:486`)",
      "spec_location": "Part E §E.0, transcendental-models bullet",
      "evidence": "[inferred existing-artefact claim] Read DispersionModels.fs:486 -> `let evaluate (model : DispersionModel) : WaveLength -> ComplexRefractionIndex =`. Cited symbol/line/signature present.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 26,
      "phrase": "`toOpticalProperties` already wraps that closure for them (`:716`)",
      "spec_location": "Part E §E.0, transcendental-models bullet",
      "evidence": "Trigger 'already wraps'. Read DispersionModels.fs:716 -> `let toOpticalProperties (model : DispersionModel) : OpticalPropertiesWithDisp =`; branch 719-721 (`| Error (NotAFiniteTermSum _) ->`) wraps `evaluate model` into an `EpsWithDisp` closure. Confirmed.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 27,
      "phrase": "the core per-axis `EpsAxisDispersion` (`Berreman/Berreman/Dispersion.fs:306`) gains a case representing an **evaluated-directly** model",
      "spec_location": "Part E §E.0, transcendental-models bullet",
      "evidence": "[inferred existing-artefact claim for the current type] Read Berreman/Berreman/Dispersion.fs:306 -> `type EpsAxisDispersion =`. The cited existing type resolves ('gains a case' is the proposed change, not audited).",
      "verdict": "CONFIRMED"
    },
    {
      "id": 28,
      "phrase": "the new case carries the evaluation the way the engine's own `EpsWithDisp of (WaveLength -> Eps)` case in the SAME file already does (`Berreman/Berreman/Dispersion.fs:11-12`)",
      "spec_location": "Part E §E.0, transcendental-models bullet, constraint 1 (Layering)",
      "evidence": "Trigger 'already does'. Read Berreman/Berreman/Dispersion.fs:11-12 -> `type EpsWithDisp =` / `| EpsWithDisp of (WaveLength -> Eps)`. Cited case/lines present in the same file.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 29,
      "phrase": "The finite-term models keep lowering to `RealNK` / `ComplexEps` (`Berreman/Berreman/Dispersion.fs:307-308`) exactly as today",
      "spec_location": "Part E §E.0, transcendental-models bullet",
      "evidence": "Trigger 'exactly as today' (project 'as today'). Read Berreman/Berreman/Dispersion.fs:307 -> `| RealNK of n : DispersionFormula * k : DispersionFormula`; line 308 -> `| ComplexEps of ComplexDispersionFormula`. Cited cases/lines present.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 30,
      "phrase": "which the existing `Assert.Equal(model, back)` and `Assert.Equal(Ok axis, toEpsAxis model)` tests depend on (`…OpticalConstructor.Tests/DispersionModelsTests.fs:121,349`)",
      "spec_location": "Part E §E.0, transcendental-models bullet, constraint 2 (Equality)",
      "evidence": "Trigger 'existing ... tests'. Read DispersionModelsTests.fs:121 -> `Assert.Equal(model, back)`; line 349 -> `Assert.Equal(Ok axis, toEpsAxis model)`. Both cited assertions present at the cited lines.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 31,
      "phrase": "both render through a primitive hand-rolled FuncUI canvas (`NkDispersionChart.inlineCanvas`, `…TestWindows/NkDispersionChart.fs:108`; used at `…TestWindows/MaterialEditorView.fs:856`)",
      "spec_location": "Part E §E.0, shared-chart-control bullet",
      "evidence": "[inferred existing-artefact claim] Read NkDispersionChart.fs:108 -> `let inlineCanvas (autoId : string) (chart : ExperimentChart) : IView =` (hand-rolled canvas geometry 109-112); MaterialEditorView.fs:856 -> `NkDispersionChart.inlineCanvas UiIds.previewChart chart`. Both cited pointers resolve.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 32,
      "phrase": "0033 Part D already REAL-MOVED the shared ScottPlot chart into `OpticalConstructor.Controls` with the dual n-left / k-right Y axes (`…Controls/ExperimentChart.fs`, `…Controls/ChartWindow.fs`), but only as the pop-out window (`ChartWindow.fs:11-19`)",
      "spec_location": "Part E §E.0, shared-chart-control bullet",
      "evidence": "Trigger 'already REAL-MOVED'. Glob confirms OpticalConstructor.Controls/ExperimentChart.fs and .../ChartWindow.fs exist. Read ChartWindow.fs:11-19 -> comment 'the pop-out interactive chart window ... A plain Avalonia `Window` hosting a `ScottPlot.Avalonia.AvaPlot` ... never opened under the headless `ui-smoke` gate'. Confirms the pop-out-window scoping.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 33,
      "phrase": "The pure chart-model builder `NkDispersionChart.nkDispersionChart` (`:55`) is reused unchanged.",
      "spec_location": "Part E §E.0, shared-chart-control bullet",
      "evidence": "Trigger 'reused unchanged'. Read NkDispersionChart.fs:55 -> `let nkDispersionChart (o : OpticalPropertiesWithDisp) (u : UnitOfMeasure) (range : Range<WaveLength>) : ExperimentChart =`. Cited pure builder present at :55.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 34,
      "phrase": "The primary eps branch is a sticky `Dispersive` toggle (\"off = constant n, k\", `…TestWindows/MaterialEditorView.fs:594,604`)",
      "spec_location": "Part E §E.0, Constant-vs-Dispersive presentation bullet",
      "evidence": "[inferred existing-artefact claim] Read MaterialEditorView.fs:594 -> `let private togglesRow (m : Model) (dispatch : Msg -> unit) : IView =`; line 604 -> `labelBlock \"Dispersion model (off = constant n, k):\"` (and 609 clickBox UiIds.dispersiveToggle 'Dispersive'). Cited lines carry the Dispersive toggle and the 'off = constant n, k' label. Confirmed.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 35,
      "phrase": "update the `absorbingToggle` / `dispersiveToggle` acceptance tests (`:56,58`) to the two-option model",
      "spec_location": "Part E §E.0, Constant-vs-Dispersive presentation bullet",
      "evidence": "[inferred existing-artefact claim] Bare `:56,58` resolves to the last-named file MaterialEditorView.fs: line 56 -> `let absorbingToggle = \"AbsorbingToggle\"`, line 58 -> `let dispersiveToggle = \"DispersiveToggle\"`. The cited symbol names exist at exactly the cited lines. NOTE: those lines are the [<Literal>] UiId string CONSTANTS; the click-driven acceptance tests referencing UiIds.absorbingToggle / UiIds.dispersiveToggle live in OpticalConstructor.Ui.Tests/MaterialEditorWindowTests.fs (Grep hits :141-142, :515-591, :674-704), not at :56,58 — the 'acceptance tests' descriptor is a semantic characterization (out of audit scope). Pointer resolves to the named symbols.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 36,
      "phrase": "the presentation change MUST NOT alter the `MaterialComplexity` that `MaterialComplexityEditor.toComplexity` (`…Domain/MaterialComplexityEditor.fs:699`) derives",
      "spec_location": "Part E §E.0, Constant-vs-Dispersive presentation bullet",
      "evidence": "Trigger 'MUST NOT alter ... derives' (unchanged-shape). Read MaterialComplexityEditor.fs:699 -> `let toComplexity (state : MaterialComplexityEditState) : Result<MaterialComplexity, MaterialComplexityEditError> =`, the derivation the claim references. Now anchored (prior spec left this un-anchored; corrected).",
      "verdict": "CONFIRMED"
    }
  ]
}
