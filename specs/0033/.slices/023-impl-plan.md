# Impl plan — spec 0033, slice 023 (ADD_COMPONENT UICOMP_XDUO_0004 MaterialEditorWindow)

## Approach

Three layers, mirroring the slice-022 SampleEditorWindow precedent:

1. **Domain: the pure MaterialComplexity edit model**
   (`OpticalConstructor.Domain/MaterialComplexityEditor.fs`, new, compiled
   last after SampleStackEditor.fs). Avalonia-free state + message DU +
   `applyMaterialComplexityMsg` returning `Result` with typed reason-carrying
   errors — the SampleStackEditor discipline. The state stores every ladder
   facet INDEPENDENTLY of its toggle, and the derivation
   (`toComplexity : state -> Result<MaterialComplexity, _>`) reads a facet
   only while its toggle is on — unchecking therefore restores the default
   losslessly by construction, and re-checking restores the user's edits.
   - Anisotropy 3-way (`Isotropic | Uniaxial | Biaxial`) × transparency
     (`Transparent | Absorbing`) × dispersion (`NonDispersive |
     DispersiveSegments`) select the `ConstantEpsValue` case or the
     `EpsDispersiveValue` flavor; three `ComplexRefractionIndex` slots feed
     1/2/3 principal axes.
   - Segments: `{ interval : WaveLengthInterval; model1/2/3 : DispersionModel }`
     (three axis slots so an existing per-axis tree seeds losslessly via the
     `SumOfTerms` identity); derivation lowers via `DispersionModels.toEpsAxis`.
     ForouhiBloomer / BrendelBormann are accepted as picks (the user's entry
     is preserved) but surface the typed `NotAFiniteTermSum` reason from
     `toComplexity` — the Part G honest-negative-scope precedent; picker
     defaults reuse the published sets from DispersionModelsTests.
   - Gyration: state stores the full `GyrationClass<RhoValue>` + `Handedness`;
     `availableGyrationClasses : Anisotropy -> _ list` constrains the picker
     (isotropic ⇒ cubic; uniaxial ⇒ the diagonal two-component class;
     biaxial ⇒ 222 / monoclinic-2 / monoclinic-m / triclinic-1). Enabling
     activity (or switching anisotropy with activity on) snaps an un-offered
     class to the first offered one; `ofComplexity` seeding never snaps.
   - Polder mu: scalar vs gyromagnetic kind over one stored
     `PolderValue<MuValue>` + `GyrationAxis`.
   - `ofComplexity : MaterialComplexity -> Result<state, _>` (dispersive
     rho/mu → typed unsupported error → the window falls back to view-only),
     `toComplexity` round-trips the editable built-ins value-identically.
   - The `imaginaryIndexGainWarning` RULE (Ui/Validation.fs:92) restated as a
     pure helper over sampled k values — Domain/TestWindows cannot reference
     Ui (the 021 `validateRepeatCount` precedent; Ui is not in `touches`, so
     the spec's "move with the editors" cannot land this round).

2. **TestWindows: MaterialEditorView.fs (pure MVU + FuncUI view) +
   MaterialEditorWindow.fs (HostWindow composition root)**, after
   SampleEditorWindow.fs. View model holds context (`MaterialProxy` +
   `requestClose`, `[<ReferenceEquality>]`), target (`NewMaterial |
   ExistingMaterial of MaterialId`), name/category/description, the Domain
   edit state, and a view-only mode for `complexity = None` entries (no
   toggles, no Save — the "no Edit affordance" mandate inside the component).
   All 13 mandated UiIds plus derived families (anisotropy options, index
   boxes, segment boxes, class/axis options). Save derives the complexity,
   builds the entry with `complexity = Some model` and
   `properties = model.toProperties`, mints `MaterialId.create` for new /
   updates in place, closes on `Ok`, surfaces typed reasons on `Error`.
   Live preview: the step-19 `nkDispersionChart` + `nkDispersionStyle` over
   `toProperties` (400–700 nm), drawn as an inline dual-axis canvas with
   per-side bounds from the 018 `ChartStyle.dataBounds`; a negative sampled
   k shows the advisory gain warning.

3. **Ui.Tests: MaterialEditorWindowTests.fs** — pure contract tests
   (`ui-tests`) for the Domain model + view update (no window), and headless
   proofs (`ui-smoke`) driving the real window by UiIds, incl. all four
   acceptance criteria: biaxial ⇒ three principal-index fields; activity on
   uniaxial ⇒ only the uniaxial gyration class offered; toggle uncheck ⇒
   lossless default restore; Save round-trip through `MaterialProxy` with
   `complexity = Some`.

TDD: the test file + fsproj entry land first; the red build (FS0039 on the
missing production symbols) is captured to `.artifacts/023-red-tdd.log`.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/MaterialComplexityEditor.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/OpticalConstructor.Domain.fsproj` (compile entry)
- `Berreman/OpticalConstructor/OpticalConstructor.TestWindows/MaterialEditorView.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.TestWindows/MaterialEditorWindow.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.TestWindows/OpticalConstructor.TestWindows.fsproj` (compile entries)
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/MaterialEditorWindowTests.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/OpticalConstructor.Ui.Tests.fsproj` (compile entry)

## Risks

- Headless font metrics push long control rows off-screen → WrapPanels for
  every clickable row (the 022 lesson).
- FuncUI Elmish needs model equality → `[<ReferenceEquality>]` on the view
  Model (holds `MaterialEntry`/dispersion functions).
- The mandated `DispersionModelPicker` literal vs per-segment pickers → the
  literal goes to segment 0's picker, `DispersionModelPicker_<i>` for later
  segments (the 022 `RepeatCountStepper` family precedent).
- "Activity toggle REMOVED for non-rotating choices": every current
  anisotropy choice offers ≥1 rotating class, so the rule is implemented
  structurally (toggle present only when `availableGyrationClasses` is
  non-empty) — recorded as a decision.
