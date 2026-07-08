# Step 014 — impl-log

## Progress

- [x] Domain: `Library.starterMultilayerStructure`
- [x] View: `SampleEditorIntent` DU + `init` rewire
- [x] Window + launcher + `SmpMakeMultilayer` distinct path
- [x] Tests migrated + new pure/headless coverage
- [x] Outputs (SoW, exit JSON)

## Files modified

- **`Berreman/OpticalConstructor/OpticalConstructor.Domain/ElementId.fs`**
  (`Library` module) — added the pure public `starterMultilayerStructure :
  SampleStructure`: ONE `Repeated` period of a 2-layer glass(1.52)/vacuum cell
  at the existing private λ/4-at-600 nm thicknesses (`qwGlassThickness` /
  `qwVacuumThickness`, the `multilayerQw` unit cell), `count = 1`.
- **`.../OpticalConstructor.TestWindows/SampleEditorView.fs`** — added the
  3-case `SampleEditorIntent` DU (`NewBlankSample | NewSeededMultilayer |
  EditSample of Sample`); `init` now branches on the intent (blank →
  `emptyStructure`; seeded → `starterMultilayerStructure`; edit → the existing
  sample). Both NEW intents map to the `NewSample` target.
- **`.../OpticalConstructor.TestWindows/SampleEditorWindow.fs`** — ctor's 3rd
  arg is `SampleEditorView.SampleEditorIntent`; Title branches per intent
  ("new multilayer" for the seeded path); `init` is threaded the intent.
- **`.../OpticalConstructor.TestWindows/TableAndElementRotationView.fs`** —
  `EditorLaunchers.openSampleEditor` (field + `defaults`) takes the intent; the
  merged `SmpAdd | SmpMakeMultilayer` arm is split — `SmpAdd` →
  `NewBlankSample`, `SmpMakeMultilayer` → `NewSeededMultilayer`; `SmpEdit` →
  `EditSample sample`.
- **`.../OpticalConstructor.Ui.Tests/SampleEditorWindowTests.fs`** — migrated
  all `None`/`Some x` call sites to `NewBlankSample`/`EditSample x`; added a pure
  test (seeded init opens a NEW sample with one Repeated 2-layer period, distinct
  from blank) and a headless ui-smoke test (the seeded period renders + Save
  persists a NEW sample via `addSample`).
- **`.../OpticalConstructor.Ui.Tests/MainWorkbenchTests.fs`** — `recordingLaunchers`
  distinguishes the intent (`sample-add` / `sample-multilayer` / `sample-edit:`);
  the pure `Add, Edit and Make-multilayer…` test now asserts SmpMakeMultilayer
  records `"sample-multilayer"` (the distinct path); the headless launcher passes
  the intent through; added a headless ui-smoke test that drives the Library
  bay's Make-multilayer button → real editor opens seeded → Save persists.

## Decisions

- **Elevate `Sample option` → `SampleEditorIntent` (3-case DU)** at the
  `init` / window / launcher seam rather than adding a naked bool or a second
  launcher field. This is the repo "no naked bool / elevate every primitive"
  discipline: the third open mode (seeded-new) becomes a NAMED case, and Save's
  fresh-id-vs-in-place choice follows the target each intent maps to. One field,
  one ctor, one `init` — no duplicated seam.
- **Seed lives in the Domain** (`starterMultilayerStructure`), not the view —
  the slice lists `OpticalConstructor.Domain` as touched and the seed is pure
  DATA (`SampleStructure` / `PeriodGroup` / `Repeated` at ElementId.fs), reused
  from the view's `init`. It reuses the existing private λ/4 thicknesses (the
  `multilayerQw` cell), so no new constants are invented.
- **Starter count = 1.** The spec says "a foldable starter **period** … one
  Repeated period **group** of a 2-layer cell … ready for the K-stepper
  (`SetRepeatCount`)". A single period (count 1) is the minimal honest seed; the
  group's inline K-stepper (`GroupCountBy` → `SetRepeatCount`) is the tool that
  builds it up. Bare structure DATA, so Save mints a fresh `SampleId`.

## Testing state

Gates are run by the arc-runner's deterministic gate engine after this worker
exits (IMPLEMENT Invariant 6 — the worker acts, it runs no checks). This
session ran no gate commands.

Step 014 roster: `build`, `unit-tests`, `constructor-unit-tests`, `ui-smoke`,
`ui-tests`. The change is exercised by `ui-tests` (one new pure
`SampleEditorWindowTests` fact) and `ui-smoke` (two new headless facts — one
driving the real `SampleEditorWindow` seeded, one driving the Main workbench's
Make-multilayer button end-to-end). `build` covers the view/window/Domain
signature changes; `unit-tests` and `constructor-unit-tests` projects are
untouched in behaviour (the Domain add is a new value, referenced only by the
view). Every slice requirement lands this round; `commit_ready: true`.

## Gotchas

- **Blast radius of the seam elevation.** ~15 test call sites used the old
  `Sample option` (`None`/`Some x`). All were migrated mechanically to
  `NewBlankSample`/`EditSample x`. `MainWorkbenchTests` opens
  `TableAndElementRotationView`, whose `UiIds`/`Msg`/`Model` would shadow
  `SampleEditorView`'s — so the intent cases and `SampleEditorView.UiIds.*` are
  QUALIFIED there, not `open`ed.
- **Save on a seeded sample needs a name.** The seeded structure is non-empty,
  so `validateSample` only rejects a blank name — the headless Save tests set the
  name box before clicking Save.
- **Title text.** The seeded path titles the window "Sample Editor — new
  multilayer"; the Edit path keeps "Sample Editor — {name}" (WireUiComposition /
  MainWorkbench headless tests still assert "Glass thin film" for Edit).
