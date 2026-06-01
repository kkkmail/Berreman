# 005 — Impl log: Optical-system construction layer (Part B UI)

## Progress

- [x] Baseline build/tests green (build 0 errors; constructor baseline 43; BerremanTests 70)
- [x] Dump serialized JSON ground-truth for schema design (temp dump test → captured shape, then removed)
- [x] Fill schema `$defs` (beamNode/beamBranch/opticalSystem/layer)
- [x] StackEditor.fs (pure transforms + selectors + substrate switch; Avalonia-free)
- [x] ConstructionPage.fs (page MVU + presentation helpers; Avalonia-free)
- [x] StackEditTests.fs (AC-B5/B6/B7/B1/B10)
- [x] Extend ProjectJsonRoundtripTests.fs (AC-B9 round-trip + negative validation)
- [x] Register new files in fsproj(s)
- [x] Gates: build / unit-tests / constructor-unit-tests — all PASS

## Files modified

New:
- `OpticalConstructor/OpticalConstructor.Ui/StackEditor.fs` — pure `films`
  transforms (add/delete/reorder/duplicate/group/rotate), incident/exit medium
  selectors, three-state substrate switch, `StackMsg`/`applyStackMsg`, default-unit
  display + `layerRowLabels`.
- `OpticalConstructor/OpticalConstructor.Ui/ConstructionPage.fs` — `NodePath`
  navigation, `descendantCount`, reused-solver `solveNode`/`solveSubtree`, page
  `Model`/`Msg`/`update`, `navEntry`, committable `projectFilePath`/`saveProject`,
  presentation helpers (`isNodeBusy`/`confirmationPrompt`/`canUndo`).
- `OpticalConstructor/OpticalConstructor.Tests/StackEditTests.fs` — 21 tests across
  AC-B5/B6/B7/B1/B10 + §B.10 items 1 & 6.

Edited:
- `OpticalConstructor/OpticalConstructor.Ui/OpticalConstructor.Ui.fsproj` — register
  `StackEditor.fs`, `ConstructionPage.fs` (after `MaterialPreview.fs`).
- `OpticalConstructor/OpticalConstructor.Storage/schema/optical-constructor-project.schema.json`
  — fill the `beamNode`/`beamBranch`/`opticalSystem`/`layer` `$defs`
  (`opticalProperties`/`constructorElement` left as permissive slice-003 anchors).
- `OpticalConstructor/OpticalConstructor.Tests/ProjectJsonRoundtripTests.fs` — add the
  AC-B9 rich-tree round-trip + a negative validate-on-load test.
- `OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj` —
  register `StackEditTests.fs`.

## Testing state

`commit_ready = true`. All three gates pass locally at Release/x64:

- `build` — exit 0, "Build succeeded.", 0 Error(s), no lowercase `error`. `.artifacts/005-build.log`.
- `unit-tests` — exit 0, Passed 70 / Skipped 5 / Total 75 (`BerremanTests` untouched; baseline 70 held). `.artifacts/005-unit-tests.log`.
- `constructor-unit-tests` — exit 0, Passed 66 / Total 66 (slice-004 baseline 43). `.artifacts/005-constructor-unit-tests.log`.

Every R-1..R-6 requirement and AC-B1/B5/B6/B7/B9/B10 is addressed this round; nothing
deferred to a "round 2". The FuncUI `view` bodies are deferred by design (P3 keeps the
model/`update` Avalonia-free; the views are a later UI-wiring slice) — this is a
deliberate scope boundary, not an unfinished requirement.

## Artifacts

- `.artifacts/005-build.log`, `.artifacts/005-unit-tests.log`,
  `.artifacts/005-constructor-unit-tests.log` — captured gate runs.

## Decisions / Gotchas

- **Empirical schema design.** Captured the real serialized JSON via a throwaway dump
  test before authoring the `$defs`, then removed it. Key finding: `children` is an
  array of `[branch, node]` pairs (DU map key), and `Thickness`/`substrate`/`element`/
  `waveLength` use the `Case`/`Fields` adjacent-tag (or bare strings for fieldless
  cases). Filled `$defs` match this exactly and stay permissive on DU-tagged fields so
  validate-on-load never rejects a legitimate document.
- **"Group" = contiguous gather** (not a repeat/period builder — that is Part J §J.2),
  since `films` is a flat `List<Layer>` and the op must produce a new flat list.
- **`SubstrateChoice` case names** `AsThinFilm`/`AsPlate`/`AsWedge` avoid clashing with
  the engine `Substrate` cases.
- **Module/type name shadowing** (`BeamTree`/`MaterialLibrary`) — call module lets
  unqualified after `open` (`solve`, `standard`).
- FuncUI container DSL (`StackPanel.orientation/spacing/children`) did not resolve
  cleanly against the 1.6 NuGet in a first view attempt; rather than risk the `build`
  gate on an uncertain view API — and because P3 mandates an Avalonia-free core and the
  ACs are all model/`update`-level — the FuncUI views were dropped this round and the
  cores + presentation-data helpers retained. Recorded here per the system-prompt
  skepticism/decision-logging rule.
