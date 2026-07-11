# Step 025 — IMPLEMENT — impl-log

## Progress

- [x] Add `SampleVersionId.firstOf` to `Library` (ElementId.fs), mirroring `MaterialVersionId.firstOf`.
- [x] Remove the `Experiments` module from `ElementId.fs` (truncated at the Library module end).
- [x] New `Experiments.fs` — grown `Experiment` (ordered `ElementDescriptor` setup + optional
      `VariedElement` + `dataFileOpt`), `DataFilePath`, `ElementBinding` (reusing `VersionRef`),
      `PlacementSummary`, `boundVersions` / `versionsInUseSeam`, updated `commit`/`edit`/`attachDataFile`,
      mechanical `ExperimentProxy`/`seedExperiments`.
- [x] Register `Experiments.fs` in the Domain fsproj after `Lifecycle.fs`.
- [x] Ui: `descriptorBinding` + `buildExperimentSetup`; `ExpCommit` captures the live scene setup;
      `chartForExperiment` reads `varied`; AppContext comment updated (seam builder + deferral).
- [x] Tests: reworked `ExperimentProxyTests.fs` for the new shape; added E1/E2/E3, DataFilePath,
      `boundVersions`, used-version-rule injection (block + mint), commit-from-scene.

## Files modified

- `OpticalConstructor.Domain/ElementId.fs`
  - `Library.SampleVersionId` gains `static member firstOf : SampleId -> SampleVersionId`.
  - The `Experiments` module (old lines 793-EOF) REMOVED (moved to `Experiments.fs`).
- `OpticalConstructor.Domain/Experiments.fs` — NEW. The grown module. Compiles after `Lifecycle.fs`
  (its `ElementBinding` reuses the step-020 `VersionRef`). Fully-qualified module name unchanged.
- `OpticalConstructor.Domain/OpticalConstructor.Domain.fsproj` — `<Compile Include="Experiments.fs" />`
  inserted after `Lifecycle.fs`, before `MaterialStore.fs`.
- `OpticalConstructor.Ui/TableAndElementRotationView.fs`
  - NEW `descriptorBinding` / `buildExperimentSetup` (after `experimentElementLabel`).
  - `ExpCommit` → `Experiments.commit (buildExperimentSetup model) …`.
  - `chartForExperiment` → matches `exp.varied` (dark line ⇒ empty chart).
- `OpticalConstructor.Ui/AppContext.fs` — comment updated to name `Experiments.versionsInUseSeam` and
  record why the live wiring is deferred (see Gotchas). Still injects `VersionsInUse.empty`.
- `OpticalConstructor.Tests/ExperimentProxyTests.fs` — reworked + new tests (below).

## Testing state

Gate execution belongs to the arc-runner's deterministic gate engine (IMPLEMENT Invariant 6 — the
worker acts and runs no checks; the per-anchor gate engine is the sole gate authority). No `dotnet
build`/`dotnet test` run here; static verification only (the step-024 precedent). Roster for step 025:
`build`, `unit-tests` (BerremanTests — untouched), `constructor-unit-tests` (ExperimentProxyTests),
`ui-smoke`, `ui-tests`.

Static verification performed (reads/grep, not gate runs):
- Compile order: `VersionRef`/`VersionsInUse` (Lifecycle.fs) precede `Experiments.fs`; nothing between
  `ElementId.fs` and `Lifecycle.fs` references `Experiments` (Propagation.fs mentions it only in a
  comment), so the module move is safe and the descriptor binding can reuse `VersionRef`.
- The ONLY external consumer of the removed `Experiment.elementId`/`elementLabel` was
  `chartForExperiment` (updated); the ONLY `Experiments.commit` call site is `ExpCommit` (updated).
  Ui/Ui.Tests touch only `draft.*` and `exp.id`/`exp.description` (all preserved). PropagationTests
  only `open`s the module (name unchanged).
- All new/edited `match` expressions are exhaustive (FS0025-as-error clean); the ambiguous
  `{ elementId; variable }` literal in `commit` is qualified `VariedElement.…`, and the test `desc`
  literal `ElementDescriptor.…`, so record-field resolution is unambiguous.
- `Set<VersionRef>` (needed by `boundVersions`) is already used in `Lifecycle.fs`, so `VersionRef` is
  comparable; `SampleStructure` `(=)` is used by `decideVersioning`, so the mint test's structure
  equality holds.

New/changed tests (all in `ExperimentProxyTests.fs`, `constructor-unit-tests`): E1 ordered chain +
detector + varied-within-list; E2 no sample; E3 empty dark line + describe; E1/E2 describe;
`DataFilePath` + `attachDataFile`; `boundVersions` (sample yes, preset/dark none); a bound version
blocks sample removal (and empty seam removes freely); editing a USED version mints v2; commit from a
scene captures the ordered chain incl. the detector kind. Net test count RISES (no regression).

## Artifacts

None — pure domain/UI/test source changes, no captured logs/traces produced.

## Gotchas

- **`Experiments` module MOVED out of `ElementId.fs`** into `Experiments.fs` (recorded per the
  "pick-a-default, record-it" rule). Forced by the compile graph: the slice requires the descriptor
  binding to carry a step-020 `VersionRef`, which lives in `Lifecycle.fs` (compiles AFTER
  `ElementId.fs`). Reusing `VersionRef` (over a parallel version-ref DU) beats keeping the module in
  place. The fully-qualified module name `OpticalConstructor.Domain.Experiments` is unchanged, so every
  `open …Experiments` is untouched.
- **`commit` signature changed** to `commit (setup) (collection)` — the bay builds the ordered setup
  from the live scene and passes it. All call sites updated (`ExpCommit`, the domain tests).
- **Dark-line (E3) experiments are constructed DIRECTLY** (record with `varied = None`, empty `setup`),
  not through the interactive draft `commit` — the bay's gated Add still requires an element+variable
  (`canCommit`), preserving the "a detector has nothing to vary ⇒ cannot commit" behaviour. A bay verb
  to author dark lines is a later UI concern (Part L inverse flow).
- **`VersionsInUse` live wiring at the composition root is DEFERRED** (AppContext keeps
  `VersionsInUse.empty`). The domain seam builder `Experiments.versionsInUseSeam` is implemented and
  UNIT-VERIFIED (injected into a real `SampleProxy`: a bound version blocks removal and mints on a
  physics edit). Wiring it into `AppContext.create` needs a mutable experiment source shared with the
  once-built stores, but the live experiment collection lives in the immutable Elmish model, not a
  store this composition root can read; that shared source arrives with the `ExperimentCollectionProxy`
  (spec 0038 Part I, a later step). Until then `VersionsInUse.empty` is truthful — no experiment is
  persisted across app scope (§0.2). The step-20 SoW already anticipated this staging.
- **"Detector kind" is captured via the detector descriptor's binding**, not a duplicated
  `DetectorKind` field: the `Detector`-kind descriptor carries `BoundByEntryId "det-…"`, from which
  step 34 resolves Intensity vs Ellipsometer through the Library. `Experiment.detectorDescriptorOpt`
  is the accessor.
