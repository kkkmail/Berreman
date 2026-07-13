# Step 039 — impl-plan (ADD_COMPONENT UICOMP_XDUO_0011 SolverHandoffWindow)

## Goal

Declare the inverse flow's terminal screen — `SolverHandoffWindow` — in
`OpticalConstructor.Ui`, plus its headless semantic-tree test. A single-instance
window (its `SolverHandoffWindowKey` already exists in `WindowLauncher`) that
summarizes a RECEIVED collection: per experiment its ordered setup, detector
kind, attached file, and a TYPED per-experiment validation status, plus the fixed
"the actual solver runs separately" message. Basic validation only — NO solver
math, NO normalization.

## Approach (mirrors the CategoryEditorWindow / LibraryWindow component precedent)

Two new Ui files + one new Ui.Tests file; three fsproj registrations.

1. **`OpticalConstructor.Ui/SolverHandoffView.fs`** — the pure view module (the
   `CategoryEditorView` precedent): a `[<RequireQualifiedAccess>] UiIds` module of
   `[<Literal>]` intent-named ids (window, summaryList, solverMessage,
   collectionName, closeButton + per-experiment `statusRow`/`summaryRow`
   derivations); the elevated `ExperimentHandoffStatus` DU (`HandoffReady` /
   `UnspecifiedElements` / `MissingDataFile` / `DataFileParseError` /
   `DataFileValidationError`, each with a `.text`); the pure `validateExperiment`
   (public) running the three checks in order — every non-Sample element bound →
   file present → `ExperimentDataLoad.loadAndValidate` (which folds in step-34
   `validateAgainstExperiment`); a `HandoffRow` and the `Model` (context +
   collection name + rows); `init`/`update`/`view`. No solver call anywhere.

2. **`OpticalConstructor.Ui/SolverHandoffWindow.fs`** — the `HostWindow`
   composition root (the `CategoryEditorWindow` precedent): sets Title/Name/
   AutomationId to `UiIds.window`, builds the `SolverHandoffContext`
   (library + experimentData proxies + the received `ExperimentCollectionSnapshot`
   + `requestClose = this.Close`), and mounts the pure MVU via `Program.mkSimple`.

3. **`OpticalConstructor.Ui.Tests/SolverHandoffWindowTests.fs`** — pure validation
   facts over mock `ExperimentDataProxy` (the `InverseConstructorTests` mock
   precedent) + headless semantic-tree proofs (Category=ui-smoke) driving the REAL
   window by its UiIds: a fully valid collection lists green (ready) statuses; a
   missing file and a range mismatch each show their typed message; the detector
   kind selects the ellipsometric path; no solver math runs (validation only).

## Files to modify

- NEW `OpticalConstructor.Ui/SolverHandoffView.fs` (+ fsproj Compile, after
  `LibraryWindow.fs`, before `TableAndElementRotationView.fs` — it needs no
  workbench type; keep it beside the other window views).
- NEW `OpticalConstructor.Ui/SolverHandoffWindow.fs` (+ fsproj Compile, right
  after its view).
- NEW `OpticalConstructor.Ui.Tests/SolverHandoffWindowTests.fs` (+ fsproj Compile).

## Interpretation choices (recorded per the "don't ask the user" rule)

- **`touches` lists `OpticalConstructor.App`, but I do NOT modify App.** The
  ADD_COMPONENT invariant is "declare the surface; do not wire it into a parent
  view here" — and step 047 (WIRE_UI, `.spec-jsonl`) explicitly OWNS building the
  `WindowLauncher` with "every window factory — Materials, Library, the three
  editors, SolverHandoff". So this step declares the component + its headless
  test; the launcher-button / composition wiring lands at 45/47. `touches` is the
  permissive allow-set, not a requirement.
- **The "received collection" is modelled as the `ExperimentCollectionSnapshot`**
  (name + experiments) — the pure DATA shape the collection persists as (step
  029), with no live handle. Natural "received" value; ties the handoff to the
  persistence contract.
- **UiIds stay in a per-view `UiIds` module** (the current precedent). Step 044
  consolidates every id into one module with a `Handoff` sub-module — a mechanical
  re-point later; values are chosen to survive that move.

## Risks

- FuncUI control recycling in the keyed per-experiment list — use
  `AutomationProperties.AutomationId` (never `Name`) on recycled rows/status
  blocks (the SampleEditor/Category precedent), and `View.withKey` per row.
- Name ambiguity across `Library` / `Experiments` / `MeasuredData` / `Placement`
  opens — qualify the domain references, open only what the Avalonia DSL needs.
- Zero-warning bar (`--warnaserror+:25`): complete matches everywhere.
