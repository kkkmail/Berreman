# Step 0203 — impl-log (IMPLEMENT: the copy's pure halves, renumbering and the reference closure)

## Progress

- [x] Red first.
  - The two facts were written before the production code.
  - `dotnet build` of `OpticalConstructor.Tests` failed with exactly four `FS0039` errors: `Experiment` has no
    `renumberedFor` (×2) and `referenceClosure` is not defined (×2). No other error.
- [x] `Experiment.renumberedFor` (Experiments.fs), a static member after `libraryReferences`. It folds `incoming` in
  order, carrying the ids given so far.
  - An experiment keeps its id unless `taken` or an earlier given id holds it.
  - Otherwise it takes `(List.max (taken @ incomingIds @ given)).next`. That list always holds the experiment's own
    id, so it is never empty.
  - Only `id` changes (`{ experiment with id = id }`), and the order is kept.
- [x] `KitLibrary.referenceClosure` (KitLibrary.fs), a free function declared after `KitLibraryStores` and before
  `KitLibraryContext`.
  - The depth-first walk moved here from `snapshotReferences`, unchanged in shape. It resolves the head reference,
    skips a version already visited, and otherwise visits the envelope and pushes `content.references` ahead of the
    rest.
  - It answers the visited envelopes in visit order. The first `resolve` error stops it.
- [x] `snapshotReferences` calls `referenceClosure` with its own `resolveReference`, which is unchanged.
  - `resolveReference` reads the kit's snapshot of an exact version, else the library's held or latest envelope, else
    the kit's highest version. A version the kit holds answers the kit's copy.
  - It then pins and writes, in visit order, each version the kit lacks and has not yet written (`writeLacking`,
    `writeClosure`).
  - `visitReferences` is deleted, so the walk exists only in `referenceClosure`.
- [x] `ExperimentProxyTests`: `copied experiments keep their ids unless the kit holds them, and a clash takes the next
  free id`.
  - The slice's three cases, over `e1` / `e2` / `e3`, so each fact compares whole records:
    - `taken` [1; 2] with E2, E3 → E4 (E2's experiment), E3;
    - `taken` empty → unchanged;
    - `taken` [5] with E1, E5, E2 → E1, E6, E2.
  - Each case also asserts that the outputs equal the inputs once `id` is blanked: every field but `id` is unchanged.
  - Two extra assertions:
    - a duplicated incoming id with `taken` empty: E1, E1, E2 → E1, E3, E2;
    - an empty incoming list answers empty.
  - A private record `RenumberingCase` holds each case. A three-tuple with two `Experiment list` positions would
    breach the AGENTS.md tuple rule.
- [x] `KitLibraryTests`: `the closure of references walks each entry's references depth first, each version once`.
  It is placed after step 139's first fact, whose order it reuses.
  - The resolver reads `BuiltinCatalogue.catalogueData ()`'s envelopes:
    - `EntryVersionReference` by `libraryVersionRef`;
    - `EntryLatestReference` by the highest version;
    - anything else `UnheldEntryErr reference.describe`.
  - [multilayer v1; multilayer v1] answers `Ok (sampleClosure ())`: the sample, then each of its materials followed
    by that material's category, each once.
  - [multilayer v1; test-local GUID] answers `Error (UnheldEntryErr …)`, both for a latest and for an exact reference.
    So the first error stops the walk even after a resolved reference.
- [x] Development runs (below).

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/Experiments.fs`: `Experiment.renumberedFor`.
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/KitLibrary.fs`:
  - `referenceClosure`;
  - `snapshotReferences` now calls it, and `visitReferences` is deleted;
  - one doc line added to `snapshotReferences`.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/ExperimentProxyTests.fs`:
  - the private `RenumberingCase` record and `numbered` helper;
  - the renumbering fact.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitLibraryTests.fs`: the closure fact.

## Decisions

- **`snapshotReferences` calls `referenceClosure` once per top-level reference, not once over the whole list.**
  - The slice says `snapshotReferences` "calls it … and then pins and writes the versions the kit lacks exactly as
    before". It also says "Every other `KitLibraryTests` fact passes as written".
  - The partial half of `a reference the library and the kit cannot resolve records nothing` records an experiment
    whose LAST top-level reference is unheld. It expects the earlier references' closures to be snapshotted and
    pinned already (`assertSnapshots (recordingClosure ()) partial`).
  - One closure over the whole list resolves everything before any write, so that fact would fail.
  - Per-reference closures keep the old single walk's visit order and writes. The old walk finished each top-level
    reference's subtree before the next (`envelope.content.references @ rest`). A later subtree that meets a version
    visited earlier finds that version's whole closure already held or written. `writeLacking` skips held and
    already-written versions.
  - The one observable difference: an unheld reference deep inside ONE top-level reference's closure now stops before
    that reference's own writes, where the old walk had already written the envelopes above it. Writes for earlier
    top-level references are kept. No fact exercises the old order, and every `KitLibraryTests` fact passes as
    written.
  - The doc of `snapshotReferences` gains one contract line (`Each reference's closure is resolved before its
    writes`), citing S6 beside S4.2.
- **`renumberedFor` compares `ExperimentId`s structurally** (`List.contains`, `List.max` over the single-case union).
  It never reads `.value`.
- **Docs:** `renumberedFor` and `referenceClosure` each carry a two-line doc citing spec 0050 S6. The one inline
  comment in `snapshotReferences` states why it makes one closure per reference.

## Testing state

These are development runs only. The gate engine gives the verdict (Invariant 6).

- **build** (`dotnet build Berreman.slnx -c Release -nologo -v:m`, from `Berreman/`): exit 0, 0 warnings, 0 errors.
- **constructor-unit-tests** (whole suite, the gate's command): `Test run summary: Passed!`, 1261 succeeded,
  0 failed (1259 → 1261, +2).
  - Both new identities are `Passed` in `TestResults/constructor-unit-tests.trx`:
    - `OpticalConstructor.Tests.ExperimentProxyTests.copied experiments keep their ids unless the kit holds them, and
      a clash takes the next free id`;
    - `OpticalConstructor.Tests.KitLibraryTests.the closure of references walks each entry's references depth first,
      each version once`.
  - Every other `KitLibraryTests` fact passes unchanged.
- **elevation** (`ASG_ARC_BASE_COMMIT` = the `.arc-base-json` origin `f395597c…`): `ELEV-GATE: OK - 0 new finding(s) in 12
  project(s)`, 195 s; known findings 0. The run widened to all 12 projects because `Berreman.slnx` differs from the
  arc origin.
- **The 14 `invariant-s22-*` greps:** each finds nothing.
- **Line endings:** LF. For the four source files, `git diff --numstat` matches `--ignore-cr-at-eol` exactly.

## Artifacts

- `specs/0050/.artifacts/0203-build-01.log`
- `specs/0050/.artifacts/0203-constructor-unit-tests-01.log`
- `specs/0050/.artifacts/0203-elevation-01.log`

## Gotchas

- **A walk over the whole reference list is not a drop-in for `snapshotReferences`.** Recording relies on the earlier
  references' snapshots surviving a later refusal. Keep the per-reference call if the walk moves again.
- **`renumberedFor` counts every incoming id, not only the earlier ones, when it picks the next free id.** So a
  renumbered experiment never takes an id a later incoming experiment keeps (E1, E1, E2 → E1, E3, E2).
- **`referenceClosure` still resolves a reference whose version it has already visited, before skipping it**, as the
  old walk did. A resolver that fails on a repeat stops the walk.
