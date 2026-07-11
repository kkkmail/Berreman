# Step 025 — IMPLEMENT — impl-plan

## Goal

Grow `Experiments.Experiment` into a FULL setup: an ordered `ElementDescriptor`
list (kind + placement/orientation summary + bound entry reference by version
where versioned) with the varied element identified within the list, replacing
the single `elementId`/`elementLabel` pair. Sample and light source optional
(E1/E2/E3). Add `DataFilePath` (elevated path DU) + `dataFileOpt`. Implement the
step-20 `VersionsInUse` over the experiment descriptors and inject it into the
step-21/22 store saves. Update `ExperimentProxy`/`seedExperiments` mechanically.

## Key constraint — compile order

`VersionRef` / `VersionsInUse` live in `Lifecycle.fs`, which compiles AFTER
`ElementId.fs` (where `Experiments` currently sits). The descriptor's binding
must reuse the existing `VersionRef` (reuse discipline), so the `Experiments`
module MUST compile after `Lifecycle.fs`.

**Decision:** MOVE the `Experiments` module out of `ElementId.fs` into a new
`Experiments.fs` compiled right after `Lifecycle.fs` (before the stores). The
fully-qualified module name `OpticalConstructor.Domain.Experiments` is unchanged,
so every `open …Experiments` consumer keeps working. No Domain file below
`ElementId.fs` depends on `Experiments` (Propagation.fs mentions it only in a
comment), so the move is safe.

## Files to modify

- `OpticalConstructor.Domain/ElementId.fs` — remove the `Experiments` module;
  add `SampleVersionId.firstOf` in the `Library` module (mirror
  `MaterialVersionId.firstOf`).
- `OpticalConstructor.Domain/Experiments.fs` — NEW. The grown module:
  `DataFilePath`, `ElementBinding` (reuses `VersionRef`), `PlacementSummary`,
  `ElementDescriptor`, `VariedElement`, the new `Experiment` (setup + varied +
  dataFileOpt), `ExperimentDraft`/`ExperimentCollection` (draft unchanged),
  `commit`/`edit`/`remove`/choosers, `versionsInUse` / `versionsInUseSeam`,
  `ExperimentProxy` + `seedExperiments` + `createInMemory`.
- `OpticalConstructor.Domain/OpticalConstructor.Domain.fsproj` — add
  `Experiments.fs` after `Lifecycle.fs`.
- `OpticalConstructor.Ui/TableAndElementRotationView.fs` — `ExpCommit` builds the
  ordered setup from the live scene (resolving each element's binding through the
  library proxy) and passes it to `commit`; `chartForExperiment` reads
  `exp.varied`; add `buildExperimentSetup` / `descriptorBinding` helpers.
- `OpticalConstructor.Tests/ExperimentProxyTests.fs` — rework for the new shape;
  add E1/E2/E3 construct+describe, versionsInUse + used-version-rule injection,
  commit-from-scene captures the ordered chain incl. detector.

## Design

- `ElementBinding = BoundByVersion of VersionRef | BoundByEntryId of string | Unbound`
  (`.versionRefOpt` projection).
- `PlacementSummary = { position : TablePoint; r1/r2/r3 : Angle }` +
  `ofPlacement`.
- `ElementDescriptor = { elementId; label; kind : CatalogueKind; placement; binding }`.
- `VariedElement = { elementId : ElementId; variable : VariableParameter }`.
- `Experiment = { id; setup : ElementDescriptor list; varied : VariedElement option;
  measurement; range; dataFileOpt : DataFilePath option }` with `variedDescriptor`
  / `variedLabel` / `detectorDescriptorOpt` / `description` members.
- `versionsInUse : Experiment list -> Set<VersionRef>` and
  `versionsInUseSeam : (unit -> Experiment list) -> VersionsInUse`.
- `commit setup c`: editing → update in place (preserving `dataFileOpt`);
  new → append only when draft identifies element+variable (dark lines are
  constructed directly, not via the interactive draft this slice).

## Risks

- Moving a module is a big mechanical diff — verify nothing between ElementId.fs
  and Lifecycle.fs references `Experiments`.
- Composition-root live wiring of `versionsInUseSeam` needs a mutable experiments
  source shared with the once-built stores; experiments currently live in the
  immutable Elmish model. The DOMAIN seam is fully unit-verified (injected into a
  store in tests); the live app wiring stays `VersionsInUse.empty` until the
  experiment-store proxy lands (a later Part I step). Recorded in Gotchas.
