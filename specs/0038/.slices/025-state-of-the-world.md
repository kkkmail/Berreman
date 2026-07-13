# Step 025 — state of the world

## Where we are

Spec 0038 Part I, step 025. The Experiments domain now holds the FULL setup. An
`Experiment` grew from a single varied-element reference into an ordered
`ElementDescriptor` list (kind + placement/orientation summary + bound entry
reference by version where versioned) with the varied element identified within
the list — sample and light source optional, the detector part of the chain, and
dark-line experiments first-class. This closes the step-020 loop: `VersionsInUse`
now has a real builder (`versionsInUseSeam`) computed over the experiment
descriptors, unit-verified against the versioned sample store's used-version rule.
Steps 26+ build the scene / experiment-collection proxies and the inverse flow on
top of this shape.

## What's working

- Grow `Experiments.Experiment` into an ordered `ElementDescriptor` setup (kind,
  placement/orientation summary, versioned/preset binding) with the varied element
  identified within the list, replacing the `elementId`/`elementLabel` pair.
- Make sample and light source optional: E1 (source+polarizer+sample+rotating
  polarizer), E2 (no sample), and E3 (empty dark line) all construct and describe.
- Add `DataFilePath` (elevated path DU) and a per-experiment `dataFileOpt` slot
  with `attachDataFile`; the detector kind rides in the chain for the data-file
  shape (step 34).
- Implement `boundVersions` / `versionsInUseSeam` over the experiment descriptors,
  unit-verified: a bound sample version blocks removal and mints on a physics edit.
- Capture the live scene as the ordered setup on commit (the Experiments bay
  builds descriptors, resolving sample bindings by version through the Library).
- Move the `Experiments` module to its own `Experiments.fs` (after `Lifecycle.fs`)
  so the descriptor binding reuses the step-020 `VersionRef`; module name unchanged.

## Tests

Gate execution is the arc-runner gate engine's job (IMPLEMENT Invariant 6 — the
worker acts and runs no checks). Static verification only this round (reads/grep),
per the step-024 precedent. Step-025 roster: `build`, `unit-tests`,
`constructor-unit-tests`, `ui-smoke`, `ui-tests`.

- `constructor-unit-tests` (`ExperimentProxyTests.fs`): reworked for the new shape
  plus new coverage — E1/E2/E3 construct+describe; `DataFilePath`+`attachDataFile`;
  `boundVersions` (sample yes, preset/dark none); a bound version blocks sample
  removal (empty seam removes freely); editing a USED version mints v2; committing
  from a scene captures the ordered chain incl. the detector. Net count rises.
- `unit-tests` (BerremanTests) untouched; `ui-smoke`/`ui-tests` exercise the
  updated `ExpCommit`/`chartForExperiment` through the message path (draft state and
  `exp.id`/`exp.description` preserved).

```yaml
gates:
  berreman_unit_tests: 119
  constructor_unit_tests: 589
  ui_smoke_tests: 158
  ui_tests: 425
```

## Architecture

- The `Experiments` module moved from `ElementId.fs` to `Experiments.fs`, compiled
  after `Lifecycle.fs`, so the descriptor's `ElementBinding` can REUSE the
  step-020 `VersionRef` rather than duplicate a parallel version-ref DU. The
  fully-qualified module name is unchanged, so all consumers are source-compatible.
- The binding is `ElementBinding = BoundByVersion of VersionRef | BoundByEntryId of
  string | Unbound`; `boundVersions : Experiment list -> Set<VersionRef>` folds the
  descriptors, and `versionsInUseSeam : (unit -> Experiment list) -> VersionsInUse`
  is the seam the stores' saves consult.
- The varied element is optional (`VariedElement option`) so a dark line is
  representable; the interactive `commit` still gates the append on an
  element+variable (`canCommit`) and only captures the scene-built setup.
- The placement/orientation summary is a serializable snapshot (`TablePoint` +
  three `Angle`s), so a descriptor survives the live element's removal and save-load
  without holding an in-memory element reference.

## Deferred

- **Live composition-root wiring of `versionsInUseSeam`.** `AppContext.create`
  keeps `VersionsInUse.empty`; the real seam needs a mutable experiment source
  shared with the once-built stores, but the live collection lives in the immutable
  Elmish model. That shared source lands with the `ExperimentCollectionProxy` (a
  later Part I step). The domain seam itself is implemented and unit-verified.
- A bay verb to author dark-line experiments interactively (the empty-setup /
  no-varied path) — the inverse flow (Part L) owns that UX.
- Resolving the detector's `DetectorKind` at capture time / the data-file schema
  itself — step 34 consumes the detector descriptor's binding.

## Gotchas

- The `Experiments` module lives in `Experiments.fs` now, NOT `ElementId.fs`
  (forced by the `VersionRef` compile order). Search by module name, not file.
- `Experiments.commit` takes the ordered `setup` first now; the bay builds it from
  the live scene (`buildExperimentSetup`) and passes it. The single call site is
  `ExpCommit`.
- Dark lines are built as records (`varied = None`, empty `setup`); the interactive
  draft `commit` still refuses to append without an element+variable, so the
  "a detector cannot commit" behaviour is preserved.
- "Detector kind" is captured as the `Detector`-kind descriptor's preset binding
  (`BoundByEntryId "det-…"`), resolved to Intensity/Ellipsometer through the Library
  by step 34 — not duplicated onto the descriptor.

## Changelog

- 2026-07-11 — Step 025 (IMPLEMENT): grew `Experiments.Experiment` into an ordered
  `ElementDescriptor` setup (optional sample/source, detector in-chain, dark lines
  first-class); added `DataFilePath` + `dataFileOpt`; implemented `boundVersions` /
  `versionsInUseSeam` over the descriptors (unit-verified against the sample store's
  used-version rule); moved the module to `Experiments.fs` after `Lifecycle.fs`;
  wired the Experiments bay to capture the live scene on commit.
