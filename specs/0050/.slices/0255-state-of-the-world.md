# Step 0255 — state of the world

## Where we are

Spec 0050, section S8.2 ("Setups as slots"), first step. Before slots exist, this step adds the identities a
setup is stored and referenced under, and the one dial rule its slots read, as pure Domain data in `Kit.fs` and
`ItemModels.fs`. These are `SetupName`, `SetupRevision`, `SetupReference`, `SetupError` and
`DialSign.physicalAngleOf` / `readingOf`. Nothing reads them before step 256. `MuellerReconstruction.physicalAngle`
now calls `DialSign`'s rule, and item labels and setup names share one suffix sequence, `FileStem.firstFreeText`.

## What's working

- Added `SetupName` (keeps its text, refuses a blank one, takes its file stem from the shared rule, next free
  suffix, first free `Setup n`), plus `SetupRevision`, `SetupReference` and the seven-case `SetupError`.
- Moved the dial rule onto `DialSign.physicalAngleOf`, with its inverse `readingOf`. `physicalAngle` calls it, so
  the reference pipeline's numbers are unchanged.
- Moved the `<text> (n)` suffix sequence out of `ItemLabel.nextFreeAmong` into `FileStem.firstFreeText`, which
  item labels and setup names share.
- Added five facts: four in `KitTests` and one in `ItemModelsTests`.

## Tests

No gate verdict is claimed here (Invariant 6). After exit, the arc-runner runs the frozen roster: build,
elevation, constructor-unit-tests (whole), unit-tests over `BerremanTests.MuellerReconstructionTests`, and the
fifteen s26 invariants.

The worker's development runs over the final tree used each gate's own command. Logs are under
`specs/0050/.artifacts/0255-dev-*`.

- **build:** exit 0, 0 warnings, 0 errors.
- **constructor-unit-tests:** `Passed!`, 1408 succeeded, 0 failed, 0 skipped, 67 s (deadline 180 s). That is
  1403 + 5, and all five census facts are present in the TRX. Step 221's
  `KitItemTests.an added item's label takes the next free suffix among the labels the kit holds` passes unchanged.
- **unit-tests (selected `BerremanTests.MuellerReconstructionTests`):** `Passed!`, 29 succeeded, 0 failed,
  0 skipped, about 4 s wall (deadline 7200 s), with its numbers unchanged.
- **elevation:** `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, 196 s (deadline 600 s), known findings 0.
  An earlier run over the slice's literal `firstFreeText` signature failed ELEV010; see Gotchas.
- **The fifteen s26 invariants,** by their greps, all find nothing.
- No development run exceeded its deadline.

```yaml
gates:
  constructor_unit_tests: 1408
  berreman_unit_tests: 29
```

`berreman_unit_tests` is the count under this step's class filter (as step 0205 reported it).

## Architecture

- **One dial rule.** `DialSign.physicalAngleOf zero reading` is `sign · (reading − zero)`. It keeps the exact
  floating-point expression the old `physicalAngle` held, so the numbers are bit-identical; a fact asserts exact
  equality. `readingOf` inverts it, because the sign is ±1. A reading is an `Angle` on the dial's scale, so no
  reading type is added.
- **One suffix sequence.** `FileStem.firstFreeText text taken` answers the first of `text`, `text (2)`, … whose
  `FileStem.ofText` matches no taken text's stem, ignoring case. `ItemLabel.nextFreeAmong` and
  `SetupName.nextFreeAmong` wrap its answer. `SetupName.firstFree` walks `Setup 1`, `Setup 2`, … and keeps the
  first one that `firstFreeText` returns unchanged, so the case-insensitive stem test lives in one place.
- The setup types are appended at the end of `Kit.fs` in the slice's order. Each `SetupError` case carries a
  `reason : string` and ends in `Err`.

## Deferred

Nothing in this step's scope is deferred.

## Gotchas

- **Deviation: `FileStem.firstFreeText (text : string) (taken : string list) : string`.** The slice wrote
  `taken : FileStem list`. ELEV010 refuses a signature that mixes a bare `string` with one of our types, and
  members are not exempt from that rule (measured: `FAILED: ELEV010 … Kit.fs:807:23 - firstFreeText mixes bare
  primitive(s) string with elevated types`). `FileStem.ofText` passes only because `ofText` is on the analyzers'
  elevation-pattern list. The analyzers win (AGENTS.md; spec 0048 `.manual/002` R6), and the slice adds no other
  type that could elevate `text`. So `taken` is the taken names' **texts**, and `firstFreeText` takes their stems
  itself by the same `FileStem.ofText` rule. Behaviour is identical.
- **For steps 286 and 306:** they call `FileStem.firstFreeText this.value` "over the taken names' `fileStem`s". Pass
  the taken names' texts instead: `taken |> List.map (fun held -> held.value)`. That is `.value` inside the name
  type's own member, which ELEV004 sanctions.
- `SetupReference` is now the last-declared record with a `name` field in `Kit`. Later code that builds an
  unannotated `{ name = …; … }` record in a file opening `Kit` should annotate it; nothing does today.
- A first build after MathNet's obj is stale prints its two pre-existing `SYSLIB0051` warnings (vendored C#). The
  next build is clean.

## Changelog

- 2026-11-10 — Step 0255: setup identities (`SetupName`, `SetupRevision`, `SetupReference`, `SetupError`), the one
  dial rule on `DialSign`, and the shared `FileStem.firstFreeText` suffix sequence; five new facts.
