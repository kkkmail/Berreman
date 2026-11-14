# Step 0406 — impl log

## Progress

- [x] Domain: `StudyPublishing.fs` (`PublishError`, `Study.calibratedItemsWith`) and its compile line after `FitRuns.fs`.
- [x] Tests: `StudyPublishingTests.fs` (four facts) and its compile line at the end of the list.
- [x] Development runs: build, the whole constructor suite, elevation and the twenty s40 invariants.
      A mutation check turned every new fact red.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/StudyPublishing.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/OpticalConstructor.Domain.fsproj` (one compile line)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/StudyPublishingTests.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj` (one compile line)

These are exactly the step's authorized `edits`. No other file changed.

## Decisions

- **D1 — the refusals run in phases, in the brief's sentence order.** The order is:
  1. foreign run;
  2. unknown item, over every item-owned entry in entry order;
  3. undetermined σ, over every item-owned entry in entry order;
  4. no item-owned entry;
  5. nothing new to publish;
  6. the item's own refusal.

  So a run with both a missing item and an open σ answers `UnknownPublishedItemErr`. The brief lists the checks in
  that order, so I did not resolve each entry's item and σ together.
- **D2 — a σ the run does not record at all counts as undetermined.** The brief says "`None`, or no element". The
  third fact checks both forms: CPL1's `sigmaOpt = None`, and no element for CPL1.
- **D3 — a follower is built from the run's calibrations before held states are dropped.** So when the leader
  already holds its calibration and the follower does not, the follower is still published. Running the same run
  twice writes nothing, because then both are held. The second fact checks this boundary: `[ published CPL1; CPL2 ]`
  answers CPL2 alone at revision 2. This assertion goes beyond the brief's list, inside an authorized identity.
- **D4 — reuse.**
  - The one revision write is `Item.publishRevision` (step 235). A follower that is also an entry, or a parameter
    the item lacks, is refused there, and the refusal surfaces as `PublishItemErr`.
  - The ordered first-error collector is `ItemEvaluation.sequenced`, the shared one (ruling 016). No local
    collector was added.
- **D5 — no other type or member.** The logic lives in local lets inside `calibratedItemsWith`. Its tuples have at
  most three elements, each of a distinct type (`FitRunEntry * ItemId * ParameterName`, `ItemId * Parameter`,
  `Item * Parameter list`).
- **D6 — rendering.**
  - An item is named by its GUID, as `StudyParameterRef.describe` does.
  - A run and a study are named by their quoted names. The foreign text names both studies, so `'LR'` and
    `'QZ-LR'` are both found.
  - `PublishItemErr` renders the `ItemError` through its `describe`.
- **Test fixtures.**
  - CPL1 and CPL2 are built through `ItemDraft.toItem`, over the built-in circular-polarizer model, dial-mounted
    as in `StudyTests`, with `c2.retardance` unknown from 1.5 within [1, 2].
  - The Phone is built the same way over the intensity-detector model, fixed-mounted, with `gain` unknown from
    1000 within [100, 10000].
  - The GUIDs are `40600000-…-0000000000{11,12,13}`.
  - `qzLr` is a literal `Study` record whose one link makes CPL2's retardance follow CPL1's.
  - `calibrationRun` is a literal `FitRun` of QZ-LR named `FitRunName "QZ-LR@20261101T120000Z"`. It is a
    staged, stage-one, nonlinear run over E1, with two entries:
    - CPL1's retardance, `ItemOwner`, 1.5 → 1.55;
    - the gain, `SessionOwner (SessionName "Session 1")`, 1000 → 900.

    Its σs are `Some (ParameterSigma 0.002)` and `None`, and its other five result lists are empty.

## Testing state

Development runs on 2026-11-14. These are not gate verdicts; the supervisor runs the gates.

- **build** (`dotnet build Berreman.slnx -c Release -nologo -v:m`):
  - exit 0, 0 errors;
  - only the exempt `NU1902` advisories, plus vendored MathNet's `SYSLIB0051` on the first build, which has been in
    the arc's logs since step 0001;
  - 81 s first, 38 s final, of 600 s;
  - logs `0406-local-build.log` and `0406-local-build-final.log`.
- **constructor-unit-tests** (whole suite, the gate's command):
  - 1730/1730 passed, up by exactly 4 from step 0405's 1726;
  - 153 s of 180 s;
  - log `0406-local-constructor-unit-tests.log`.
  - The TRX shows the four new facts `Passed`:
    - `publishing a calibration writes each item-owned fitted value with its sigma and fit run into the item's next revision, and a linked follower takes its leader's`
    - `a session-owned value is no item calibration, and a run that calibrates nothing or only what is published is refused`
    - `a value without a sigma, a run of another study or an item the kit lacks publishes nothing`
    - `every publish error renders its condition and its payload`
- **elevation** (`Elevation/Run-ElevationGate.ps1 -ExcludeRules ELEV004`, no base, so all 13 projects):
  - `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)`, known findings 0;
  - 226 s of 600 s;
  - log `0406-local-elevation.log`.
- **invariant-s40-\*** (20): each ran its own gate command from `0406.gates` and printed
  `INVARIANT HOLDS ... found nothing`, exit 0. Log `0406-local-invariants.log`.
- **Mutation check** (in `0406-mutation/`):
  - Four mutants ran in one build:
    - followers dropped;
    - held states never dropped;
    - the foreign check disabled;
    - one `describe` text changed.
  - All four new facts failed against them (`mutant-run.log`).
  - The source was restored from the `.orig` copy, and `sha256sum -c` said OK. Both new files were touched before
    the final build, so no mutant code survives.

## Artifacts

All under `specs/0050/.artifacts/`:

- `0406-local-build.log` and `0406-local-build-final.log`;
- `0406-local-constructor-unit-tests.log`;
- `0406-local-elevation.log`;
- `0406-local-invariants.log`;
- `0406-mutation/`: the `.orig` backup, `orig.sha256` and `mutant-run.log`.

## Gotchas

- `calibratedItemsWith` answers only the items that change. A caller that records the kit replaces those items by
  id; recording is step 413's job.
- A link whose follower's item is not in `items` is skipped silently, as the brief says. Only the leader's own
  missing item is `UnknownPublishedItemErr`.
- The constructor suite now runs 153 s of its 180 s deadline, which is close. The four new facts take
  milliseconds; the critical path is still `StudyEngineTests`.
