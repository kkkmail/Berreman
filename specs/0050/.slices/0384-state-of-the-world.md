# Step 0384 — state of the world

## Where we are

Spec 0050, section S11.3 (engines, staged and joint fits, append-only fit runs). Step 383 gave the domain its
`FitRun` record; this step persists it as the kit's own append-only file `<study>@<stamp>.fitrun-json`, in
canonical JSON validated against its own schema, through the step 146 / step 359 storage stack. Later S11.3
steps write fit runs from the engines and read them back for the review and calibration flows.

## What's working

- Added `KitError.FitRunExistsErr`, rendered `the kit already holds that fit run: <reason>`.
- Added `schema/fitrun.schema.json` and `SchemaValidation.validateFitRunFile` (the fifteen `FitRun` fields under v1).
- Registered int-tag converters for `FitStage`, `CalibrationUse` and `StudyEngineKind` in `CanonicalJson.options`.
- Added `FitRun.fileText`, `KitFileText.parseFitRun`, append-only `KitProxy.writeFitRun` and per-study
  `KitProxy.readFitRuns` (stamp order, then name).
- Added `KitFitRunFileTests` and three fit-run facts in `KitContentFileJsonTests`, and widened the
  closed-set and kit-error facts.

## Tests

Run locally on 2026-11-13. Every run finished within its effective deadline:

- `build`: pass. 0 errors; the only warnings are the exempt NU1902 advisories. Took 45 s, then 24 s for the
  incremental rebuild (deadline 600 s).
- `elevation`: pass, `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)`. Took 224 s (deadline 600 s). This
  was a standalone run with no `-Base`, so all projects were analyzed.
- `constructor-unit-tests`: pass, 1662/1662. Took 73 s (deadline 180 s). The count is 1656 → 1662, so +6:
  checkpoint key 382 is step 383's 1656. All six new facts passed, and so did the three widened ones
  (`every closed set with an int tag is written as its tag and read back`, `an unknown disk tag is refused`,
  `every kit error renders its condition and its payload`).
- The twenty `invariant-s38-*` must-find-nothing greps: all OK. They were also re-run with `--untracked` so
  the two new files are covered.

```yaml
gates:
  constructor_unit_tests: 1662
```

## Architecture

- Append-only means `writeFitRun` checks `listKindFiles FitRunKitFile` before calling `writeKindFile`. No
  proxy verb was added. A stem already held, compared ignoring case, gives `FitRunExistsErr` naming the held
  file, and nothing is written. Through `readOnlyView`, a new name gives `KitReadOnlyErr` from
  `writeKindFile`; an existing name still gives `FitRunExistsErr`, because the existence check comes first.
  Either way nothing is written.
- `readFitRuns` reuses `readEveryKindFile`. A broken file, or a file whose run name is not its stem, fails
  the read for every study with `KitFileRejectedErr` naming the file. The study filter is applied after the
  whole kind has been read. Order is `compare` on `ClockTime`, then the name ordinal.
- The schema checks only the root shape. The inner shapes (`FitMemberOutcome`, `UnknownOwner`,
  `StudyParameterRef`, `ParameterState`) are bound by the serializer in its case form, and a malformed one
  is `MalformedJsonErr`.

## Deferred

Nothing.

## Gotchas

- The test fit run lives in `module FitRunFileFixtures` at the top of `KitContentFileJsonTests.fs`, which is
  the first file compiled among the fit-run facts. This follows the `StudyFileFixtures` precedent, so the
  fixture is shared rather than copied. `KitFitRunFileTests` opens it.
- `KitFitRunFileTests` has a private `folderBytes`, matching the four existing private copies
  (`KitStudyFileTests`, `KitItemFileTests`, `KitOpeningTests`, `KitWindowModelTests`). They cannot be merged
  into one helper in this step because those files are outside its authorized edits.
- `CanonicalJsonTests` names the fit-run types as `FitRuns.X` through its existing `open
  OpticalConstructor.Domain` instead of opening `FitRuns`. That keeps the new record labels from shadowing
  the file's record expressions.
- When writing fsproj paths from Python, `"schema\fitrun…"` turns `\f` into a form feed. The fsproj was fixed
  before any build.

## Changelog

- 2026-11-13 — Step 0384: fit runs persist as append-only `<study>@<stamp>.fitrun-json` kit files, with a
  schema, tag converters, `writeFitRun` / `readFitRuns`, and six new facts.
