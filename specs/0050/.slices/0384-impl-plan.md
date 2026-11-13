# Step 0384 — impl plan

A fit run (`FitRun`, step 383) persists as its own append-only kit file `<study>@<stamp>.fitrun-json`
(spec 0050 S11.3; 001 §A, §E, §P). The step reuses step 146's canonical JSON stack and step 359's study
file shape: no second parser or writer, no new proxy verb.

## Approach

1. `Kit.fs` — `KitError` gains `FitRunExistsErr of reason : string`, rendered
   `the kit already holds that fit run: <reason>`. Only `describe` matches `KitError` exhaustively.
2. `schema/fitrun.schema.json` — draft 2020-12, `$id` `https://berreman/schema/fitrun.json`, shaped as
   `study.schema.json`: `schemaVersion` checked unconditionally (`library-common` `positiveVersion`); under
   `if schemaVersion = 1` the root holds exactly the fifteen `FitRun` fields (`name` / `study` `\S`,
   `stamp` string, `mode` 1–2, `stage` 1–3, `calibrationUse` 1–2, `engine` 1–2, `runs` non-empty strings,
   five arrays, `notes` string). Copied `PreserveNewest` beside `study.schema.json` in the Storage fsproj.
3. `SchemaValidation.fs` — `validateFitRunFile` over `validateWith`, its schema behind a `lazy` loaded
   through `tryLoadLibrarySchema` (which loads the common schema first), as `validateStudyFile`.
4. `CanonicalJson.fs` — three `ClosedSetTagConverter`s (`FitStage`, `CalibrationUse`, `StudyEngineKind`)
   registered beside `ParameterScope` / `FitMode`; the doc lists them.
5. `KitContentFiles.fs` — `FitRun.fileText`, `KitFileText.parseFitRun`, `KitProxy.writeFitRun` (append-only:
   `listKindFiles` holding the stem ignoring case → `FitRunExistsErr` naming the file, nothing written; else
   `writeRenderedKindFile`) and `KitProxy.readFitRuns (study)` (through `readEveryKindFile`, a stem mismatch
   is `KitFileRejectedErr`; filtered by study, sorted by stamp then name ordinal).
6. Tests — a shared `FitRunFileFixtures` module (the `StudyFileFixtures` precedent: it lives in the first
   file compiled among its facts, `KitContentFileJsonTests.fs`) holding the slice's test-local run;
   three facts in `KitContentFileJsonTests`; new `KitFitRunFileTests.fs` (three facts, appended last in the
   compile list) over `createContentTestKit` (step 359's fixture, folder
   `AppContext.BaseDirectory/KitFitRunFileTests/<guid>`); widened `CanonicalJsonTests` (two facts) and
   `KitTests` (one fact).

## Risks

- The serializer's case form for `FitMemberOutcome` / `UnknownOwner` / `StudyParameterRef` /
  `ParameterState` must round-trip (FSharp.SystemTextJson handles these as it does in the study / item files).
- `ClockTime` round-trips as a `DateTime` with `Kind=Utc` ("Z" suffix) — equality of `DateTime` ignores Kind.
- A FitRun test-local run must not use NaN (the serializer would refuse it).
