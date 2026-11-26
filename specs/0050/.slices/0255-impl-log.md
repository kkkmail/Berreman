# Step 0255 — impl log

## Progress

- Read the task file, both system prompts, the project prompt, the slice and its `.gates` snapshot. No `.manual/`
  ruling names step 0255 (016 binds 0252, 0254 and 0259–0261; 014 binds 0185–0188 and 0260), so none applies.
- Before writing, checked the new type, case and member names for uniqueness across the repo and STL
  (ELEV014/016/019). None was found. Read STL's ELEV001 analyzer, which flags only `Ctor other.value` shapes.
- [x] `DialSign.physicalAngleOf` / `readingOf` (ItemModels.fs).
- [x] `MuellerReconstruction.physicalAngle` now calls `analyzer.dialSign.physicalAngleOf analyzer.zeroDial dial`.
  The signature is kept, and the doc names `DialSign.physicalAngleOf` as the rule.
- [x] `FileStem.firstFreeText` is declared directly before `ItemLabel.nextFreeAmong`, whose body now reads it
  (Kit.fs).
- [x] `SetupError`, `SetupName`, `SetupRevision` and `SetupReference` are appended at the end of Kit.fs, in that
  order.
- [x] Four `KitTests` facts and one `ItemModelsTests` fact: the census's five identities, names verbatim.
- First build: exit 0, 0 errors. The incremental rebuild: 0 warnings. The constructor suite: 1408 passed
  (1403 + 5). `MuellerReconstructionTests`: 29 passed.
- **First elevation run: FAILED, 1 new finding.** `ELEV010 Kit.fs:807:23 - firstFreeText mixes bare primitive(s)
  string with elevated types in one signature`. This is the slice's literal signature
  `(text : string) (taken : FileStem list) : string`. In STL's `NoPrimitiveBesideElevated.fs`, ELEV010 has no
  member exemption; its exemptions are the elevation-pattern members, and `ofText` is on that list, which is why
  `FileStem.ofText` passes.
- Fix: `taken : string list`, the taken names' texts, with their stems taken inside by `FileStem.ofText`. Callers
  pass `held.value`, which ELEV004 sanctions inside the name type's own member. `SetupName.firstFree` now reuses
  `firstFreeText` (a text is free exactly when the sequence keeps it unchanged) instead of a second stem test, and
  no longer reads `FileStem.value` outside `FileStem`. The ELEV004 count, excluded but measured, fell 1351 → 1349.
- After the fix, everything was re-run over the final tree: build, constructor suite, selected class, elevation
  and the invariants (see Testing state).

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/ItemModels.fs`: `DialSign.physicalAngleOf`, `readingOf`.
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/MuellerReconstruction.fs`: `physicalAngle` body and doc.
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/Kit.fs`:
  - `FileStem.firstFreeText`, and `ItemLabel.nextFreeAmong` over it;
  - the `ItemDial` doc now points at `DialSign.physicalAngleOf`;
  - the four setup types.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitTests.fs`: four facts appended.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/ItemModelsTests.fs`: one fact after the dial-sign fact, and
  `open OpticalConstructor.Domain`, so that `MuellerReconstruction.AnalyzerModel` and `.physicalAngle` resolve.

## Decisions

- `physicalAngleOf` keeps the exact floating-point expression the old `physicalAngle` held,
  `Angle (this.value * (reading.value - zero.value))`, rather than `Angle`'s `(-)` operator. So the reference
  pipeline's numbers are bit-identical, and the new fact asserts that `physicalAngle` equals it exactly.
- **Deviation from the slice:** `FileStem.firstFreeText (text : string) (taken : string list) : string` instead of
  `taken : FileStem list`. ELEV010 refuses the slice's signature, and the analyzers win (AGENTS.md; spec 0048
  `.manual/002` R6). The slice adds no other type that could elevate `text`, and editing the analyzers or
  `.elevation-analyzers.json` is forbidden. So the taken side gives up the stem type instead: one primitive-only
  helper on `FileStem`, which members may speak. Behaviour is identical, because each name's `fileStem` is
  `FileStem.ofText` of its text. The symbol `OpticalConstructor.Domain.Kit.FileStem.firstFreeText` exists as the
  contract registry names it.
- `SetupName.firstFree` reuses `firstFreeText`, so one case-insensitive stem test serves items, setups and
  `Setup n` (AGENTS.md "Reuse, never copy").
- The dial fact compares the 30° / −30° values and the round trips within 1e-12, the slice's tolerance. It uses
  xUnit's tolerance overload, as the neighbouring `ItemModelsTests` facts do. The `physicalAngle` equality is
  exact.

## Testing state

Development runs over the final tree used each gate's own command. No gate verdict is claimed (Invariant 6).

- **build:** exit 0, 0 warnings, 0 errors.
- **constructor-unit-tests:** `Passed!`, 1408 succeeded, 0 failed, 0 skipped, 67 s (deadline 180 s). The baseline
  was 1403, so the count rose by 5.
- **unit-tests (`BerremanTests.MuellerReconstructionTests`):** `Passed!`, 29 succeeded, 0 failed, 0 skipped
  (deadline 7200 s).
- **elevation:** `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, 196 s (deadline 600 s).
- **The fifteen s26 invariants:** each grep finds nothing.
- `git diff --numstat` matches `--ignore-cr-at-eol`, so there is no CRLF churn. Only the five authorized files
  changed under `Berreman/`.

## Artifacts

- `specs/0050/.artifacts/0255-dev-build.log`: the final build.
- `specs/0050/.artifacts/0255-dev-constructor-unit-tests.log`: the constructor suite.
- `specs/0050/.artifacts/0255-dev-unit-tests.log`: `BerremanTests.MuellerReconstructionTests`.
- `specs/0050/.artifacts/0255-dev-elevation.log`: the final ELEV gate run. The failing first run was overwritten;
  its finding is quoted above.

## Gotchas

- Steps 286 and 306 call `FileStem.firstFreeText this.value` "over the taken names' `fileStem`s". With this
  step's signature, they pass the taken names' texts instead (`taken |> List.map (fun held -> held.value)`).
- In the test, `Angle.degree -30.0<deg>` is written `Angle.degree (-30.0<deg>)`, so the minus cannot parse as a
  subtraction.
- `SetupReference` is now the last-declared record with a `name` field in `Kit`. Nothing later in the compile
  order builds an unannotated `{ name = …; … }` that would now resolve to it; the build is green.
