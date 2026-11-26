# Step 0019 — ADD_RED — impl log

## Progress

- Read the protocol (`arc-runner.system-md`, `add_red_worker.system-md`), the project prompt,
  `AGENTS.md`, the slice and its `.gates` snapshot, `.spec-md` §3.3 and 001 §B, and step 6's
  ADD_RED outputs for the shape.
- Confirmed the red reasons on the tree step 18 left:
  - `Library.PresetIds` (`OpticalConstructor.Domain/ElementId.fs:66-97` and on) writes the
    source, detector and polarizer presets as text ids (`src-600`, `det-intensity`, `pol-lp`, …);
  - `newSourceId` (`ElementId.fs:650`; the slice's `:594` has drifted) mints
    `ElementValueId $"src-%s{Guid.NewGuid().ToString()}"`;
  - a sample's entry id is already `ElementValueId (string guid)` (`SampleId.toElementValueId`,
    `ElementId.fs:43`), the canonical lowercase hyphenated form.
- Looked for a GUID-check to reuse: the Domain has none on `ElementValueId` (the `Guid.TryParse`
  sites in `ElementId.fs:1214`, `MaterialLibrary.fs:38` and `ConstructorUiIdOfDomain.fs` parse,
  they do not check canonical form), and ADD_RED forbids adding one.
- `SourceLibraryTests.fs`, after `the seeded tree lists every seeded source under Sources`:
  - a private `canonicalGuidOf : ElementValueId -> Result<System.Guid, string>`: it matches
    `System.Guid.TryParse` (no `.IsSome`), accepts only when `string guid` equals the id, and its
    `Error` names the id and why it fails;
  - `every seeded Library entry id is the canonical string of a GUID, and no two are equal`: every
    `seedEntries` id passes the helper (the message lists each refusal), and `List.countBy`
    finds no repeated id;
  - `newSourceId mints the canonical string of a fresh GUID`: two `newSourceId ()` calls each pass
    the helper and are not equal.
- `LightSourceEditorTests.fs`, `Add source opens the editor on a freshly minted source id`: the
  name, the setup, the `AddSource` update and `Assert.Equal(1, calls.Count)` stay.
  `Assert.StartsWith("source-add:src-", calls.[0])` became `Assert.StartsWith("source-add:", …)`,
  then the rest of the call is matched through `System.Guid.TryParse` and must equal the parsed
  GUID's `string`.
- No production file was touched.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Tests/SourceLibraryTests.fs` (+27 / −0)
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/LightSourceEditorTests.fs` (+7 / −1)

Both are LF: `git diff --numstat` equals `git diff --ignore-cr-at-eol --numstat`.

## Testing state

These runs are local observations made to record the red, as the ADD_RED obligation requires.
The arc-runner's gate engine runs and records the gates.

- `dotnet build Berreman.slnx -c Release -nologo -v:m` exited 0 with 0 errors. The only
  warnings are the two vendored `MathNetNumerics/Numerics/Exceptions.cs` SYSLIB0051 warnings,
  already on the start tree. After a doc-comment-only edit to `canonicalGuidOf`, the rebuild exited
  0 with 0 errors and 0 warnings (MathNet up to date), and the two red selections and ui-smoke
  below were re-run on that final tree with the same results. Log: `.artifacts/0019-build.log`.
- `constructor-unit-tests`, the gate's selection (`--filter-method
  "OpticalConstructor.Tests.SourceLibraryTests.*" --filter-method
  "OpticalConstructor.Tests.SourceLibraryTests"`): exit 2, `total: 8, failed: 2, succeeded: 6`.
  Log: `.artifacts/0019-red-sourcelibrarytests.log`. The actual failures:

  ```
  failed OpticalConstructor.Tests.SourceLibraryTests.newSourceId mints the canonical string of a fresh GUID (1ms)
    'src-188b70f9-0aff-4fcf-ac0a-0fa4b3986d72' is not the string of a GUID
      at ...SourceLibraryTests.newSourceId mints the canonical string of a fresh GUID() in ...\SourceLibraryTests.fs:93
  failed OpticalConstructor.Tests.SourceLibraryTests.every seeded Library entry id is the canonical string of a GUID, and no two are equal (9ms)
    33 of 44 seeded entry ids are not GUIDs: 'det-intensity' is not the string of a GUID; 'det-ellipsometer' ...;
    'pol-lp' ...; 'pol-cp-left' ...; 'pol-cp-right' ...; 'src-600' ...; 'src-hene-633' ...; ... 'src-euv-beamline' is not the string of a GUID
      at ...SourceLibraryTests.every seeded Library entry id is the canonical string of a GUID, and no two are equal() in ...\SourceLibraryTests.fs:83
  ```

  The 11 seeded samples pass; the 2 detectors, 3 polarizers and 28 sources fail. The six facts
  the class held before all pass.
- The whole constructor suite: `total: 738, failed: 2, succeeded: 736`; the two failures are the
  declared reds, and 736 equals the last checkpoint (`constructor_unit_tests: 736`, step 15).
  Log: `.artifacts/0019-constructor-unit-tests-whole.log`.
- `ui-tests`, the gate's selection (`--filter-not-trait "Category=ui-smoke" --filter-not-trait
  "Category=kit-acceptance"` plus the `LightSourceEditorTests` method filters): exit 2,
  `total: 7, failed: 1, succeeded: 6`. Log: `.artifacts/0019-red-lightsourceeditortests.log`.
  The actual failure:

  ```
  failed OpticalConstructor.Ui.Tests.LightSourceEditorTests.Add source opens the editor on a freshly minted source id (275ms)
    the minted source id 'src-9fe2360f-cbe0-4541-97af-4bb85f63cbb6' is not the string of a GUID
      at ...LightSourceEditorTests.Add source opens the editor on a freshly minted source id() in ...\LightSourceEditorTests.fs:155
  ```

  The six other facts of the class pass.
- The whole ui-tests suite (no class filter): `total: 560, failed: 1, succeeded: 559`; the one
  failure is the declared red. Log: `.artifacts/0019-ui-tests-whole.log`.
- `ui-smoke`: `Passed!`, `total: 208, succeeded: 208`, equal to the checkpoint. Log:
  `.artifacts/0019-ui-smoke.log`.
- `elevation`: exit 0, `ELEV-GATE: OK - no in-scope project was touched` (both files are in
  test projects, outside the ELEV scope). Log: `.artifacts/0019-elevation.log`.
- The five s03 invariants each ran their `git grep` and found nothing (exit 1):
  `s03-elev-list-empty`, `s03-no-vstest`, `s03-no-kit-kit-json`, `s03-no-eps-axis-evaluated`,
  `s03-complexity-mandatory`.

## Artifacts

All under `specs/0050/.artifacts/`:

- `0019-build.log`
- `0019-red-sourcelibrarytests.log`
- `0019-constructor-unit-tests-whole.log`
- `0019-red-lightsourceeditortests.log`
- `0019-ui-tests-whole.log`
- `0019-ui-smoke.log`
- `0019-elevation.log`

## Gotchas

- The canonical-GUID check is written twice, once per test project: a private helper in
  `SourceLibraryTests` (used by both new facts) and three inline lines in the one UI fact. The
  authorized effects allow only the two `.fs` files (no linked shared test source, no `.fsproj`
  edit), and the Domain has no such check to call.
- `System.Guid` is written qualified; neither file opens `System`, and opening it in these files
  could shadow names they already use.
- "Its first two assertions stay" (the UI fact) read as: everything before the `StartsWith`
  stays — the setup, the `AddSource` update and `Assert.Equal(1, calls.Count)`.
- The ui-tests selection runs 7 facts here, so its succeeded count (6) is not comparable with
  step 18's 36, which ran five classes.
