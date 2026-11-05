# Step 0016 — IMPLEMENT — impl log

## Progress

- [x] Read the protocol (the arc-runner base and the IMPLEMENT delta), the project prompt, the
      slice, its gates snapshot, step 15's state of the world and the `S13-CONSTRUCTOR-FIXTURES`
      oracle entry. The project prompt's Operator note section is empty.
- [x] Switched the nine test files to `PresetIds` with a count-asserting script,
      `.artifacts/0016-scripts/preset-ids.py`. Every pattern matched exactly the expected number
      of times.
- [x] First compile failed: `PolarizerBehaviorTests` does not open `Placement`, so the
      `ElementValueId` annotation on its two helpers was unresolved (FS0039). The two
      annotations now read `Placement.ElementValueId`. The second compile was clean.
- [x] Local construction checks (see Testing state).

## Files modified

Exactly the nine files in the slice's authorized effects.

- **`ExperimentCollectionProxyTests.fs:46-48`**: `BoundByEntryId PresetIds.source600` /
  `polarizerLp` / `detectorIntensity`.
- **`ExperimentCollectionRoundTripTests.fs:56-60`, `:76-79`**: the same members. Two sources,
  four polarizers, two detectors.
- **`ExperimentProxyTests.fs`** (`:38-39`, `:197-219`, `:238`, `:338`, `:365`): the same
  members. Three sources, six polarizers, four detectors.
- **`LibraryProxyTests.fs`** (`tryGetEntry hits a known id and misses an unknown id`):
  - it looks up `PresetIds.source600` and asserts `Assert.Equal(PresetIds.source600, s.id)`;
  - the unknown id "no-such-id" stays.
- **`PlacedSampleEmissionTests.fs`**:
  - `entry` and `bind` take an `ElementValueId`, so `bind` passes `BindValueId entryId`
    directly;
  - `mirrorId`, `thinFilmId` and `plateId` hold `(SampleItem …).entryId` itself;
  - `sourceId` is `PresetIds.source600`;
  - `entry`'s `failwith` renders `%s{id.value}`, so the message text is unchanged.
- **`PolarizerBehaviorTests.fs`**:
  - The expected protected ids are the five detector and polarizer members followed by the
    seeded sources' ids. All of them go through `.value`, then `List.sort`, as today.
  - `polarizer` and `extractedLabels` take a `Placement.ElementValueId` and compare ids
    (`p.id = id`, `e.entryId = id`).
  - Their nine callers pass `PresetIds.polarizerLp` / `polarizerCpLeft` / `polarizerCpRight`.
- **`SceneProxyTests.fs:32`**: `Some Library.PresetIds.source600`.
- **`SceneRoundTripTests.fs:32`, `:38`, `:105`**: `Some Library.PresetIds.source600` and
  `Some Library.PresetIds.polarizerLp`.
- **`SourceLibraryTests.fs`**:
  - `wavelengthOf` takes an `ElementValueId` and compares `s.id = id`;
  - its seven checks read `PresetIds.source600`, `heNe633`, `ndYag1064`, `mercuryI365`,
    `krf248`, `arf193` and `euv13p5`;
  - the live-Library detector is `PresetIds.detectorIntensity`, both in its `DetectorItem`
    record and in the `tryGetEntry` lookup.

## Decisions

- **The Scene files write `Library.PresetIds.<member>`.** `SceneProxyTests` and
  `SceneRoundTripTests` do not open `OpticalConstructor.Domain.Library`, and both already write
  `Library.ElementId`. They name the same members the slice lists, in the qualified form step 15
  used for its test files that do not open `Library`. Adding an `open` would have brought every
  `Library` name into scope over the files' `Scene` and `TableAndElementRotationView` opens.
- **`PolarizerBehaviorTests` writes `Placement.ElementValueId`.** The file does not open
  `Placement`. The qualified type annotation is the same form `ExperimentRunTests.fs:79` uses.
  It avoids a new `open` over the file's six existing ones.
- **The expected protected-ids list maps `.value` once.** The five members and the seeded
  sources' ids are concatenated as `ElementValueId`s, then mapped through `.value` and sorted.
  The result is the same `string list`, sorted the same way. The comparison against
  `presets |> List.map (fun e -> e.entryId.value) |> List.sort` is unchanged.
- **The test-local user sources stay as they are.** The slice lists "src-user-1", "src-a",
  "src-b", "src-c" and "src-user-krf" as ids that stay, and names no change to `userSource`.
  `userSource (id : string)` and its `ElementValueId "src-user-1"` lookups are therefore
  untouched.
- **No comment in the nine files held preset id text**, so no comment changed. The slice's
  "comments included" clause had nothing to act on.

## Testing state

**No gate was run.** Under Invariant 6 the gate engine is the only gate authority. To be sure
the construction is real code, the round compiled the tree and ran the affected suite locally.
These are observations, not gate results.

- **Compile.** `dotnet build Berreman.slnx -c Release -nologo -v:m` exited 0, with 0 errors and
  0 warnings. The log contains no "error". This was an incremental build: the vendored MathNet
  projects were up to date, so their two SYSLIB0051 warnings were not re-emitted.
  `OpticalConstructor.Tests.dll` was rebuilt. Log: `.artifacts/0016-build.log`.
- **constructor-unit-tests, whole suite**: `Test run summary: Passed!`, 736 total, 736
  succeeded, 0 failed, 0 skipped. That equals the last checkpoint (`constructor_unit_tests: 736`).
  Log: `.artifacts/0016-constructor-unit-tests-local.log`.
- **ELEV analyzers** (`Run-ElevationGate.ps1 -ExcludeRules ELEV004`): `ELEV-GATE: OK - no
  in-scope project was touched`. The test projects are outside the analyzers' scope, so the nine
  files are noted and not analyzed. Log: `.artifacts/0016-elevation-local.log`.

Read-only checks:

- **The slice's grep lists only the ids the slice says stay.**
  `git grep -n -E "(src|det|pol)-[a-z0-9]" -- Berreman/OpticalConstructor/OpticalConstructor.Tests`
  finds:
  - `SourceLibraryTests.fs:74/76/80/81/82/89/92/95/107`: the test-local user sources;
  - `ExperimentRunTests.fs:79`: "src-t";
  - `EnvironmentRoundTripTests.fs:64/106` and `SourceProjectionTests.fs:25`: the
    `SourceSpecId`s.
- **The five s03 invariants each find nothing** (exit 1): `elev-list-empty`, `no-vstest`,
  `no-kit-kit-json`, `no-eps-axis-evaluated` and `complexity-mandatory`.
- **The test count cannot have moved.** The diff adds and removes no `[<Fact>]`, `[<Theory>]`,
  `[<Trait>]` or `InlineData` line.
- **LF holds.** `git diff --numstat` equals `--numstat --ignore-cr-at-eol`, and none of the nine
  files contains a CR byte.

## Artifacts

- `.artifacts/0016-scripts/preset-ids.py`: the count-asserting replacement script.
- `.artifacts/0016-build.log`: the local compile.
- `.artifacts/0016-constructor-unit-tests-local.log`: the local constructor suite run.
- `.artifacts/0016-elevation-local.log`: the local ELEV analyzer run.
