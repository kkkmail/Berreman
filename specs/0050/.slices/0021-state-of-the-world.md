# Step 0021 — IMPLEMENT — state of the world

## Where we are

Spec 0050, section S1.4: "Library entry files: envelope, provenance, canonical JSON, content hash,
schemas". This step is the section's first. It moves the app's one serializer options value out of
`ProjectJson` into a new `CanonicalJson` module (001 §B, `.spec-md` §5 "Canonical JSON + SHA-256"),
changing no output. Every JSON store now reads `CanonicalJson.options`: the project file, the groups
library, the recent list, the material library and the user environment. Step 22 adds
`CanonicalJsonTests` (sorted keys, invariant-culture round-trip numbers) on this seam.

## What's working

- Moved `ProjectJson.options` and its five converters verbatim into the new `CanonicalJson` module
- Re-pointed every JSON store to `CanonicalJson.options`: the groups library, recent files, the material library, the user environment and the tests
- Turned `serializeProject` into an `OpticalConstructorProject` extension member, which ELEV013 requires once the options left its module
- Re-pointed every comment that named the old options value or its converters; `git grep "ProjectJson.options"` finds nothing under `Berreman/`

## Tests

The arc-runner's gate engine is the gate authority. These are local observations over the final
tree:

- `build`: exit 0, 0 errors, 0 warnings. The first build of the round showed only the two
  pre-existing vendored MathNet SYSLIB0051 warnings.
- `constructor-unit-tests` (whole suite): Passed!, 739 of 739. Every round-trip class the slice
  names passes, and its fact count is unchanged:
  - ProjectJsonRoundtripTests 7
  - RoundTripTests 3
  - GroupsRoundTripTests 9
  - EnvironmentRoundTripTests 33
  - ExportImportTests 7
  - DispersionModelsTests 31
  - PlacementTests 10
- `ui-smoke`: Passed!, 208 of 208.
- `ui-tests` (the 3 selected classes): Passed!, 26 of 26 (AppShellSeamTests 3,
  DataFilePickerFolderTests 7, LocalizationTests 16).
- `elevation`: `ELEV-GATE: OK - 0 new finding(s) in 8 project(s)`. The first run found one new
  ELEV013 on `serializeProject`; the member repair cleared it.
- The six s04 invariants each find nothing, and so does the acceptance grep
  `git grep -n -F "ProjectJson.options" -- Berreman/`.

```yaml
gates:
  constructor_unit_tests: 739
  ui_smoke_tests:         208
  ui_tests:                26
```

## Architecture

- **`OpticalConstructor.Storage/CanonicalJson.fs`** (`module CanonicalJson`) compiles right after
  `SchemaValidation.fs` and before `ProjectJson.fs`.
  - It owns `options : JsonSerializerOptions` and the five private wire converters (BeamBranch,
    RotationLock, BeamPresence, ToggleState and the Math.NET complex matrix).
  - The converter registration order and the FSharp.SystemTextJson option chain are unchanged,
    so every stored byte is unchanged.
- **`ProjectJson`** now holds only the project envelope:
  - `schemaVersion`;
  - `ProjectJsonText.parseProject`;
  - the new extension `OpticalConstructorProject.serializeProject`, which replaces the free
    `ProjectJson.serializeProject`. A caller writes `project.serializeProject` with
    `open OpticalConstructor.Storage.ProjectJson`.
- There is no second options value. `JsonSerializerOptions` is constructed only in
  `CanonicalJson.fs`.

## Deferred

Nothing in this step's scope.

## Gotchas

- **ELEV013 bites a free function that loses its module state.** STL's `NoSingleParameterFunction`
  exempts a one-domain-parameter free function only while it reads a module-level value of its own
  module.
  - `serializeProject` read `ProjectJson.options` and so was exempt. The move ended that, so it
    became an extension member.
  - That changed three callers outside the slice's file list, in call form only:
    - `ProjectFile.fs`;
    - `PlacementTests.fs`;
    - `ProjectJsonRoundtripTests.fs`.
  - The deviation is recorded in the impl-log.
- **The fsproj comment was re-pointed.** It is the existing GroupsLibrary comment at
  `OpticalConstructor.Storage.fsproj:26-27`, and the acceptance grep requires it. No fsproj comment
  was added.
- **`git grep` skips untracked files.** Until the step is committed, a working-tree grep does not
  see `CanonicalJson.fs`. A plain `grep` over it finds no `ProjectJson.options` either.

## Changelog

- 2026-11-05 — Step 0021: moved the shared serializer options and their converters from
  `ProjectJson` to the new `CanonicalJson` module. All five JSON stores re-point to it, and
  `serializeProject` became an `OpticalConstructorProject` member (ELEV013). No output changed.
