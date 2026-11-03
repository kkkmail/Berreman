# 006 — The cascading duplicate names, renamed by hand (`005`)

**Why:** `004` — a duplicate-name message lists the name's other declarations, and the gate keys a recorded
finding on `rule | file | message`. So renaming one declaration of a name declared three or more times rewrites the
message of every higher one, and the bottom-up loop can never close it. The operator ruled (`005`): the names must be
different; rename them by hand and unblock the loop.

## What changed

- **63 names, 279 declarations renamed in one change**, across Domain, Storage, Controls, Ui, TestWindows and the
  two constructor test projects (122 `.fs` files). The lowest declaration of each name keeps it (R2). The others carry
  their surface, as STL's VNC viewer does (R2):
  - types `<Surface><Name>`: `CategoryControlsState`, `TableRotationModel`, `MaterialEditorExitPrompt`;
  - `view` → `<surface>View` (`categoryControlsView`); `init` / `update` / `empty` / `validate` → `<name><Surface>`
    (`initTableRotation`, `emptyCategoryControls`, `validateUserEnvironment`); `isDirty` → `is<Surface>Dirty`; other
    values `<surface><Name>` (`tableRotationCanvasWidth`);
  - union cases `<Surface><Case>` (`TableRotationRotCancel`, `MaterialCancelClicked`), with these by hand:
    `SetCategoryEditorName`, `SetLightSourceName`, `SetMaterialName`, `SetSampleSubstrate`, `SetStackSubstrate`,
    `ChartFontTarget.LegendFont`, `BeamBranch.ReflectedBeam` / `TransmittedBeam` and
    `RayGroupKind.ReflectedRayGroup` / `TransmittedRayGroup` (the engine's `Fields.RT`, the lower assembly, keeps
    `Reflected` / `Transmitted`; `RayModel` already had `ReflectedBranch`);
  - domain values `allSeedSamples`, `allSeedSources`, `allBayNames`, `vacuumCategory`, `vacuumMaterial`,
    `createInMemoryExperiments`, `createInMemoryExperimentExport`, `groupsLibrarySchemaVersion`,
    `userEnvironmentSchemaVersion`, `defaultChartFontState`, `defaultChartStyleState`.
- **Method:** each declaration and every `Module.name` / alias-qualified use was renamed mechanically. Every bare use
  was renamed only where the compiler reported it, so a local or a field that merely shares a name is untouched. Every
  `FS0049` (a renamed case still written in a pattern, which compiles as a catch-all) was fixed, and the final
  `--no-incremental` build has none. No string literal changed (automation ids byte for byte); every source change is
  a one-for-one line rename. The tools are in `specs/0048/.artifacts/dupnames/`.
- **The stored format did not change.** `BeamBranch` is persisted by case name (the project schema's
  `beamBranch` enum, and the report). `ProjectJson.BeamBranchConverter` keeps it on the wire as `"Reflected"` /
  `"Transmitted"`; the five project round-trip tests prove it. No line of `stored-format-changes.md`.
- **No public rename in a package** (Core, OpticalModel, OpticalProperties and Analytics are untouched), so no
  `renames.tsv` row.

## The known-findings list

3,172 → 2,892 enforced: the 280 entries of the 63 names deleted. 407 other entries keep their rule, file and count
and quote the new name. These are the same findings (for example `ELEV008 … State.enabled` →
`ExperimentControlsState.enabled`), matched one for one by applying the rename map to the recorded text. The gate
found exactly 3,172 − 280. No entry was added. What is left of ELEV014 / 015 / 016 (12 / 46 / 56) is names declared
exactly twice, none with a second entry, so renaming either side clears the pair without rewriting another message.

## Gates

- `build`: 0 errors, no warning from our code (only the vendored MathNet `SYSLIB0051`).
- `elevation-full`: OK, 0 new, 0 stale, 2,892 known.
- `constructor-unit-tests` 727/727, `ui-smoke` 208/208, `ui-tests` 559/559. The counts are unchanged.
- `unit-tests` not run: no `BerremanTests` file and no solver project changed.
