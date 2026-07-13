# 043 — IMPLEMENT impl-log

## Progress

- [x] Read system/project/slice prompts and the target files.
- [x] Remove `EnvironmentSettings.recentFiles` field + default (UserEnvironment.fs).
- [x] Remove `recentFiles` from the env schema (required + property).
- [x] Update tests (drop the field's assertions; add an old-file-load regression).

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Ui/UserEnvironment.fs` — dropped
  the `recentFiles` field and its default; trimmed the two doc comments that listed
  it as a persisted field.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/optical-constructor-environment.schema.json`
  — removed `recentFiles` from `required` and from `properties`. Root stays
  permissive so old files carrying `recentFiles` still validate.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/EnvironmentRoundTripTests.fs`
  — removed the `recentFiles` uses; converted the recent-files round-trip test to a
  last-folders test; added a regression that an old file with `recentFiles` still
  loads (dropped field ignored, no migration).

## Decisions

- Kept the schema root PERMISSIVE (no `additionalProperties: false`) so old
  `environment.json` files that still carry `recentFiles` validate; STJ's default
  `Skip` unmapped-member handling then ignores the field on bind — the total-load
  contract holds with zero migration code.
- No production consumer reads `EnvironmentSettings.recentFiles` (the step-2 shell is
  retired), so there was nothing to re-point onto `Storage.RecentFiles`; it already
  stands as the sole store.

## Testing state

- Gates (`build`, `unit-tests`) are run by the arc-runner's gate engine AFTER I exit;
  I do not run or self-report them (Invariant 6).

## Artifacts

- None beyond the standard round outputs.
