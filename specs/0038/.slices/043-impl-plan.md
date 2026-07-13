# 043 — IMPLEMENT: collapse the two recent-files stores into one

## Goal

Make `Storage.RecentFiles` the single recent-files mechanism by removing the
duplicate `EnvironmentSettings.recentFiles` field (Ui/UserEnvironment.fs) and its
default. Old `environment.json` files that still carry a `recentFiles` array must
keep loading (the field is ignored, no migration).

## Approach

1. **`OpticalConstructor.Ui/UserEnvironment.fs`**
   - Remove the `recentFiles : string list` field from `EnvironmentSettings` (~:191).
   - Remove `recentFiles = []` from `defaults` (~:247).
   - Trim the two doc comments that list `recentFiles` as a persisted field of this
     record (module header + the `EnvironmentSettings` type doc), pointing recent
     files at `Storage.RecentFiles` for accuracy.

2. **`OpticalConstructor.Ui/optical-constructor-environment.schema.json`**
   - Remove `"recentFiles"` from `required`.
   - Remove the `recentFiles` property block.
   - The root object has no `additionalProperties: false`, so an OLD file that still
     carries `recentFiles` continues to VALIDATE (the extra property is allowed) and
     `System.Text.Json` (default `Skip` unmapped-member handling in
     `ProjectJson.options`) IGNORES it on bind — the total-load contract is preserved
     with no migration code.

3. **`OpticalConstructor.Tests/EnvironmentRoundTripTests.fs`**
   - Drop `recentFiles = [...]` from the `sample` record and its round-trip assertion.
   - Drop `Assert.Empty defaults.recentFiles`.
   - Convert the `recent files round-trip …` test to a last-folders-only test.
   - ADD a regression test proving an old `environment.json` that still carries a
     `recentFiles` array loads to a valid `EnvironmentSettings` (dropped field
     ignored, no migration) — directly exercising the acceptance's total-load
     contract.

## Consumers

Grep confirms no production code READS `EnvironmentSettings.recentFiles` (the
step-2 Elmish shell that consumed it is already retired) — the only hits are the
field declaration, its default, the schema, and the tests. So there is nothing to
"re-point"; `Storage.RecentFiles` already stands alone (`HistoryTests.fs` exercises
`RecentFiles.bump`).

## Risks

- Schema over-tightening: must NOT add `additionalProperties: false` at the root, or
  old files carrying `recentFiles` would fail validation and lose the total-load
  contract. Leave the root permissive.
- Line-ending churn (LF). Verify no CRLF introduced.

## Gates (run by the arc-runner after exit — not by me)

- `build` — `dotnet build Berreman.slnx -c Release`
- `unit-tests` — `dotnet test` against `BerremanTests`
