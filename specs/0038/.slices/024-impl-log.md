# 024 — IMPLEMENT — impl-log

## Progress

- [x] Rename `Groups.ExperimentCollection` → `WorkbenchToggleSet` in `Groups.fs`
      (type def + `create` return type + doc comments + section header).
- [x] Re-point `GroupsLibrary.collections` field annotation to
      `WorkbenchToggleSet list` (field NAME `collections` unchanged → JSON shape
      unchanged).
- [x] Update `GroupsRoundTripTests.fs` constructor call
      `Groups.ExperimentCollection.create` → `Groups.WorkbenchToggleSet.create`
      (+ its descriptive comment).
- [x] Final grep: no `Groups.ExperimentCollection` / bare OLD reference remains;
      LIVE `Experiments.ExperimentCollection` untouched.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/Groups.fs`
  - `type ExperimentCollection` → `type WorkbenchToggleSet` (line 117).
  - `static member create … : ExperimentCollection` → `: WorkbenchToggleSet`
    (line 131).
  - `GroupsLibrary.collections : ExperimentCollection list`
    → `: WorkbenchToggleSet list` (line 150).
  - Doc comments/section header that *named* the type "an experiment collection"
    rephrased to "workbench toggle set" (module header line 3, section header
    line 95, type doc lines 111-116, `create` doc line 130) so the phrase
    *experiment collection* denotes the live `Experiments` concept exclusively.
    All `collections` field references and H.1 spec anchors kept.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/GroupsRoundTripTests.fs`
  - `Groups.ExperimentCollection.create` → `Groups.WorkbenchToggleSet.create`
    (line 104) and the library-fixture doc comment (line 94).

## Not modified (deliberate)

- `Domain/ElementId.fs` `module Experiments` `ExperimentCollection` — the LIVE
  type. Untouched (the concept the rename frees the name for).
- `Ui/TableAndElementRotationView.fs` (`Experiments.ExperimentCollection`) and
  `Tests/ExperimentProxyTests.fs` (`open …Experiments`) — LIVE references.
- `Controls/ExperimentControls.fs:210` `let collection = "ExperimentCollection"`
  — a stable UI automation-ID string for the LIVE experiments list, not a type
  reference; changing it would break an automation contract.
- `Storage/GroupsLibrary.fs` — names `Groups.GroupsLibrary` only, never the
  element type; re-points mechanically, no edit needed.
- `Storage/schema/optical-constructor-groups.schema.json` — the persisted JSON
  data shape (field `collections`) is unchanged; the schema's internal
  `$defs/experimentCollection` key is a schema-authoring detail (not an F# type,
  not part of the data instance), so it stays and the round-trip is preserved.

## Testing state

Gate execution belongs to the arc-runner's deterministic gate engine
(IMPLEMENT Invariant 6 — the worker acts and runs no checks); the worker does
not run, self-report, or green-light gates. This is a type-name rename with no
change to any test's assertions or count, so the prior green baselines carry
forward unchanged (`berreman_unit_tests: 119`, `constructor_unit_tests: 589`,
`ui_smoke_tests: 158`, `ui_tests: 425`).

Static verification performed (reads/grep, not gate runs):
- Full-tree grep confirms every surviving `ExperimentCollection` is the LIVE
  `Experiments` type or the UI automation-ID string; the OLD type name no longer
  exists anywhere.
- Rename is internally consistent in `Groups.fs` (def, `create`, field
  annotation all `WorkbenchToggleSet`).
- `git diff --numstat` == `--numstat --ignore-cr-at-eol` (16/13 and 2/2), so no
  CRLF churn introduced; git normalizes the working-tree CRLF to LF on commit
  via `.gitattributes`.

## Artifacts

None — pure type rename, no captured logs/traces produced.
