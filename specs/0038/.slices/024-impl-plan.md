# 024 — IMPLEMENT — impl-plan

## Goal

Rename the OLD toggle-based `Groups.ExperimentCollection`
(`OpticalConstructor.Domain/Groups.fs:114-129`) to `WorkbenchToggleSet`.
Type rename only — the `GroupsLibrary` record shape and the persisted groups
JSON are unchanged (the F# type name never appears in the serialized JSON,
which keys off record field names). After the step, *experiment collection*
names the live `Experiments` concept exclusively
(`Domain/ElementId.fs:951`, its `commit` near line 1010).

## Approach

Single mechanical type rename plus its handful of references. The type is only
referenced from three F# sites (the definition, the `GroupsLibrary.collections`
field annotation, and one test constructor call), because Storage flows the
value through `Groups.GroupsLibrary` and never names the element type directly.

## Files to modify

1. `Berreman/OpticalConstructor/OpticalConstructor.Domain/Groups.fs`
   - `type ExperimentCollection` → `type WorkbenchToggleSet` (def + `create`
     return type + self-references).
   - `GroupsLibrary.collections : ExperimentCollection list`
     → `... : WorkbenchToggleSet list` (field NAME `collections` unchanged →
     JSON shape unchanged).
   - Update the doc comments that *name* the type "an experiment collection"
     so the phrase no longer denotes the toggle-set (section header + type doc +
     `create` doc + module header). Keep every `collections` field-referring
     word and all H.1 spec anchors.

2. `Berreman/OpticalConstructor/OpticalConstructor.Tests/GroupsRoundTripTests.fs`
   - `Groups.ExperimentCollection.create` → `Groups.WorkbenchToggleSet.create`
     (line 104).

## Explicitly NOT changed

- `Domain/ElementId.fs` `module Experiments` `ExperimentCollection` — the LIVE
  type, must remain.
- `Ui/TableAndElementRotationView.fs` (`Experiments.ExperimentCollection`) and
  `Tests/ExperimentProxyTests.fs` (opens `Experiments`) — LIVE references.
- `Controls/ExperimentControls.fs:210` `let collection = "ExperimentCollection"`
  — a stable UI automation-ID string for the LIVE experiments list, not a type
  reference; changing it would break an automation contract.
- `Storage/GroupsLibrary.fs` — references `Groups.GroupsLibrary` only; re-points
  mechanically, no edit.
- `Storage/schema/optical-constructor-groups.schema.json` — the persisted JSON
  shape (field `collections`) is unchanged; the schema's internal
  `$defs/experimentCollection` key is a schema-authoring detail, not an F# type
  and not part of the data instance, so it stays (round-trip preserved).

## Risks

- Missing an unqualified reference resolving through `open Groups`. Verified by
  grep: the only OLD-type code references are `Groups.fs` and the one qualified
  test call. All other `ExperimentCollection` matches are the LIVE Experiments
  type or the UI-id string.
- Keep LF line endings.

## Gates (run by the arc-runner engine after exit)

- `build` — `dotnet build Berreman.slnx -c Release`
- `unit-tests` — `dotnet test` against BerremanTests + constructor tests.
