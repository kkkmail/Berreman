# State of the world — Step 024 (Spec 0038 Part H, IMPLEMENT)

## Where we are

Step 024 removes a name collision ahead of the experiment-descriptor work later
in Part H. Two unrelated types were both called `ExperimentCollection`: the OLD
toggle-based one in `Domain/Groups.fs` (a named set of experiment/element/group
on-off toggles persisted inside the separate groups JSON, Spec 0026 Part H) and
the LIVE one in `Domain/ElementId.fs`'s `Experiments` module (the editable
choose→commit collection of experiments, spec 028). This step renames the OLD
type to `WorkbenchToggleSet` — a type rename only — so *experiment collection*
names the live constructor concept exclusively (its `commit`,
`ElementId.fs:1010`). No behaviour, record shape, or on-disk JSON changes.

## What's working

- Rename `Groups.ExperimentCollection` to `WorkbenchToggleSet` (type definition,
  its `create` factory, and the `GroupsLibrary.collections` field annotation).
- Keep the `GroupsLibrary` record and the persisted groups JSON shape unchanged —
  the field name `collections` is untouched and the F# type name never appears in
  the serialized JSON, so `WorkbenchToggleSet` round-trips the same groups file.
- Re-point the one external reference — the `GroupsRoundTripTests` library
  fixture now builds via `Groups.WorkbenchToggleSet.create`.
- Leave the LIVE `Experiments.ExperimentCollection` and every reference to it
  (Ui view, proxy tests, the `"ExperimentCollection"` automation-id string) as
  the sole remaining `ExperimentCollection` in the tree.
- Storage load/save re-points mechanically: it names only `Groups.GroupsLibrary`,
  never the renamed element type, so no Storage edit was required.

## Tests

Gate execution is the arc-runner's (IMPLEMENT Invariant 6 — the worker acts and
runs no checks; it does not run, self-report, or green-light a gate). This is a
type-name rename that alters no test assertion and adds/removes no test, so the
prior green baselines carry forward unchanged and no `count_at_least` gate can
regress. Static verification only (grep/read, not gate runs): the OLD type name
no longer exists anywhere; every surviving `ExperimentCollection` is the LIVE
`Experiments` type or the automation-id string; `git diff --numstat` is identical
with and without `--ignore-cr-at-eol` (no CRLF churn introduced).

```yaml
gates:
  berreman_unit_tests: 119
  constructor_unit_tests: 589
  ui_smoke_tests: 158
  ui_tests: 425
```

## Architecture

- **A pure type rename, not a re-model.** `WorkbenchToggleSet` keeps the exact
  record shape (`name`, `sampleValueIds`, `experiments`, `elements`, `groups`)
  and the same `Toggle`-list fields; only the identifier changed. This is the
  minimal move that resolves the collision without touching serialization,
  storage, or UI wiring.
- **The JSON contract is field-name-keyed, so the rename is invisible on disk.**
  `System.Text.Json` serializes `GroupsLibrary` by its record field names
  (`groups`, `collections`), never by the element type's name, so renaming the
  element type cannot change the persisted bytes — the round-trip test and the
  schema (which validates the `collections` array shape) are unaffected.
- **Storage stayed untouched by construction.** `Storage/GroupsLibrary.fs`
  serializes/deserializes `Groups.GroupsLibrary` as a whole and never names the
  collection element type, so the "re-point" the slice describes is automatic —
  the serializer follows the record structure regardless of the element type
  name.

## Deferred

- Renaming the schema's internal `$defs/experimentCollection` definition key /
  its human descriptions in `optical-constructor-groups.schema.json`. It is a
  schema-authoring detail, not an F# type and not part of the serialized data
  instance, and changing it is unnecessary for the round-trip. Left as-is to
  hold the "JSON shape unchanged" guarantee with zero risk; a later cosmetic
  pass can align the schema's internal naming if desired.
- The `collections` field NAME on `GroupsLibrary` (kept for JSON stability); any
  future field rename would be a separate, breaking storage change out of this
  slice's "type rename only" scope.

## Gotchas

- **The task file's system-prompt path was stale.** It points at
  `C:\GitHub\AI-Strategy-Generator\implement_worker.system-md`; the file actually
  lives under `src/ai_strategy_generator/multistep/implement_worker.system-md`
  (a Development-family delta over the shared `arc-runner.system-md`). Both were
  located by search and read in full (recurring stale-path gotcha, cf. steps
  013/015/023).
- **`Controls/ExperimentControls.fs:210` `let collection = "ExperimentCollection"`
  is NOT a type reference** — it is the stable UI automation-id string for the
  LIVE experiments list control. It was deliberately left unchanged; renaming it
  would break an automation contract and it names the live concept, not the OLD
  toggle-set type.
- **The working tree is CRLF everywhere on this machine** (every `.fs` checks out
  with CRLF, e.g. `ElementId.fs` 1107/1107), but `.gitattributes` normalizes to
  LF on commit. Confirmed the edits add no CR-only churn: `git diff --numstat`
  and `--numstat --ignore-cr-at-eol` are byte-identical (Groups.fs 16/13,
  GroupsRoundTripTests.fs 2/2). Do not "fix" the working-tree CRLF — it is the
  ambient checkout, not this slice's doing.
- **Remaining "collection" wording in `Groups.fs` is field-oriented, not the
  reserved phrase.** The `Toggle`/`setToggle` docs and the `collections` field
  still say "collection(s)" because they reference the on-disk `collections`
  array; the reserved phrase *experiment collection* was removed from the OLD
  type's own docs, which is what the slice asked for.

## Changelog

- 2026-07-11 — Step 024 (Part H, IMPLEMENT): rename the OLD toggle-based
  `Groups.ExperimentCollection` to `WorkbenchToggleSet` (type rename only —
  `GroupsLibrary` record and groups JSON shape unchanged, Storage re-points
  mechanically), so *experiment collection* now names the live `Experiments`
  concept exclusively. Updated the one test reference; the LIVE type and its UI
  automation-id string are untouched. No test-count change; baselines carry
  forward (unit 119, constructor 589, ui-smoke 158, ui-tests 425).
