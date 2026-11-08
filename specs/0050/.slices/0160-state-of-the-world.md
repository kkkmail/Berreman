# Where we are

Slice 0160 resumes at committed HEAD `7392e82`, after ASG 0090 manual 033
repaired the live-input guards. That repair makes the operator's mid-arc
`.specrc` backend switch valid. Since the operator-verified `37cb549`, the
product and the gate inputs are unchanged. This worker kept the complete
material-schema repair and codec retirement from
[manual 012](../.manual/012-step-0160-material-schema-recovery.md), and the
arc-origin elevation coverage from
[manual 013](../.manual/013-arc-origin-elevation-recovery.md). It refreshed the
handoff for normal supervisor review. Checkpoint 159 is not yet recorded.

# What's working

- Retire the `.ocproj` project codec, the project schema and the
  `OpticalConstructorProject` aggregate; the retirement search finds nothing.
- Validate material metadata against `schema/material-metadata.schema.json`
  through the shared `tryLoadSchema` / `validateWith` seams.
- Leave category membership to the caller's list, and keep every retained
  export/import assertion.
- Migrate the fresh-table fact to TableViewTests, retire the seven declared
  facts and add the structural-refusal fact.
- Keep elevation coverage from the immutable arc origin `f395597c`.

# Tests

As Invariant 6 requires, this IMPLEMENT worker ran no checks or gates:
`gates_run: []` and `gate_results: {}`. The only command run was a read-only
retirement `git grep`, which found nothing; it is not a gate verdict.

The operator's earlier
[verification summary](../.artifacts/0160-02-operator-repair/verification-summary.json)
records all fifteen frozen gates passing. That evidence is not supervisor
acceptance. It records:

- 1,136 constructor cases / 1,111 method identities, with exact census and zero
  failures or skips;
- twelve of twelve product projects analyzed, with zero new findings under the
  ELEV004 exclusion;
- no owned-code warnings.

Before step 0161, the supervisor must run build, elevation, the whole
constructor suite and the twelve s18 invariants, then publish checkpoint 159
with measured case evidence. Seven deletions, one unchanged migration and one
addition give `1142 - 7 + 1 = 1136`. The YAML keeps checkpoint 158's inherited
floor; it is not a new measurement.

```yaml
gates:
  constructor_unit_tests: 1142
```

# Architecture

Material metadata has its own schema file: draft 2020-12, `$id`
`https://berreman/schema/material-metadata.json`, root
`$ref` `#/$defs/materialEntry`. It holds `materialEntry`, `dispersionModel`,
`thermoOptic` and `unitOfMeasure`. The only change from the project schema is
the removal of the obsolete category `enum` (manual 012). The schema checks
structure. Import and export resolve category membership through the caller's
list.

Elevation reads the `.arc-base-json` origin through `ASG_ARC_BASE_COMMIT`
(manual 013). This round made no architectural or production change.

# Deferred

None of the slice's implementation is deferred. What remains is protocol owned
by the supervisor: normal review, all fifteen gates, and checkpoint 159 with
case evidence.

# Gotchas

- Keep `--executability-profile 1`. Do not use `--accept-current-slice`
  (manuals 012 and 013).
- `.specrc` selects `claude-code`, committed in `7392e82`. Under ASG 0090
  manual 033 this is a valid operator edit, not a worker defect.
- Operator commits never narrow elevation below the arc origin. Earlier
  evidence is not a new verdict.
- `.executability-json`, `.manifest.state.json`, `.supervisor-lock.diag` and the
  CRLF-only compiled slice files are supervisor-owned and were left alone.
- ASG and STL are read-only. This round made no git mutation.

# Changelog

- 2026-11-08: Operator material-schema recovery completed the declared retirement
  and test effects (`.artifacts/0160-operator-repair/`).
- 2026-11-08: Operator arc-origin recovery recorded twelve-project analysis and
  fifteen passing gates (`.artifacts/0160-02-operator-repair/`).
- 2026-11-08: After the ASG live-input repair, the resumed worker kept the
  committed repairs and the operator's backend selection. It refreshed the
  handoff with the inherited 1,142 floor for the supervisor to measure.
