# Where we are

The operator-approved 0160 repair is complete and locally verified under
[manual 012](../.manual/012-step-0160-material-schema-recovery.md). Awaiting the
operator's commit and a later explicit relaunch request. The initial worker's
schema conflict is resolved by that ruling; no implementation blocker remains.

# What's working

Implemented manual 012 and the complete 0160 retirement. The four schema
definitions moved into material-metadata.schema.json; their only semantic change
is removal of the obsolete JSON category allowlist. Category remains a string,
all other constraints remain, and the F# category types are byte-identical.
Report.fs now uses the existing lazy FileName.tryLoadSchema and validateWith
seams. Its DTO, conversion and import/export membership behavior are unchanged.

Deleted the old codec, project schema, Domain aggregate and obsolete round-trip
test file, with their project entries. Migrated the fresh-table fact byte-for-byte
to TableViewTests, retired exactly seven declared facts, and added the declared
numeric-category refusal with nonempty diagnostics. All six existing export/import
fact bodies and all nine retained placement fact bodies are unchanged. The required
retirement search is empty. The two additional storage files have documentation-only
changes; already-retired history/project-file code remains retired.

# Tests

All fifteen unchanged gates passed: build, elevation, the whole constructor suite
and twelve s18 invariants. The measured constructor report contains **1,136 passed
cases / 1111 method identities**, zero failures/skips, and an exact census
match. It includes all seven ExportImportTests facts and the migrated table fact.
The evaluator consumed checkpoint 158's 1,142 floor and the seven verified deletion
credits; no floor was edited. The new fact restores one case, giving the declared
net reduction of six.

The Release build has zero errors and no owned-code warnings. It reports the two
known SYSLIB0051 warnings in vendored MathNet Numerics/Exceptions.cs (lines 24 and
77); these are recorded in build-warnings.json, not suppressed. Elevation passes
all seven affected product projects and dependents with zero new findings under
the existing ELEV004 exclusion. Full-plan validation passes all 494 steps; the
refreshed profile-1 receipt validates and all 988 compiled files are byte-identical.

The YAML below retains checkpoint 158's inherited floor. The measured 1,136 is
reported above, not authored as a new checkpoint or baseline.

```yaml
gates:
  constructor_unit_tests: 1142
```

Evidence: [.artifacts/0160-operator-repair/](../.artifacts/0160-operator-repair/),
including verification-summary.json, gate-results.json, source-review.json,
contract-validation.json, test-inventory.json, 0160-constructor-unit-tests.trx,
the per-gate stdout/stderr logs, and final-scope-check.json. The future resume
note is operator-reply.md in that folder.

# Architecture

The standalone material metadata schema owns structural validation; the caller's
live category list owns membership. FileName.tryLoadSchema loads the new schema
once and validateWith supplies the shared diagnostic evaluation. No F# category
enum was introduced or replaced. No serializer, migration or compatibility path
was added. CanonicalJson and kit serializers retain their existing implementations.

# Deferred

No implementation or local verification is deferred. Operator commit and normal
supervisor review/acceptance remain, as explicitly required by manual 012.

# Gotchas

Published requirements, test effects, gate snapshots, supervisor state/counters,
expected-red records, checkpoints/case evidence, contract registries, versions
and the version-update lock remain unchanged. Git HEAD/index are unchanged;
no ASG/STL source was edited. The complete verified repair awaits the operator's
commit. The arc remains stopped and checkpoint 159 remains absent.

After the operator commits and explicitly requests relaunch, manual 012 requires
normal resume with `--operator-reply <absolute-recovery-note> --executability-profile 1`,
without `--accept-current-slice`. Normal supervisor review and all fifteen gates
must record checkpoint 159 and its measured case evidence before advancing.
Local verification does not publish supervisor acceptance. Do not reset budgets
or fabricate a checkpoint; step 0161 has no deletion credit for this retirement.

- Preserve caller-defined category success, typed membership refusals and all
  structural constraints through later changes, especially step 0354.
- The JSON Schema keyword `enum` was a fixed string allowlist. MaterialCategory
  remains a record using DU-wrapped ID/name; those domain types were untouched.
- The new negative fact keeps its declared identity and uses numeric category 12.
- The migrated fresh-table fact keeps its original body and assertions.
- All source and handoff edits use LF; no version bump was requested.

# Changelog

- 2026-11-08: Worker recorded the exact-copy/caller-category conflict and stopped
  before product implementation. The preliminary inherited-tree build passed.
- 2026-11-08 (approved operator recovery): Recorded manual 012, completed the
  effective schema extraction and full retirement, verified all fifteen gates and
  the exact 1,136-case census, and prepared the normal-resume handoff. Awaiting commit.
