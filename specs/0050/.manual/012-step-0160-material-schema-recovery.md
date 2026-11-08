# Step 0160: material schema extraction and codec retirement

The operator approved the four-step plan in
`.slices/0160-01.postmortem-review-md` on 2026-11-08, after clarifying that the
obsolete restriction is JSON Schema's `enum` keyword, not an F#/.NET enum.
Entry HEAD `8f2deca` contains the stopped attempt's reports; accepted 0159
(`8d82da6`) remains the implementation comparison base.

## Authorized reconciliation

Material category membership belongs to the caller's live category list.
Preserve the accepted 0046 behavior and every existing ExportImportTests
assertion. Extract the **effective** structural material schema into
`schema/material-metadata.schema.json`: move `materialEntry`, `dispersionModel`,
`thermoOptic` and `unitOfMeasure`, removing only the obsolete fixed-string
restriction from `materialEntry.properties.category`. Its string type and
every other definition constraint remain. Use the specified draft, `$id`, root
reference and `PreserveNewest` content item.

This supersedes 0160's literal exact-copy and `NotACategory` example instructions.
In the already-declared fact `a material entry outside the materialEntry
definition is SchemaValidationErr`, use a valid GUID/name and numeric category
`12`, requiring `SchemaValidationErr` with nonempty messages. Category IDs,
names and the other F# category types remain unchanged. Do not special-case a
fixture string or restore a fixed category vocabulary.

Use the existing lazy `tryLoadSchema` and `validateWith` seams in Report.fs.
Remove its in-memory schema adaptation and duplicate evaluation body; preserve
the DTO and caller-list import/export membership checks. Complete every other
0160 retirement, project-entry change and test effect: migrate the fresh-table
fact unchanged, delete exactly seven declared facts and add the one declared
structural-refusal fact. No new test identity or operator census row is needed.

In addition to the published edit list, authorize documentation-only retirement
cleanup in Storage `CanonicalJson.fs` and `EntryFileJson.fs`. Keep all other
source and assertions outside the declared retirement intact. No compatibility
path, schema migration or ASG runtime repair is authorized or required.

## Verification and handoff

Run the required whole-tree retirement search and all fifteen unchanged 0160
gates: build, elevation, the whole constructor suite and twelve s18 invariants.
Require exactly 1,136 constructor cases, the exact declared identity changes,
zero failures/skips, no owned-code warnings and zero new elevation findings.
Cover every affected product project and dependent; compare against accepted
0159 if an intervening commit hides product edits from working-tree selection.

Validate all 494 steps and refresh the profile-1 receipt for the project-prompt
pointer while preserving all 988 compiled slice/gate artifacts byte-for-byte.
Preserve the published spec, oracle, test effects, gate snapshots, supervisor
state/counters, expected-red records, checkpoints/case evidence, contract
registries, version files/lock and ASG/STL source.

Store evidence and the future operator reply in `.artifacts/0160-operator-repair/`.
Update the implementation log and state-of-world with measured results, retaining
the inherited YAML floor of 1,142. Complete verification and wait for the
operator's commit. Do not commit or restart as part of the repair.

## Explicit recovery exception

For **0160 only**, this overrides the standing committed-green acceptance rule.
After the operator commits the complete verified repair and requests relaunch,
resume normally with `--operator-reply <recovery-note> --executability-profile 1`,
without `--accept-current-slice`. Preserve the verified implementation and return
it for normal supervisor review and all fifteen gates. The supervisor must
publish checkpoint **159** and its measured case evidence before advancing.

Checkpoint 158 records 1,142 cases. Seven observed single-case deletions, one
unchanged migration and one addition yield 1,136. Step 0161 has no deletion
credit; accepting 0160 without measurement would leave its floor at 1,142.
Do not manufacture a checkpoint, reset budgets or waive the count gate.

## Later edits

The same structural-validation/caller-membership split remains binding at 0354,
the only later declared edit to Report.fs or ExportImportTests.fs. Preserve all
accepted assertions through those changes and later constructor observations.
Step 0267's historical serializer reference does not restore the retired codec.

## Measured implementation

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

Source review confirms only the category allowlist was removed from the moved
definitions; the DTO, membership checks, F# category types, six retained
ExportImportTests bodies and migrated table fact remain unchanged. The retirement
search is empty and the complete repair is ready for the operator's commit.
Evidence is under `.artifacts/0160-operator-repair/`. Checkpoint 159 and supervisor
acceptance remain pending the required normal resume.
