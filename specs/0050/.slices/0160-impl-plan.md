# Implementation plan — slice 0160

Retire the standalone project codec and aggregate under `S44-PROJECT-CODEC`.
Move the material metadata schema's reference closure into its own content file,
reuse `SchemaValidation.validateWith`, migrate the fresh-table fact unchanged,
and retire only the seven authorized obsolete facts while adding the one named
material-validation fact. Keep the DTO, import/export behavior, sidecar logic,
shared serialization and kit persistence intact.

The planned source changes are the Domain `Project.fs` deletion and project
entry; Storage `ProjectJson.fs` and project-schema deletions, `SchemaValidation.fs`,
`Report.fs`, the new `schema/material-metadata.schema.json`, `Sidecar.fs` and
project entries; and the authorized constructor-test files and project entries.
The prior History/ProjectFile retirements require no further changes.

## Blocking requirement conflict

Source inspection establishes that the literal implementation cannot preserve
the accepted caller-owned category behavior. Step 0160 requires moving the
`materialEntry` definition exactly, retaining its five-name category enum, and
validating it directly through `validateWith`. The retained fact
`OpticalConstructor.Tests.ExportImportTests.0035 category round-trips through export then import back to the matching CategoryId`
requires `Glass from the caller` to export, validate and re-import. It is not an
enum member. Step 0046 deliberately removed this enum from the runtime schema
after reproducing that failure and preserved the regression inside this fact.

Before source implementation, the spec/operator owner must resolve
`S44-PROJECT-CODEC` against `S17-CATEGORY-LIST`: authorize a standalone structural
schema without the fixed category enum, preserving caller-list membership
checks, and revise the new negative example accordingly; or explicitly change
the accepted caller-owned category contract and its regression obligations.
The worker cannot silently alter the published slice/oracle or weaken existing
assertions. No source edits will be made while these obligations conflict.

## Additional retirement scope

The required whole-tree absence obligation also reaches documentation in
`OpticalConstructor.Storage/CanonicalJson.fs:128` and `EntryFileJson.fs:87`.
Neither file is in the slice's explicit `effects.edits`. The owner should
authorize these two documentation-only edits with the category ruling. Their
implementation bodies need no change.

## Verification and handoff

The IMPLEMENT worker protocol reserves gate execution to the supervisor. No
build, tests, analyzers or acceptance/invariant commands will be run here.
Write the implementation log and state-of-the-world at the supplied paths,
recording the exact conflict and historical evidence. Inherit checkpoint 158's
1142 constructor cases without authoring a new baseline. The intended completed
slice would reduce that count by six; this blocked round changes no tests.
