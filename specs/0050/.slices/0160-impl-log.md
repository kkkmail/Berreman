# Implementation log — slice 0160

## Progress

- Read the task, shared worker protocol, IMPLEMENT delta, project prompt,
  AGENTS.md, naming conventions, slice and resolved gate snapshot.
- Inspected the retirement census, material loader and validator, original
  schema, affected tests, prior slice handoff and immutable acceptance oracle.
- Before source implementation: traced the material schema extraction against
  the existing ExportImportTests assertions. Found a direct conflict between
  exact schema preservation and the retained renamed-category regression.
- Consulted step 0046's implementation log, state-of-the-world, accepted code
  review and recorded failing test output. These establish that the current
  enum removal intentionally repairs `S17-CATEGORY-LIST`; it is not an accidental
  discrepancy or an unmeasured concern about a future gate.
- Wrote the implementation plan and this log. Stopped before product/test edits
  because satisfying either conflicting requirement literally violates the
  other. No partial retirement or intentionally failing implementation was left.
- Wrote the state-of-the-world at the exact supplied path, with all required
  sections and the inherited checkpoint count. The handoff records the authority
  conflict; implementation and supervisor verification remain incomplete.

## Files modified

- Created `specs/0050/.slices/0160-impl-plan.md`.
- Created `specs/0050/.slices/0160-impl-log.md`.
- Created `specs/0050/.slices/0160-state-of-the-world.md`.
- No production, test, schema or project file was changed.

## Testing state

Blocked on a requirement decision, not on a newly measured gate failure.
No checks or gates ran under the IMPLEMENT worker's Invariant 6. No gate outcome
is asserted. `commit_ready` is false because the slice is not implemented.
The supervisor's roster remains build, elevation, all-discovered
constructor-unit-tests and the twelve s18 invariants.

Checkpoint 158 records 1142 constructor cases. The requested completed slice
migrates one fact, deletes seven and creates one, for a net reduction of six
(1136 expected cases). This round changes no tests and claims no measured count.
No census, baseline, oracle, checkpoint or gate was edited.

## Blocking requirement and missing authority

Owner: spec 0050 operator/spec owner. Step: 0160.
Obligations: `S44-PROJECT-CODEC` and accepted `S17-CATEGORY-LIST` from step 0046.

1. `0160.slice-md:23` requires the four definitions to move **exactly as the
   project schema holds them**, a lazy `tryLoadSchema`, and
   `validateEntryElement = validateWith materialEntrySchema element`.
   `schema/optical-constructor-project.schema.json:84-87` defines category as
   one of Glass, Metal, Semiconductor, Crystal or Vacuum. The new fact must
   reject the otherwise valid category `NotACategory`.
2. `0160.slice-md:25` requires every other ExportImportTests fact to pass.
   The existing accepted fact
   `OpticalConstructor.Tests.ExportImportTests.0035 category round-trips through export then import back to the matching CategoryId`
   (`ExportImportTests.fs:112`, renamed-category assertions at lines 146-179)
   supplies `Glass from the caller`, requires successful export, checks that
   exact persisted name, calls `validateEntryElement` successfully, and requires
   import to preserve the id, CategoryId and description.
3. `Report.fs:59` removes only the inherited category enum in memory. Export
   checks membership against the caller's categories before calling
   `validateEntryElement` (`Report.fs:142-158`). Replacing this loader with the
   required unchanged file restores precisely the rejection that step 0046
   repaired; both the export and direct validation cannot accept the retained
   fixture under the specified enum.

Required decision: authorize preserving caller-owned categories by moving the
effective structural definition without its enum and changing the new invalid
example to an actual structural violation (for example a non-string category),
or explicitly supersede the caller-category behavior and its accepted regression.
The former is recommended because it preserves the accepted library contract.
The owner must supply the ruling; the worker may not edit the published slice
or oracle, silently change the requested negative case, change the renamed
fixture to a built-in category, or remove existing assertions.

The whole-tree absence requirement additionally includes two stale docs outside
the explicit edit list: `OpticalConstructor.Storage/CanonicalJson.fs:128`
(`OpticalConstructorProject`) and `EntryFileJson.fs:87`
(`ProjectJson.serializeProject`). Authorize documentation-only cleanup in those
two files to complete the retirement census. No implementation change is needed
there.

## Artifacts

No new runtime artifacts were produced. Historical evidence read, not rerun:

- `.artifacts/0046-retry-red.log:2-4`: renamed-category export failed with
  `SchemaValidationErr` before enum removal.
- `.slices/0046-impl-log.md`, Attempt 02: documents the reproduced failure and
  repair, retaining structural constraints and caller-list membership checks.
- `.slices/0046-state-of-the-world.md`: documents folding the regression into
  the accepted 0035 fact while retaining all assertions and test identities.
- `.artifacts/code_judge/0046-01-code-judge.md`, cycle 4: review-green explicitly
  accepts the schema repair and renamed-category regression as satisfying
  `S17-CATEGORY-LIST`.

## Gotchas

- This is a concrete contradictory requirement, not missing permission for
  ordinary implementation or a request to expand the gate roster.
- Adding `Glass from the caller` to the enum would both violate exact schema
  preservation and encode a test fixture into the production schema. Special
  casing `NotACategory` would likewise be incorrect.
- No new test identity is missing authority: the new fact is already declared.
  The unresolved authority concerns its prescribed example, the category schema
  semantics and preservation of existing accepted assertions.
- `.manifest.state.json` was modified on entry and belongs to the supervisor;
  it was not edited. No git mutation, version bump, cross-repository edit,
  notebook change or analyzer configuration change was made.
