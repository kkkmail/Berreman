# Step 0037 - RENAME_CONTRACT - impl plan

Move the existing seeding module into Domain as `CatalogueImport.fs`, directly
after `SampleStore.fs`. Apply the slice's exact type, error-case and namespace
renames while retaining the four proxy fields, five store fields, reference
equality, seed-fed orchestration and in-memory implementation. Use a deterministic
text-transform recipe; no repository-provided F# rename tool was found.

Rename both test files and declared modules at their existing compile positions.
Keep every assertion and fact name except the specified reference-equality fact.
Update the four Domain files' seam documentation, remove the obsolete project
comments and references, and delete the Seeding project directory including its
build output. Check the resolved deletion target stays inside this workspace.

The implementation touches only the files authorized by slice 0037 plus these
worker outputs and its recipe under the supplied artifacts folder. Preserve the
supervisor's existing manifest change. No version bump, catalogue data change,
new behavior, compatibility alias or git mutation is needed. Existing spec and
operator history retains its historical names.

Compile order, mechanical preservation of the six facts and deletion of the old
project are the main risks. The RENAME_CONTRACT protocol assigns build, elevation,
constructor-unit-tests and all six s06 invariants to the supervisor; this worker
does not run gates or acceptance greps. Copy only a historical supervisor count
into the final state-of-the-world, clearly identified as unmeasured this round.
