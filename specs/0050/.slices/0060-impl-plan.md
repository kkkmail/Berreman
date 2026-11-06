# Step 0060 implementation plan

Implement STORE_BigBoy_0002 as the C# EF Core library model specified by
S21-LIBRARY-MODEL. Add the seven sealed row types and LibraryDbContext with
explicit table names, caller-owned keys, required strings, query indexes and
the specified delete behavior. Reuse the Database project's existing SQLite
provider and central package versions. The sample's material reference remains
an indexed pair without a foreign key.

Add the Database project reference beside Storage's Domain reference. Append
LibraryDatabaseTests.fs to the constructor test project's compile list. Its
single fact will create the schema on an open in-memory SQLite connection,
compare the exact table and column sets, and prove EnsureCreated is idempotent.

Product edits are limited to the five paths in the slice's authorized effects.
STORE_BigBoy_0002 is already registered as declared at step 60 in
.contracts-json, so no registry edit is needed. No UI, runtime composition,
schema stamping implementation, migrations or new providers belong here.

Watch for nullable-reference warnings and transitive assembly conflicts from
the new Storage reference. Keep worker output in the supplied .slices paths
and any validation captures in .artifacts. The supervisor owns the mandatory
gate decisions and measured checkpoint. The inherited constructor count is
794 at checkpoint 58; do not author a new baseline.
