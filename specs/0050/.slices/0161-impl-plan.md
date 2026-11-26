# Step 0161 — implementation plan

The step removes the last `.ocproj` wording from `Berreman/OpticalConstructor/` and from the
`.gitignore` `*.binz` comment. It changes only comments, one schema `description` and two test
literals. There is no behaviour change.

Files and edits:

1. `OpticalConstructor.Storage/GroupsLibrary.fs`: the module doc (`:13-22`) becomes three lines.
   It names the per-user groups and experiment-collections file, read and written through the
   shared options and validated on load through `validateWith`. A `schemaVersion` mismatch is
   a `SchemaValidationErr`.
2. `OpticalConstructor.Domain/Groups.fs`: the `GroupsLibrary` doc (`:180-182`) loses the clause
   "it is kept apart from any per-project `.ocproj`".
3. `schema/optical-constructor-groups.schema.json`: the `description` (`:5`) loses
   "kept apart from any per-project .ocproj,". Nothing else in the schema changes.
4. `OpticalConstructor.Tests/GroupsRoundTripTests.fs`: the doc (`:20`) loses
   "independent of any `.ocproj`;".
5. `OpticalConstructor.Tests/EnvironmentRoundTripTests.fs`: the two old-file facts (`:383`,
   `:395`) graft `[ "C:/kits/a", "C:/kits/b" ]` and `[ "C:/kits/a" ]`. Their assertions do not
   change.
6. `Scene.fs`, `ExperimentCollectionStore.fs`, `OpticalConstructor.Domain.fsproj`: in scope
   only if a comment still names `.ocproj`. A case-insensitive `git grep` finds none, so they
   are left untouched.
7. `.gitignore`: the `*.binz` comment drops "Canonical `.ocproj` project files remain
   committable." The rule stays.

Risks: almost none. The schema is build-copied content, and only its free-text `description`
changes. Watch for CRLF churn (all files are LF), and keep the rewritten module doc within
AGENTS.md's comment rules (one to three lines, cite the spec).
