# Step 0161 — implementation log

## Progress

- Read the system prompts, the project prompt, the slice, manual 013 and the gate snapshot.
  Checkpoint 159 is recorded in `.checkpoints-json` (`constructor_unit_tests: 1136`).
- Starting `git grep -n -i -F "ocproj" -- Berreman/OpticalConstructor/` found six lines:
  `GroupsLibrary.fs:15`, `Groups.fs:182`, `optical-constructor-groups.schema.json:5`,
  `GroupsRoundTripTests.fs:20` and `EnvironmentRoundTripTests.fs:383`, `:395`. `.gitignore:338`
  carried one more.
- Applied the seven edits in the files below. Only comments, one schema `description` and two
  test literals changed.
- Afterwards the same `git grep` finds nothing (exit 1). `.gitignore:339` still reads
  `*.binz`.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Storage/GroupsLibrary.fs`: the ten-line
  module doc is now three lines. It names the per-user groups and experiment-collections file,
  read and written through the shared `CanonicalJson.options` and validated on load through
  `validateWith` (spec 0050 step 29). A `schemaVersion` mismatch is a `SchemaValidationErr`,
  never a migration.
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/Groups.fs`: the `GroupsLibrary` doc
  drops "it is kept apart from any per-project `.ocproj`".
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/schema/optical-constructor-groups.schema.json`:
  the `description` drops "kept apart from any per-project .ocproj,". Nothing else in the schema
  changed.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/GroupsRoundTripTests.fs`: the AC-G3 doc
  bullet drops "independent of any `.ocproj`;". The rest of that bullet was reflowed onto two
  lines, with the same words.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/EnvironmentRoundTripTests.fs`: the two
  old-file facts now graft `[ "C:/kits/a", "C:/kits/b" ]` and `[ "C:/kits/a" ]`. Their
  assertions are unchanged.
- `.gitignore`: the `*.binz` comment drops "Canonical `.ocproj` project files remain
  committable." The rule is unchanged.
- `Scene.fs`, `ExperimentCollectionStore.fs`, `OpticalConstructor.Domain.fsproj`: not
  modified. No comment line in them names `.ocproj` (case-insensitive `git grep`, and the
  fsproj's `:141-155` was read directly), so item (3) has nothing to remove.

## Testing state

As Invariant 6 requires, this IMPLEMENT worker ran no gates. The gate engine runs build,
elevation, constructor-unit-tests and the twelve s18 invariants after exit. The only commands
run were read-only:

- the acceptance `git grep`, which found nothing;
- `git diff --numstat` against `--ignore-cr-at-eol`: identical for every product file, so
  there is no CRLF churn.

Expected test effects: no fact is added, removed or renamed, so the constructor count stays
at checkpoint 159's 1,136. Both `change_expectation` facts keep their identities and
assertions over the new literals. The literals reach only the ignored `recentFiles` property:
the root environment schema is permissive and STJ drops the unmapped field on bind, so they
cannot change either outcome. The schema's `description` is a free-text annotation and does
not affect validation. Every GroupsRoundTripTests fact is untouched.

## Artifacts

None. The step produced no run artifact.

## Gotchas

- Item (3) of the slice is conditional ("a comment line of them that still names `.ocproj`").
  None does at HEAD `e43fa72`, so those three files stay byte-identical.
- The slice cites `GroupsLibrary.fs:13-25`. At HEAD the doc spans `:13-22` (the module line is
  `:23`); the cited range came from an earlier line layout.
- `specs/0050/.manifest.state.json` was already dirty, and it is supervisor-owned; it was left
  alone. No git mutation was made.
