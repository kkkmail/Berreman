# Where we are

Step 0161 is the last S4.4 wording step after the `.ocproj` aggregate was retired in 0160.
It removes every remaining `.ocproj` mention from `Berreman/OpticalConstructor/` and from the
`.gitignore` `*.binz` comment. It changes only comments, a schema `description` and two test
literals, with no behaviour change. From S5.1 onward, `git grep -n -i -F "ocproj" --
Berreman/OpticalConstructor/` finding nothing is an invariant.

# What's working

- Shorten the GroupsLibrary module doc to three lines without the `.ocproj` reference
- Drop the `.ocproj` clause from the Domain `GroupsLibrary` doc and the groups schema description
- Graft kit-folder paths in the two old-environment-file facts; assertions unchanged
- Drop the `.ocproj` sentence from the `*.binz` comment; `.gitignore` still ignores `*.binz`

# Tests

As Invariant 6 requires, this worker ran no gates: `gates_run: []`, `gate_results: {}`. The
gate engine runs build, elevation, constructor-unit-tests and the twelve s18 invariants.

The acceptance `git grep` (read-only) now finds nothing. No fact is added, removed or renamed,
so the constructor count stays at checkpoint 159's measured 1,136. The two
`change_expectation` facts keep their identities and assertions.

```yaml
gates:
  constructor_unit_tests: 1136
```

# Architecture

No architectural change. The groups file keeps its store, its schema rules and its
validate-on-load path. `Scene.fs`, `ExperimentCollectionStore.fs` and the Domain fsproj had no
`.ocproj` comment line, so they are untouched.

# Deferred

Nothing.

# Gotchas

- The `ocproj` invariant covers `Berreman/OpticalConstructor/` tracked files only.
  `TestResults/*.trx` files are untracked, so `git grep` does not see their old test names.
- The `.gitignore` `*.binz` rule must stay. The sidecars (`Sidecar.fs`) still exist; only the
  project-file sentence went.
- `.manifest.state.json` is supervisor-owned and was left dirty as found.

# Changelog

- 2026-11-08: Step 0161 removed the last `.ocproj` wording (GroupsLibrary docs, groups schema
  description, test doc and literals, `.gitignore` comment); the retirement search finds nothing.
