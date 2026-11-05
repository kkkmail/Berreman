# Step 0038 - IMPLEMENT - impl log

## Progress

- Read AGENTS.md, the shared and IMPLEMENT worker protocols, the project
  prompt, slice, gate snapshot, naming conventions, S16-BUILTIN-FOLDER oracle,
  relevant catalogue requirements and previous round outputs.
- Located the existing folder proxy, five parity facts and Storage Content item.
  The only pre-existing working-tree change is the supervisor's manifest state.
- Before the first substantive step: add the exact named location fact with
  independent path, successful reads, identity and full catalogue equality.
- Added the fact to `CatalogueFolderTests.fs` using the existing `okOf` helper
  and typed equality assertions. Its expected folder is independently stated
  under `System.AppContext.BaseDirectory`; no fixture writes the shipped files.
- Before the second substantive step: add the production member over the
  existing folder proxy and route parity reads through it, preserving assertions.
- Added `createBuiltIn` beside `create`, with an elevated folder local and the
  required one-line spec citation. It delegates directly to `CatalogueProxy.create`.
- Replaced the parity reader's path with `createBuiltIn` and removed unused
  System imports. All five existing parity facts and their assertions remain.
- Before handoff: review the source diff and write the mandatory state file,
  retaining the previous round's historical count without claiming a new result.
- Reviewed the source diff: only the three authorized code files changed; the
  production member delegates to the existing proxy and the parity assertions
  are unchanged. Ordinary and ignore-CR numstats agree for the edited code.
- Wrote `0038-state-of-the-world.md` at the exact task path with all mandatory
  headings, pending gate roster and the explicitly historical inherited count.
  No implementation requirement was deferred or changed.

## Files modified

- Created this log and `0038-impl-plan.md` at the task's prescribed paths.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/CatalogueFolderTests.fs`.
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/CatalogueFolderStore.fs`.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/BuiltInCatalogueParityTests.fs`.
- `specs/0050/.slices/0038-state-of-the-world.md`.

## Testing state

No gates run. The IMPLEMENT worker's invariant 6 assigns verification to the
supervisor after exit. Build, elevation, constructor-unit-tests and the six s06
invariants remain pending. No baseline or passing result is authored here.

`gates_run` and `gate_results` are empty. Under the role-specific act-only
protocol, `commit_ready: true` reports completed implementation ready for the
supervisor's gate run; it does not report gate success.

## Artifacts

No run artifacts generated.

## Gotchas

- Leave the supervisor's existing `.manifest.state.json` modification untouched.
- The act-only worker protocol supersedes the shared protocol's legacy local
  gate instructions. Final gate collections remain empty.
