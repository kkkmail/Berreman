# Step 0037 - RENAME_CONTRACT - impl log

## Progress

- Read AGENTS.md, both worker protocols, the project prompt, the slice and its
  gate snapshot, naming conventions, the relevant spec/operator requirements,
  the S16-MOVE oracle and previous round outputs.
- Located the seam, its six facts, project wiring and Domain documentation.
  No repository-provided F# rename tool was found; use the explicit slice recipe
  as a deterministic text transform.
- Before implementation: move the seam into Domain, rename the two test modules,
  update their compilation entries and remove the retired Seeding project.
  Preserve the orchestrator and store bodies, all assertions and seed sources.
- Completed the deterministic recipe: moved the module into Domain immediately
  after SampleStore, renamed the three types and rejection case, and moved both
  test files with their declared modules and existing compilation positions.
- Removed the obsolete project comments, solution entry and sole project
  reference, then removed the retired directory and its build output after
  resolving the exact workspace-contained path and excluding reparse points.
- Updated the four Domain files' seam docs and shortened moved contract/module
  docs. The implementation bodies and test assertions were transformed only by
  the required symbol substitutions. Every write used UTF-8 without BOM and LF.
- Before handoff: write the state-of-the-world with the historical step-34 count
  of 791 constructor facts, explicitly unmeasured in this round.
- Read the resulting source and wiring diff for editorial review; changes were
  limited to the intended move, names, documentation and project wiring.
- Completed the state-of-the-world at the task's exact path. All implementation
  requirements are addressed; the supervisor's gate results remain pending.

## Files modified

- Created the implementation plan and this log at the task's exact paths.
- Moved `OpticalConstructor.Seeding/Seeding.fs` to Domain's `CatalogueImport.fs`.
- Deleted `OpticalConstructor.Seeding.fsproj` and the retired project directory.
- Updated Domain's project file and `Berreman/Berreman.slnx`.
- Renamed `SeedingProxyTests.fs` and `SeedingInMemoryTests.fs` to
  `CatalogueImportProxyTests.fs` and `CatalogueImportInMemoryTests.fs` and updated
  `OpticalConstructor.Tests.fsproj`.
- Updated documentation in Domain's `MaterialLibrary.fs`, `MaterialStore.fs`,
  `SampleStore.fs` and `ElementId.fs`.
- Created `specs/0050/.slices/0037-state-of-the-world.md`.

## Testing state

No gates or acceptance checks run. The RENAME_CONTRACT worker's invariant 6
assigns verification to the supervisor after exit. Build, elevation, the full
constructor unit suite and the six s06 invariants remain pending.

`gates_run` and `gate_results` are empty. Under the role-specific act-only
protocol, `commit_ready: true` reports completed implementation ready for the
supervisor's gate run; it does not report gate success.

## Artifacts

- Applied deterministic recipe: `.artifacts/0037-rename-catalogue-import.ps1`.
- No build, test, analyzer or acceptance-grep artifacts were generated.

## Gotchas

- The existing modification to `.manifest.state.json` belongs to the supervisor
  and must remain untouched.
- This slice explicitly keeps `seedAll` seed-fed and retains five original fact
  names, including the rejection fact's historical `SeedRejected` wording.
- The task-specific worker protocol supersedes the legacy local-gate steps in
  the shared protocol. No passing result will be claimed.
