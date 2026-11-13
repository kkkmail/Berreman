# Step 0347 — state of the world

## Where we are

This step is part of spec 0050 S10.2 (001 §Q AT-2, §S10), the shared test-support project.
- Steps 0344–0346 created `BerremanTestSupport` and moved into it the committed-kit locator, the biaxial ground truth
  and the absorbing sample.
- At that point only `BerremanTests` referenced it.
- This step makes `OpticalConstructor.Ui.Tests` reference it too, so one compiled copy serves both test suites.
- No source file changed.

## What's working

- Referenced `BerremanTestSupport` from `OpticalConstructor.Ui.Tests.fsproj`, after its TestWindows.App reference,
  with no comment.
- `BerremanTests` and `OpticalConstructor.Ui.Tests` now build against one `BerremanTestSupport` assembly. All three
  output copies are byte-identical.
- The headless suite still loads, discovers and runs: ui-smoke passed 258 of 258, the same count as before.

## Tests

These are development runs of each gate's own command from `0347.gates`, over the final tree. The arc-runner's gate
engine runs the gates themselves.

| Gate | Result | Time | Deadline |
|---|---|---|---|
| build | exit 0, 0 errors; 8 warnings: NU1902 (exempt advisory) and the vendored MathNet SYSLIB0051 | 79 s | 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)`; known findings 0; ELEV004 excluded | 221 s | 600 s |
| ui-smoke | Passed!, 258 of 258, 0 failed, 0 skipped | 231 s | 600 s |
| ui-tests (`FsxAcceptanceTests`) | Passed!, 14 of 14, 0 failed, 0 skipped | 73 s | 360 s |
| invariant-s34-\* (20) | each grep found nothing, both as written and with `--untracked` | under 1 s each | 300 s |

- No run went past its effective deadline. ui-smoke overlapped the elevation run.
- ui-smoke is unchanged against the checkpoint baseline (258).
- Elevation's suppressed count is unchanged at 1502.
- The only touched file is LF-only: `git diff --numstat` equals `--ignore-cr-at-eol`.

```yaml
gates:
  ui_smoke_tests: 258
  ui_tests: 14
```

## Architecture

- The dependency is one-way: `BerremanTestSupport` → Domain + Optimization. Both suites now point at it, and it still
  references no test project and no product project references it.
- The reference brings in no new assembly or package. Domain and Optimization already reached the suite through
  `OpticalConstructor.Ui`, and the restored package graph matches that of `.Ui`, `.Domain` and `.App`.

## Deferred

- Nothing. The step asked for one reference, and it is in place.

## Gotchas

- **NU1902 now also shows on `OpticalConstructor.Ui.Tests`.** NuGet logs the WebUtilities 2.3.12 advisory only for
  projects that actually re-restore, and this suite re-restored because its project file changed. Its graph is the same
  as before. NU1902 is an exempt advisory under AGENTS.md.
- The brief says the `ui-tests` gate is not selected, but the snapshot selects `FsxAcceptanceTests`. It passed.
- The brief's line citation (`:268`) points at the `ItemGroup` start. The TestWindows.App reference was at line 278.

## Changelog

- 2026-11-12 — Step 0347: `OpticalConstructor.Ui.Tests` references `BerremanTestSupport` (001 §Q AT-2). Build,
  elevation, ui-smoke (258), ui-tests (`FsxAcceptanceTests`, 14) and all 20 s34 invariants are green.
