# Step 0347 — impl log

## Progress

- Read the task, both system prompts, the project prompt, AGENTS.md and the slice. There is no operator note and no
  `## Operator rulings for this step` section. None of the standing rulings in the project prompt names step 0347;
  ruling 013 (arc-origin elevation scope) applies as always.
- `OpticalConstructor.Ui.Tests.fsproj` now has
  `<ProjectReference Include="..\..\BerremanTestSupport\BerremanTestSupport.fsproj" />` as the last line of its
  `ProjectReference` item group, right after the `OpticalConstructor.TestWindows.App` reference. It has no comment.
  The diff is one added line (`git diff --numstat`: `1 0`).
- No source file of the suite changed, and no test, package or other reference was added.
- No `.fs` under `OpticalConstructor.Ui.Tests` opens or names `BerremanTestSupport`, `CommittedKits` or a
  `*BiaxialSample` module, so the new assembly cannot cause a name clash.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/OpticalConstructor.Ui.Tests.fsproj`. This is the step's
  only authorized edit.

The file has no CR. `git diff --numstat` equals `--ignore-cr-at-eol` (`1 0`).

## Testing state

These are development runs of each gate's own command from `0347.gates`, over the final tree.

- **build** (`dotnet build Berreman.slnx -c Release -nologo -v:m`, run from `Berreman/`): exit 0, `0 Error(s)`, 79 s
  (deadline 600 s). It printed `8 Warning(s)`, all outside our code:
  - 6 × NU1902, two each on `BerremanTestSupport`, `BerremanTests` and now `OpticalConstructor.Ui.Tests`. This is an
    exempt advisory: `Microsoft.AspNetCore.WebUtilities` 2.3.12 reaches every project through `Softellect.Sys` →
    CoreWCF → Kestrel.Core 2.3.13. See Gotchas.
  - 2 × SYSLIB0051 from the vendored `MathNetNumerics/Numerics/Exceptions.cs`. These were already there before this
    step.
- **elevation** (`Run-ElevationGate.ps1 -ExcludeRules ELEV004`, run standalone, so all 13 projects):
  `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)`, 0 known findings, exit 0, 221 s (deadline 600 s).
  - The suppressed count is unchanged at 1502, the same as step 0346.
  - The test projects are out of the gate's scope, so the gate analyzed nothing new.
- **ui-smoke** (the whole suite, `--filter-trait "Category=ui-smoke"`): `Test run summary: Passed!`, 258 of 258,
  0 failed, 0 skipped, 231 s (deadline 600 s). The count is unchanged against the checkpoint baseline
  `ui_smoke_tests: 258`. This run overlapped the elevation run.
- **ui-tests** (the snapshot's `step_select` `OpticalConstructor.Ui.Tests.FsxAcceptanceTests`, using the gate's own
  selector): `Test run summary: Passed!`, 14 of 14, 0 failed, 0 skipped, 73 s (deadline 360 s). The TRX shows all 14
  `FsxAcceptanceTests` facts `Passed`.
- **s34 invariants**: I decoded all 20 `git grep` commands from the snapshot and ran each one as written and again
  with `--untracked`. None found anything (20 of 20 OK).
- **Acceptance structure**: `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/bin/x64/Release/net10.0/BerremanTestSupport.dll`,
  `Berreman/BerremanTests/bin/x64/Release/net10.0/BerremanTestSupport.dll` and
  `Berreman/BerremanTestSupport/bin/x64/Release/net10.0/BerremanTestSupport.dll` have the same MD5
  (`8df3f8c6c2fa7fab8db6e9545d858810`). Both suites therefore build against one compiled `BerremanTestSupport`.
- No run went past its effective deadline.

## Artifacts

- `specs/0050/.artifacts/0347-build.log`
- `specs/0050/.artifacts/0347-elevation.log`
- `specs/0050/.artifacts/0347-ui-smoke.log`
- `specs/0050/.artifacts/0347-ui-tests.log`
- `specs/0050/.artifacts/0347-invariants.log`

## Gotchas

- **NU1902 now also appears on `OpticalConstructor.Ui.Tests`.** I checked the restored package graphs
  (`obj/project.assets.json`) of `OpticalConstructor.Ui.Tests`, `.Ui`, `.Domain`, `.App` and `BerremanTestSupport`:
  - all five resolve the same `Microsoft.AspNetCore.WebUtilities/2.3.12` through
    `Microsoft.AspNetCore.Server.Kestrel.Core/2.3.13`, with `auditMode: all`;
  - so the reference changed nothing in the package graph;
  - the advisory is logged only by projects that actually re-restored this round (`OpticalConstructor.Ui.Tests`,
    because its project file changed, plus the two that already showed it at step 0346);
  - projects whose restore was a no-op keep their older, empty audit log.

  NU1902 is one of AGENTS.md's exempt NuGet advisories about third-party packages, so nothing in our code needs fixing.
- The brief puts the `OpticalConstructor.TestWindows.App` reference at `OpticalConstructor.Ui.Tests.fsproj:268`.
  In the tree it was at line 278; line 268 opens the `ItemGroup`. The new reference went after the
  TestWindows.App line, which is what the brief means.
- The brief says the `ui-tests` gate "is not selected", but the compiled snapshot has
  `step_select: [OpticalConstructor.Ui.Tests.FsxAcceptanceTests]`. The snapshot decides, so I ran that class.
  It passed (14 of 14).
