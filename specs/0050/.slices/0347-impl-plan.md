# Step 0347 — impl plan

## Approach

001 §Q AT-2 / §S10 / the S10.2 brief: `OpticalConstructor.Ui.Tests` references the shared
`BerremanTestSupport` project so one compiled copy of the ground truth, the designs and the kit locator
serves both test suites. The step is one line in one project file:

- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/OpticalConstructor.Ui.Tests.fsproj` gains
  `<ProjectReference Include="..\..\BerremanTestSupport\BerremanTestSupport.fsproj" />` after its last
  `ProjectReference` (the `OpticalConstructor.TestWindows.App` one), with no comment.

No source file of the suite changes, no test and no package is added.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/OpticalConstructor.Ui.Tests.fsproj` (the only
  authorized edit).

## Risks

- A new transitive assembly or a version conflict (MSB3277): `BerremanTestSupport` references only
  `OpticalConstructor.Domain` and `OpticalConstructor.Optimization`, both already reached through
  `OpticalConstructor.Ui`, so none is expected; the build log is checked for new warnings.
- Name clashes when the suite opens nothing from the new assembly: none, since no source opens it.
- CRLF churn in the project file: checked with `git diff --numstat` vs `--ignore-cr-at-eol`.
