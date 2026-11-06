# Step 0068 implementation log

## Progress

- Read AGENTS.md, the worker protocols, project prompt, naming conventions,
  slice 0068, its gate snapshot and the immutable acceptance obligations for 68.
- Before implementation: the version payload and three refusal formats are
  local to MaterialStore; resolveMaterialWithDisp duplicates the unknown-id
  format. Move those bodies and replace each original caller.
- Implemented: MaterialMetadata is public; MaterialEntry.versionPayload holds
  the unchanged complexity and display fields. MaterialError owns all three
  moved refusal formats, and every original caller now delegates to them.
- Preserved quoting before sample-name sorting, ascending numeric version
  sorting, duplicate counts and sample-reference priority over pins. No facts,
  durable formats or proxy shapes changed.
- Before verification: inspect the diff for move-only behavior, then compile
  OpticalConstructor.Tests and run existing material contract/resolution tests.
- After verification: the Release x64 test-project build succeeded, including
  the Domain, Storage and Ui consumers. All 94 existing tests in MaterialProxyTests,
  DispersionModelsTests and LibraryFacetsTests passed, with no failures or skips.
- Before handoff: wrote the required state-of-the-world, documented inherited
  checkpoint 66's 811 count without changing it, and reviewed the final diff.

## Files modified

- MaterialLibrary.fs: shared refusal members and resolution delegation.
- MaterialStore.fs: public metadata/payload member and store delegation.
- Created the required 0068 implementation plan and this log.
- Created the required 0068 state-of-the-world at the supplied path.

## Testing state

Local development build:
`dotnet build OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj -c Release -nologo -v:m`
from Berreman/ succeeded. Zero errors or warnings from our code; two existing
SYSLIB0051 warnings originate in vendored MathNetNumerics/Numerics/Exceptions.cs.

Local development test run used `dotnet test --no-build -c Release` with xUnit
`--filter-class` selections for OpticalConstructor.Tests.MaterialProxyTests,
OpticalConstructor.Tests.DispersionModelsTests and
OpticalConstructor.Tests.LibraryFacetsTests. All 94 cases passed, zero failed
and zero skipped. This covers metadata and physics version decisions, lifecycle
and deletion refusals, reference-before-pin priority, pinned versions and
known/unknown material resolution. No test facts or assertions changed.

The product diff passes whitespace review and has identical line counts with
and without `--ignore-cr-at-eol`. Both edited source files use LF.

These are local development results, not acceptance-gate results. The supervisor
must execute build, elevation, constructor-unit-tests and the ten s09 invariants.
No acceptance failure is waived; commit readiness is not asserted before those
checks. No implementation requirement remains outstanding.

## Artifacts

- specs/0050/.artifacts/0068-local-build.log
- specs/0050/.artifacts/0068-local-tests.log
- specs/0050/.artifacts/0068-local-test-results/material-members.trx

## Gotchas

- specs/0050/.manifest.state.json was dirty at entry and is left untouched.
- The most recent recorded constructor baseline is 811 at checkpoint 66;
  this worker must not author or change checkpoint counts.
- VersionNumber is declared after MaterialError, so versionsPinned belongs
  beside unknownMaterialVersion, while the other two members precede resolution.
- The worker protocol assigns acceptance to the supervisor. Higher-priority
  development instructions still require appropriate local verification; its
  results are not represented as acceptance-gate results.
- No implementation-scope deviation was needed. No git mutation, version bump,
  manual-note edit or analyzer-configuration change was made.
