# Reuse critique -- 002.slice-md cycle 1

## Coverage

- Helper roots walked: `C:\GitHub\Berreman` (focused: `Berreman/Berreman/` engine core, `Berreman/Berreman/OpticalConstructor/**`, `Berreman/BerremanTests/`).
- Files inspected: ~20/200 (targeted at the helper surfaces the diff touches; cap not reached).
- Extensions: `.fs` (the task's `.py,.md,.json` bound is a cross-repo template default — this is a pure-F# repo, so the reusable helpers live in `.fs`; walking `.json/.md` would surface no engine helpers).
- Diff read from `git diff HEAD` stdout (BeamTree.fs, Units.fs, both `.fsproj`) plus the four untracked new files (Project.fs, UnitsTests.fs, BeamTreeTests.fs, BeamRoutingTests.fs, GradientDiscretizeTests.fs).
- Engine seams confirmed reused, not forked: `OpticalSystemSolver`/`BaseOpticalSystemSolver` (`Solvers.fs:147,156,199`), `EmField.propagate` (`BerremanMatrix.fs`), `WaveLength` + `.value` (`Fields.fs:280,284`), `Constants.fs` factors, `Layer`/`Thickness`/`OpticalSystem` (`Media.fs`), `OpticalProperties` (`MaterialProperties.fs`).

## Findings

### F1: New test project hand-rolls scalar tolerance comparison three ways, diverging from the project's named comparison-helper precedent

- **Worker added:** a private `relClose expected actual relTol` in `UnitsTests.fs:14-16`, an inline relative check `abs ((sum - total) / total) <= 1e-12` in `GradientDiscretizeTests.fs:34,54`, and xUnit's precision overload `Assert.Equal(expected, actual, 12)` in `BeamRoutingTests.fs:49,50,70,81` — three distinct numeric-tolerance idioms across four sibling test files in the same new `OpticalConstructor.Tests` project.
- **Existing helper:** `MatrixComparison.fs:13` `allowedDiff` and its norm-ratio comparison pattern `(diffNorm / norm).Should().BeLessThan(allowedDiff, …)` (`MatrixComparison.fs:33-34,50-51,65-66`), which the project prompt names explicitly as "the project's chosen comparison helpers" / floating-point-tolerance precedent (`.user-md/Berreman/arc-runner.user-md:127`).
- **Why it matters:** the project has a single, named precedent for "how this repo asserts numeric closeness in tests," and the new test project establishes a different convention — and three variants of it — for the same problem. Future OpticalConstructor.* test files have no single comparator to copy, so the divergence compounds slice by slice (this is the arc's first test project, so the pattern set here propagates).
- **Important caveat (why this is low-confidence):** `MatrixComparison.fs` lives in the separate `BerremanTests.fsproj`, is not referenced by `OpticalConstructor.Tests.fsproj`, depends on FluentAssertions, and exposes only matrix/vector/Stokes comparators bound to Math.NET types — it has **no scalar `float`-vs-`float` relative-closeness function** to call directly. So there is no drop-in symbol to reuse; literal reuse would require a cross-project reference plus a new scalar overload. The system prompt forbids me proposing a new shared helper, so I record this as pattern divergence against an existing named precedent, not as direct duplication.
- **Suggested action:** advisory only — most defensibly **leave as-is**; the three idioms are each correct and the tolerances are sound. If the judge wants convergence, the minimal move is to pick ONE idiom for the new test project and apply it uniformly (the inline relative-ratio already used in two files is the lightest, needs no FluentAssertions, and mirrors `MatrixComparison`'s `diff/norm` shape). No engine change; no cross-project reference required.

## Bottom line

The diff is strongly reuse-disciplined: the beam-tree topology, `BeamTreeError`, `GradientLayer`/`discretize`, and the eV/cm⁻¹/Å conversions are all genuinely net-new with no engine equivalent (confirmed — no `1239.84`, eV, wavenumber, or layer-discretization helper exists anywhere in `Berreman/Berreman/**`, and `RepeatBuilder.expand` is not yet in-tree), and every engine seam the spec names is *called* rather than re-typed (`OpticalSystemSolver`, `BaseOpticalSystemSolver`, `EmField.propagate`, `WaveLength`, `Constants` factors, `Layer`/`OpticalSystem`/`OpticalProperties`), with the tests reusing `OpticalProperties.fromRefractionIndex`/`.vacuum`, `IncidentLightInfo.create`, and `Thickness.nm`. The single finding is a low-confidence test-comparison pattern divergence with no scalar helper to literally reuse, so it is not substantive enough on its own to motivate a re-spawn — the judge can reasonably treat this diff as clean.
