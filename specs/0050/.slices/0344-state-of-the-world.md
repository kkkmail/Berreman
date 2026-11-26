# Step 0344 — state of the world

## Where we are

Spec 0050, section S10.2 (BerremanTestSupport: the shared ground truth and designs). This step creates the shared
test-support library `Berreman/BerremanTestSupport/` and moves its first source, the repository-root kit locator
`CommittedKit` (S9.4), into it out of `BerremanTests`. Later S10.2 steps move the ground truth and designs and add
the `OpticalConstructor.Ui.Tests` reference.

## What's working

- Create `BerremanTestSupport`, a plain net10.0 x64 library listed in `Berreman.slnx`
- Move `CommittedKit` from `BerremanTests` into it (namespace `BerremanTestSupport`, body unchanged)
- Reference `BerremanTestSupport` from `BerremanTests`; `MuellerReconstructionTests` opens it
- Exactly one `CommittedKit` remains; `Berreman/BerremanTests/` holds no `CommittedKits.fs`

## Tests

- build: green (80 s of 600 s); only the exempt NU1902 and the pre-existing vendored-MathNet SYSLIB0051 warnings.
- elevation: `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)` (218 s of 600 s); `BerremanTestSupport` 0.
- unit-tests over `BerremanTests.MuellerReconstructionTests`: Passed!, 25 succeeded, 0 failed, 0 skipped
  (count unchanged, numbers bit-identical: the locator's body is unchanged).
- s34 invariants: untouched by this diff (no VSTest package, no `kit.kit-json`, nothing in the known-findings list).

```yaml
gates:
  berreman_unit_tests: 25
```

## Architecture

- `BerremanTestSupport` is a plain library (no `OutputType`, `IsTestProject`, MTP runner or test package),
  referencing only `OpticalConstructor.Domain` (for `KitFolder`); `Softellect.Sys` arrives transitively.
- Its assembly name `BerremanTestSupport` sits outside `inScopePatterns` / `ownAssemblyPatterns`, so the ELEV
  analyzers scan it and apply no rule; no elevation mark is placed in it.

## Deferred

Nothing.

## Gotchas

- The file was moved with `mv`, not `git mv`: workers run no git-mutation command. Git sees the rename at commit.
- `OpticalConstructor.Ui.Tests` does not reference `BerremanTestSupport` yet; this step's effects do not touch it.

## Changelog

- 2026-11-12 — step 0344: `BerremanTestSupport` created; `CommittedKit` moved into it from `BerremanTests`.
