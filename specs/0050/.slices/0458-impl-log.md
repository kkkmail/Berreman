# Step 0458 — impl log

## Progress

- Read the task, both system prompts, the project prompt, rulings R0001–R0005. None of them touches this file: their
  extra authority is confined to 0427 / 0428 / 0436, and later steps only have to preserve their rules, which a
  tests-only change does.
- Read the slice, `MaterialSourcePullSqliteTests.fs`, `MaterialSourceFixture.fs`, the PTB / Henke facts of
  `MaterialSourceStoreTests.fs`, `MaterialSourceStore.fs` (PTB / Henke adapters), `MaterialSourcePulls.fs`,
  `CatalogueImport.attributionLinesOf`, `LibraryEntryFiles` attribution, `MaterialImport.PtbOcdbText.importMaterial`.
- Generalised the class's helpers so all three adapters share them:
  - `fixtureWith database adapter` composes the import context (fixed `newEntryId`) and the material store;
    `fixtureOver` (refractiveindex.info) and the new `ptbFixtureOver` call it.
  - `resultAt` takes the `MaterialSourceHit` it reports.
  - `pulledFixture` became `pulledIn hit fixture`.
  - `tabulatedTableOf` is factored out of `nAtLongestWaveLength`.
  - The existing four facts keep every assertion; only their helper call sites changed (`siHit` moved to module
    level).
- Added `coHit`, `silicaHit`, `ptbFirstPull` / `ptbChangedPull`, `silicaPull`, `ptbTable`, `firstSigmaN` and
  `silicaTable`, then the three facts the step names:
  - `a pulled PTB OCDB record keeps its uncertainties in the library and credits PTB`: covers the pull at v1;
    `resolveVersion` v1 named `Co (PTB OCDB)`, its table equal to the import (σ included), first σn =
    (0.96804 − 0.96772) / 6; the provenance and `UserManaged`; one `PulledSources` row; and the exact CC-BY
    attribution line with its citation.
  - `an update whose PTB OCDB bounds changed mints version 2 and keeps version 1`: covers `updateSourceSnapshot` with
    `copyingDownloader` of `writePtbArchive (changedPtbRecord ())` at 2026-11-04, which answers `PullMintedVersion`
    at v2. v2's table equals the changed import, with first σn = (0.96810 − 0.96772) / 6, and v2's provenance is the
    changed pull. v1 is unchanged, with its own σn; the held versions are [1; 2], with two `PulledSources` rows.
  - `a pulled Henke compound is a user material at version 1 under CXRO's provenance, its licence named`: covers the
    search `SiO2 2.2` → `SiO2@2.2` and the pull at v1. It checks `tryLatestEnvelope`'s `SourcePullProvenance` (CXRO
    Henke, record, 2026-11-01, `LicenseRef-CXRO`, step 457's citation, hash over the record text + `si.nff` +
    `o.nff`) and `UserManaged`. The resolved table equals `indexTableWith` of the fixture factors. It also checks the
    `PulledSources` row and the licence-only attribution line. A 404-serving downloader records that nothing was
    downloaded (`Assert.Empty asked`).

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Tests/MaterialSourcePullSqliteTests.fs` (the step's one authorized
  edit). No product file changed.

## Testing state

These are development checks only. The gate engine runs the roster after this session and is the authority. Every
figure is from the final source.

- **build**: exit 0 in 85 s (600 s deadline), 0 errors.
  - Warnings: 12 × NU1902 (exempt) and 4 × SYSLIB0051 from the vendored `MathNetNumerics/Numerics/Exceptions.cs`
    (see Gotchas). There is no FS or MSB warning.
  - Log: `.artifacts/0458-dev-build.log`.
- **Touched class, filtered** (`OpticalConstructor.Tests.MaterialSourcePullSqliteTests`): `Passed!`, 7 of 7 (the 4
  existing and the 3 new facts).
  - `--list-tests` shows the census's three identities exactly.
  - Log: `.artifacts/0458-dev-touched-class.log`.
- **constructor-unit-tests, whole suite as the gate selects it** (`all_discovered`, the gate's own command):
  `Passed!`, 1823 of 1823, 0 skipped, in 106 s (180 s deadline).
  - 1823 = 1820 (step 0457's baseline) + 3, the count rising by 3 that the acceptance asks for.
  - Log: `.artifacts/0458-dev-constructor-unit-tests.log`.
- **elevation, all 13 projects** (standalone, no comparison base): `ELEV-GATE: OK - 0 new finding(s) in 13 project(s), 241s`, known findings 0, against a 600 s deadline. Log: `.artifacts/0458-dev-elevation.log`.
- **21 s44 invariants**: each decoded `git grep` finds nothing (exit 1), both as written and with `--untracked`.
  Log: `.artifacts/0458-dev-invariants.log`.
- **LF**: `git diff --numstat` equals `--ignore-cr-at-eol` (236 / 33).

## Artifacts

- `.artifacts/0458-dev-build.log`
- `.artifacts/0458-dev-touched-class.log`
- `.artifacts/0458-dev-constructor-unit-tests.log`
- `.artifacts/0458-dev-elevation.log`
- `.artifacts/0458-dev-invariants.log`

## Gotchas

- **σ equality is exact on purpose.** The reader computes `(high − low) / (2.0 * float 3)` from the parsed bound
  strings. The test's `(0.96810 − 0.96772) / 6.0` uses the same doubles and the same operations, and the canonical
  payload round-trips doubles. So the comparison is bit-exact, as the class's existing `Assert.Equal(3.790, …)` is.
  A tolerance would hide a lost or rescaled σ.
- **Snapshot folder names are hard-coded.** `ptb-ocdb` / `cxro-henke` are private to the adapters' layout. As in
  `MaterialSourceStoreTests`, the tests name the folder each adapter reads.
- **`refreshedWith` stamps three files.** That is true of both the refractiveindex.info and the PTB archives (step
  455's store fact counts `COPYING`, `Co.txt` and `Ru_2019.txt`), so one helper serves both updates.
- **Henke's expected factors are rebuilt locally.** `silicaTable` reads the fixtures' factors through
  `HenkeScatteringText` and `CompoundSpec.indexTableWith`. The equivalent local inside step 457's
  `MaterialSourceStoreTests` fact is not reachable: it is fact-local, and that file is outside this step's
  authorized edits.
- **SYSLIB0051 is pre-existing, not ours.** It comes from vendored MathNet source unchanged since 2022, and appears
  whenever MathNet recompiles (also in `.artifacts/b11-build.log`). This step touched nothing there, and it is
  outside the step's authority.
