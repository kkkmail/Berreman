# Step 0001 — ADD_RED — impl log

## Progress

- Read the protocol (arc-runner.system-md, add_red_worker.system-md), the project prompt, the
  slice, `AGENTS.md`, `MaterialImport.fs` (`entryFromTabulated` `:183-194`,
  `buildTabulatedClosure` `:158-179`), `MaterialComplexityEditor.fs:864` (`isEditable`), the
  fixtures `Si.yml` / `sample-nk.csv`, and slice 0002's brief (so the facts fit the
  `TabulatedNK` shape it lands: `sampleRange` endpoints are `WaveLength.nm`, so the facts
  compare `.value` in metres, never the `WaveLength` case).
- Added the three facts to `MaterialImportTests` (module `MaterialImportTests`), right after the
  tabulated facts (`:35-60`), before the formula helpers:
  - `TabulatedSample` / `TabulatedFixture`: private test-local records holding the two fixtures'
    rows written out by hand (λ as `UnitMagnitude` in the file's own unit, n, k). `first` /
    `last` / `waveLengthAt` go through `toWaveLength`, the path the existing facts already use.
    No fixture is parsed a second way.
  - `assertViewOnlyTabulatedSegment` (facts 1 and 2): `Ok entry` → `complexity` must be `Some c`
    (a `None` fails with "got a None complexity") → `c.eps` must be
    `EpsWithDispValue (IsotropicDispersive [ segment ])` with lower/upper `.value` equal to the
    first/last sample within `1e-15<meter>` → `c.magnetic, c.active` must be `None, None` →
    `c.isEditable` must be false → `entry.properties.epsWithDisp.getEps w = c.toProperties.epsWithDisp.getEps w`
    at every sample (F# structural equality on `Eps`; the vendored MathNet `Matrix<T>.Equals`
    is element-wise, `Matrix.BCL.cs:51` → `MatrixStorage.cs:144`).
  - `assertSamplesAndMidpoints` (fact 3): `epsAt` at every sample equals `nkSquared n k`, and at
    every neighbouring midpoint equals `nkSquared` of the mean n and mean k, each within
    `closeC 1e-12`.
  - Opened `OpticalConstructor.Domain.MaterialComplexityEditor` for the `isEditable` extension
    (`MaterialComplexityTests` opens it too; no name it defines collides with this file's).
- No fact names an `EpsAxisDispersion` case that does not exist yet; the file compiles on the
  start tree.
- No production file touched.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Tests/MaterialImportTests.fs` (+102 lines,
  LF; `git diff --numstat` equals `--ignore-cr-at-eol`).

## Testing state

Observed locally (to record the red, per the ADD_RED obligation; the arc-runner's gate engine
runs and records the gates):

- `dotnet build OpticalConstructor.Tests.fsproj -c Release` — succeeded. The only warnings are
  the vendored `MathNetNumerics/Numerics/Exceptions.cs` SYSLIB0051 pair, already present on the
  start tree (`.artifacts/b11-build.log` carries the same four lines). Log:
  `.artifacts/0001-build-tests-project.log`.
- `dotnet test --no-build -c Release -p:Platform=x64 --filter-method "OpticalConstructor.Tests.MaterialImportTests.*" --filter-method "OpticalConstructor.Tests.MaterialImportTests"`
  — exit 2, `total: 16, failed: 2, succeeded: 14`. Log:
  `.artifacts/0001-red-materialimporttests.log`. The actual failures:

  ```
  failed OpticalConstructor.Tests.MaterialImportTests.an n,k CSV imports as one view-only dispersive segment over its sample range (158ms)
    expected the tabulated import to carry a dispersive complexity, got a None complexity
      at OpticalConstructor.Tests.MaterialImportTests.assertViewOnlyTabulatedSegment(...) in ...\MaterialImportTests.fs:118
  failed OpticalConstructor.Tests.MaterialImportTests.a tabulated refractiveindex.info page imports as one view-only dispersive segment over its sample range (4ms)
    expected the tabulated import to carry a dispersive complexity, got a None complexity
      at OpticalConstructor.Tests.MaterialImportTests.assertViewOnlyTabulatedSegment(...) in ...\MaterialImportTests.fs:118
  ```

  That is exactly the acceptance: the two shape facts fail on a `None` complexity
  (`entryFromTabulated` sets `complexity = None`, `MaterialImport.fs:193`); `tabulated imports
  reproduce every sample and the linear interpolation between neighbours` and the 13 facts the
  class held before pass (13 + 1 = 14).
- `elevation`: the test projects are outside the analyzers' scope, and no product project
  changed. The three s01 invariants grep files this step does not touch
  (`Elevation/elevation-known-findings.json`, project/props/targets files, `Berreman/` for
  `kit.kit-json`).

## Artifacts

- `specs/0050/.artifacts/0001-build-tests-project.log`
- `specs/0050/.artifacts/0001-red-materialimporttests.log`

## Gotchas

- Step 2 builds the segment interval from `TabulatedNkTable.sampleRange` as `WaveLength.nm`
  values while the fixture rows are µm (`Si.yml`): comparing the `WaveLength` values
  structurally (`Mkm` vs `Nm`) would fail on a correct import, so facts 1 and 2 compare
  `.value` in metres within 1e-15 m, as the slice asks.
- The seed-invariant check is exact (`=` on `Eps`), not a tolerance: `properties` and
  `complexity.toProperties` evaluate the same data through the same path, so they agree to the
  bit. A tolerance would hide a second evaluation path.
- Fact 3's midpoints come from `(a + b) / 2` of the row magnitudes in the file's own unit; the
  interpolation weight is then 0.5 up to a few ulps, far inside `1e-12` on eps.
