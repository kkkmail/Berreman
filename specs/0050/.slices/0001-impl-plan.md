# Step 0001 — ADD_RED — impl plan

## Approach

Red first for 001 §B "Physics is pure data" (.spec-md §3.1): a tabulated import must become
axis data. This step adds three facts to `MaterialImportTests` (the only authorized edit,
`Berreman/OpticalConstructor/OpticalConstructor.Tests/MaterialImportTests.fs`); no production
file changes.

- A test-local record `TabulatedSample` (λ magnitude in the fixture's own unit, n, k) and the two
  fixtures' rows written out by hand (`Si.yml` in µm, `sample-nk.csv` in nm). The facts never
  parse a fixture a second way.
- One private helper asserts the view-only segment shape: `complexity = Some c`,
  `c.eps = EpsWithDispValue (IsotropicDispersive [ segment ])`, `c.magnetic = None`,
  `c.active = None`, the segment's lower/upper `.value` equal the first/last sample in metres
  within 1e-15 m, `c.isEditable = false`, and `entry.properties.epsWithDisp.getEps w` equals
  `c.toProperties.epsWithDisp.getEps w` at every sample. A `None` complexity fails first with
  a message naming it.
- Fact 1 (YAML) and fact 2 (CSV) call that helper. Fact 3 checks `epsAt` at every sample and
  at every neighbouring midpoint against `nkSquared` (mean n, mean k) within `closeC 1e-12`.
- No fact names an `EpsAxisDispersion` case that does not exist yet, so the file compiles on the
  start tree.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Tests/MaterialImportTests.fs` (edit).

## Risks

- `segment.wavelengthInterval` endpoints: step 2 builds them as `WaveLength.nm`, so the facts
  compare `.value` in metres, never the `WaveLength` case.
- `isEditable` is an extension in `OpticalConstructor.Domain.MaterialComplexityEditor`; the file
  must open that module (MaterialComplexityTests already does, with no name clash).
- Fact 3 must be green on the start tree and stay green in step 2: interpolation at a midpoint
  gives t = 0.5 up to a few ulps, far inside 1e-12 on eps.
