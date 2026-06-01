# Reuse critique -- 012.slice-md cycle 3

## Coverage

- Helper roots walked: `C:\GitHub\Berreman` — `Berreman/Analytics/` (Charting.fs,
  Variables.fs, Colorimetry.fs, FieldProfile.fs), `Berreman/Berreman/` (Fields.fs,
  FieldFunctions.fs), the slice's seven new `OpticalConstructor.Ui/Charts/*.fs`, and
  the four new `OpticalConstructor.Tests/*.fs`.
- Files inspected: ~22/200 (focused on the diff's additions and the engine seams the
  slice spec names as the reuse targets); cap not reached.
- Extensions: the task bound listed `.py,.md,.json` — a stale cross-repo default that
  matches **no** source in this pure-F# tree. I walked `.fs` (the only defensible
  evidence base) plus the spec/state `.md`.

## Findings

Note on the prior cycle: cycle-2 **F2** (the choose-on-`Some` projection inlined twice
inside `SeriesData`) is **resolved** — attempt 03 folded both call sites into one
private `projectPoints` (`SeriesData.fs:43-46`), consumed by `series1D` and
`seriesComparison`. The remaining findings are near-misses where the "existing helper"
is a non-exported nested local that the engine itself already duplicates.

### F1: `SeriesData.projectPoints` is a third copy of the engine's nested `getFuncData`

- **Worker added:** `projectPoints` (`SeriesData.fs:43-46`) — `Array.map (fun (v, s) -> (v, s.func fn)) |> Array.choose (...Some -> ...)`.
- **Existing helper:** the identical choose-on-`Some` projection already exists
  **twice** in the engine, both as nested locals named `getFuncData`: inside
  `Charting.plot` (`Analytics/Charting.fs:26-30`) and inside `Charting.plotComparison`
  (`Analytics/Charting.fs:43-46`).
- **Why it matters:** §H.2 correctly tells Part H to *mirror* this pattern (it cites
  `Charting.fs:26-30`), and the worker has now collapsed its own two copies into one
  (the cycle-2 F2 fix). The residual issue is upstream: the engine carries the
  projection as a non-exported nested local, so it cannot be called, and the codebase
  now holds three byte-for-byte copies of the same 3-line projection. A future change
  to it (NaN handling, a new `Solution.func` contract) must be made in three places.
- **Suggested action:** advisory — lift `getFuncData` to a module-level
  `Analytics.Charting` helper (e.g. `funcData : (float * Solution)[] -> OpticalFunction -> (float * float)[]`)
  and have `plot`, `plotComparison`, and `SeriesData.projectPoints` all call it. This
  is an engine-side extraction in `Charting.fs` the worker cannot do without widening
  scope; leaving Part H's copy as-is is defensible since the engine local is
  unreachable. The judge decides whether the extraction belongs in this slice.

### F2: the nine dispersion `(name, calc)` pairs are duplicated verbatim from `Charting`

- **Worker added:** `plotN11Series`…`plotRho33Series` (`SeriesData.fs:103-111`), each
  binding a label literal to a `calculate*` accessor: `"Re[e11]" calculateN11Re`,
  `"Im[e11]" calculateXi11Im`, …, `"rho33" calculateRho33Im`.
- **Existing helper:** `Charting.plotN11`…`plotRho33` (`Analytics/Charting.fs:95-107`)
  declare the **same nine** `(name, calc)` pairings (`plotDispersion calculateN11Re "Re[e11]"`, …).
- **Why it matters:** the worker correctly reused the *accessors* — the load-bearing
  part of §H.11 — but the engine `plot*` functions can't be called (they terminate in
  `Chart.show`, `Charting.fs:92`, which §H.11 forbids), so re-binding `calc` is
  justified. The nine legend strings, however, are now authored in two files and must
  be kept in lockstep by hand; a relabel in the engine silently diverges the app's
  chart legends from the engine's.
- **Suggested action:** advisory/low — acceptable to leave (labels are cosmetic and
  the engine exposes them only as inline literals, not a shared constant). If a single
  source is ever wanted, a `(name, calc)` table belongs in `Charting`, not
  `SeriesData`, and is outside this slice's reuse mandate.

### F3: `Plot3DView.surfaceChart` re-expresses `plot3D`'s surface construction

- **Worker added:** `surfaceChart` (`Plot3DView.fs:20-25`) — `Chart.Surface(s.z, s.yValues, s.xValues, Opacity = 0.7, Contours = ...initXyz (Show = true), Name = …) |> withXAxisStyle yVarName |> withYAxisStyle xVarName |> withZAxisStyle`.
- **Existing helper:** the same construction (same `Opacity = 0.7`, same
  `Contours.initXyz(Show = true)`, same `kk:20180922` X/Y swap) lives in `plot3D`'s
  nested `plotFun` (`Analytics/Charting.fs:74-78`).
- **Why it matters:** the surface recipe — the swap convention and the `0.7`/contour
  magic numbers in particular — now lives in two files; if the engine's swap or
  contour default changes, `surfaceChart` silently drifts. This is the single largest
  verbatim copy in the diff.
- **Suggested action:** leave as-is and note the drift point. `plot3D` is genuinely
  *not* callable here — it takes a `FixedInfo`, runs `calculate3D` internally
  (re-solving, forbidden by §H.2/§H.5), returns a `list`, and bundles
  `withDescription`. §H.5 explicitly directs Part H to "reuse the `Chart.Surface`
  construction and the axis-swap convention documented in `plot3D`," i.e. copy the
  recipe rather than call the function. Spec-sanctioned; lowest priority.

### F4: `CieView.spectralLocus` duplicates data derivable from `Colorimetry.cie1931`

- **Worker added:** `spectralLocus` (`CieView.fs:24-59`) — a 33-row static `(x, y)`
  chromaticity literal, 380–700 nm at 10 nm.
- **Existing helper:** `Colorimetry.cie1931` (`Analytics/Colorimetry.fs:22`) — the CIE
  1931 2° colour-matching functions `(λ, x̄, ȳ, z̄)` at 10 nm. The spectral-locus
  chromaticity at each λ is the normalized CMF triple (`x = x̄/(x̄+ȳ+z̄)`,
  `y = ȳ/(x̄+ȳ+z̄)`), so the locus is derivable from that table.
- **Why it matters:** the horseshoe boundary and the CMF table encode the same CIE
  1931 reference data; a literal locus is a second source of truth.
- **Suggested action:** **leave as-is** — informational only. R-10 *explicitly
  mandates* the literal locus in `CieView.fs` ("MUST live as a literal array … the
  only static numeric table Part H introduces"), and `cie1931` is `private` to
  `Colorimetry`, so reuse is neither permitted by the spec nor reachable. Recorded so
  the judge knows the relationship; if the table is ever promoted, derive the locus
  from `Colorimetry.cie1931` rather than maintaining two CIE tables.

### Considered and dropped

- **`PolarizationPlots.poincarePoint` Stokes normalization** (`PolarizationPlots.fs:49-53`):
  divides `(S1,S2,S3)/S0` from a `StokesVector`. `StokesVector` (`Fields.fs:580-587`)
  exposes only `create`/`(+)`/`Zero` — no normalization member to reuse — and §H.8
  forbids recomputing Stokes *parameters*, not normalizing the existing vector. No
  helper to cite; not a duplication.
- **`CieView.ChromaticityPoint { x; y; rgb }`** (`CieView.fs:15-20`): `Colorimetry`
  returns bare tuples, not this record, so minting it at the chart boundary duplicates
  nothing.
- **`Readout.peak`/`minimum`/`fwhm`/`nearestPoint`** (`Readout.fs`): no exported
  peak/FWHM/nearest helper exists to reuse; §H.7 mandates these be pure array scans
  (not ScottPlot's `GetNearest`). Not duplication.
- **Tests:** `SeriesDataTests` reuses the `StandardSystems.transparentGlass` fixture
  and re-derives the engine projection only as the test *oracle* — correct test
  practice, not reinvention.

## Bottom line

The diff reuses the engine seams thoroughly — `calculate`/`calculate3D`/`mapFun`, the
nine `calculate*` dispersion accessors, §F.6 `fieldDepthProfile`, §F.8 `Colorimetry`,
the `OpticalFunction` DU, `StokesVector`/`ellipticityR`/`azimuthR` members, the §A.10
`Units.fromMeters` boundary, and the `StandardSystems` fixtures — and forks none of the
solver, fields, tensor, or units primitives; cycle-2 F2 is fixed. The four remaining
findings are near-misses where the cited helper is a non-exported nested local the
engine already duplicates (F1/F2/F3) or is spec-mandated and unreachable (F4). In my
read these are cleanup-grade and do not warrant a re-spawn, but the code judge holds
that decision.
