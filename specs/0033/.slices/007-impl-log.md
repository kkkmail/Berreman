# Slice 007 — impl log

## Progress

- [x] Read the worker system prompt (implement_worker + arc-runner base), project prompt (Operator note: empty), slice spec, gates snapshot, spec-md Part B, and the current `Dispersion.fs` / `MatrixComparison.fs` / `Dispersive.fs` / `Fields.fs` (WaveLength).
- [x] Impl-plan written.
- [x] Red: `DispersionFormulaTests.fs` written against the not-yet-existing formula types + fsproj entry — build failed with `FS0039: The type 'DispersionFormula' is not defined` (and siblings) — the missing-production-symbol red the worker base names (log `007-red-tdd.log`).
- [x] Green: the five formula blocks appended to `Berreman/Berreman/Dispersion.fs` (engine unions untouched); solution builds clean; BerremanTests 89 passed / 0 failed (`007-unit-tests.log`).
- [x] Remaining suites run locally (ADVISORY — the arc-runner gate engine is the sole gate authority and re-runs after exit); all green; logs in `.artifacts/`.
- [x] Line endings verified: no CRLF churn (`--numstat` = `--ignore-cr-at-eol`); the touched `Dispersion.fs` working copy normalized to LF (BOM preserved); the new test file is LF.
- [x] State-of-the-world written.

## Files modified

- `specs/0033/.slices/007-impl-plan.md` (new)
- `specs/0033/.slices/007-impl-log.md` (new, this file)
- `Berreman/Berreman/Dispersion.fs` — appended after the engine unions (which are byte-identical, spec §0.1): `WaveLengthInterval { lower : WaveLength; upper : WaveLength }` (pure data, no validation/clamping per §0.4); `DispersionTerm { lambda : double; coefficients : double array; power : int; multiplier : double }` with `member evaluate : double -> double` = `multiplier * (Σ_k coefficients.[k] * (x − lambda)^k) ^ power` (Horner inner polynomial, `pown` outer power — negative powers give the inverse/Laurent shapes); `DispersionFormula { terms : DispersionTerm list; wavelengthScale : double }` with `member evaluate : WaveLength -> double` reducing the canonical wavelength (metres, `WaveLength.value`) by `wavelengthScale` (metres per coefficient unit) and summing the terms; `ComplexDispersionTerm` / `ComplexDispersionFormula` mirroring them over `System.Numerics.Complex` with `evaluate : WaveLength -> Complex`; private `complexPown` helper (repeated multiplication; `pown` does not resolve against `Complex` — fsi-verified). New opens: `System.Numerics` (Complex), `Constants` (the `meter` measure used by the reduction).
- `Berreman/BerremanTests/DispersionFormulaTests.fs` (new) — five facts, every expected value computed inline from the direct mathematical form (independent of the term/Horner/pown path under test): Cauchy-shaped Laurent polynomial `A + B/x² + C/x⁴` (µm coefficients, 500 nm); Sellmeier-shaped inverse term `1 + B·x²/(x² − C)` via the partial fraction `(1 + B) + B·C·(x² − C)^(−1)` (BK7 first-term coefficients, 587.6 nm) — cross-checks the algebra against the direct rational form; a shifted-centre squared term pinning `(x − lambda)` centring and a positive outer power; the complex mirror on a Lorentz-shaped pole `1 + S/(x₀² − x² − iγx)` compared by `.Magnitude`; `WaveLengthInterval` carrying its elevated endpoints. All comparisons use `MatrixComparison.allowedDiff` (the `MuellerMatrixTests.fs:24` / `OptimizationTests.fs:260` precedent) — no new epsilon logic.
- `Berreman/BerremanTests/BerremanTests.fsproj` — one `<Compile Include="DispersionFormulaTests.fs" />` entry (before `OptimizationTests.fs`).

## Decisions

- **The slice's pinned raw-`double` fields win over a per-field elevation.** CLAUDE.md
  elevates every primitive, but the slice pins `DispersionTerm (lambda : double,
  coefficients : double array, power : int, multiplier : double)` and
  `wavelengthScale : double` explicitly — these are dimensionless model coefficients
  whose unit is DEFINED by the sibling `wavelengthScale`; the named record is the
  elevation. `evaluate` keeps the elevated `WaveLength` argument, and
  `WaveLengthInterval` keeps elevated `WaveLength` endpoints.
- **Complex mirror is field-for-field.** The value fields (`lambda`, `coefficients`,
  `multiplier`) become `Complex`; `power : int` (an exponent) and
  `wavelengthScale : double` (a unit scale) stay real. Complex coefficients alone
  would suffice mathematically, but the full mirror keeps the two shapes symmetric
  as the slice words it ("mirroring them over System.Numerics.Complex").
- **`pown` for the real outer power, `complexPown` for the complex one.**
  `pown` supports negative integer exponents on `float`, but does NOT resolve
  against `Complex` (verified in fsi: `error FS0001: The type 'Complex' does not
  support the operator 'pown'`), so the complex mirror uses a private
  repeated-multiplication helper (fold, no mutation) that inverts on a negative
  exponent — exact for integer powers, unlike `Complex.Pow`'s exp/log branch.
- **Complex summation by explicit fold over `Complex.Zero`** — `List.sumBy` needs
  the `Zero` static-member trait, which `Complex` exposes only as a static field /
  static-abstract interface member; the fold sidesteps trait-resolution risk under
  `--warnaserror+:25`.
- **Placement: appended at the end of the `Dispersion` module** with a section
  banner — not interleaved — so the engine unions stay byte-identical (§0.1) and
  the later Part B `…Value` types can follow the blocks they consume.
- **No `contains` / ordering helper on `WaveLengthInterval`** — "an interval is
  just an interval"; §0.4 forbids segment validation/clamping, and the covering /
  extrapolation rules belong to the later Part B consumer steps.
- **Poles are not guarded**: `pown 0.0 -1` / `complexPown Complex.Zero -1` yield
  infinity — intentional (§0.4: no clamping; a formula pole is a pole).
- **Sellmeier decomposition recorded**: a single term is `multiplier · (poly)^power`,
  so `B·x²/(x² − C)` is expressed exactly as `(1 + B) + B·C·(x² − C)^(−1)` — the
  partial fraction is the intended term-form for Sellmeier-shaped models, and the
  test pins it against the direct rational form.

## Testing state

TDD sequence: red first (`FS0039` on `DispersionFormula` / record labels — the
missing-production-symbol red the worker base names, `007-red-tdd.log`), then the
implementation, then green. Local runs are ADVISORY — the arc-runner gate engine
is the sole gate authority and re-runs the roster after exit:

- `build` — `dotnet build Berreman.slnx -c Release -nologo -v:m` (cwd `Berreman/`):
  exit 0, `Build succeeded.`, 0 occurrences of "error" in the log (the gate regex).
- `unit-tests` (BerremanTests, `--no-build`): 89 passed, 5 skipped (pre-existing
  skips), 0 failed — +5 over the 84 baseline.
- `constructor-unit-tests`: 350 passed, 0 failed (= baseline; nothing in the
  OpticalConstructor projects was touched).
- `ui-smoke`: 54 passed, 0 failed (= baseline).
- `ui-tests`: 249 passed, 0 failed (= baseline).

No CRLF churn: `git diff --numstat` equals `--ignore-cr-at-eol`; the touched
`Dispersion.fs` working copy normalized to LF (BOM preserved, index already LF);
the new test file is LF; the fsproj stayed LF.

## Artifacts

All in `specs/0033/.artifacts/`:

- `007-red-tdd.log` — the failing (red) build naming the missing production symbols.
- `007-build.log` — the build-gate command output (green).
- `007-unit-tests.log` — BerremanTests run (89 passed).
- `007-constructor-unit-tests.log` — 350 passed.
- `007-ui-smoke.log` / `007-ui-tests.log` — the two headless UI gate runs.

## Gotchas

- **`pown` does not resolve against `System.Numerics.Complex`** (fsi-verified
  `FS0001`) — `Complex.One`/`Complex.Zero` are static FIELDS, not the properties
  the SRTP trait wants. Hence the private `complexPown` and the explicit fold in
  `ComplexDispersionFormula.evaluate`. Don't "simplify" these back to
  `pown`/`sumBy` — the build will break.
- **Record-literal inference needs the outer annotation.** `DispersionTerm` /
  `ComplexDispersionTerm` (and the two formula types) share identical field
  NAMES; a bare `{ terms = …; wavelengthScale = … }` literal infers the
  last-declared type. Annotate the binding (`let f : DispersionFormula = …`) as
  the tests do.
- **`wavelengthScale` is metres per coefficient unit** (e.g. `1.0e-6` for µm-
  tabulated coefficients) — it DIVIDES the canonical metre-valued wavelength.
  Getting this inverted is silent (values just come out absurd); the tests pin
  the direction with nm-created wavelengths against µm coefficients.
- The engine `…WithDisp` unions above the new block are byte-identical (§0.1) —
  the file diff is pure addition (93 added / 0 deleted) plus two `open`s.
- Working-copy line endings in this checkout are CRLF for several tracked files
  (`git ls-files --eol` shows `i/lf w/crlf` even on files this slice never
  touched, e.g. `MatrixComparison.fs`); the index is LF everywhere and diffs are
  normalization-invisible. The touched `Dispersion.fs` was converted to `w/lf`;
  untouched files were left alone.
- `.manifest.state.json` shows as modified in `git status` — the arc-runner's own
  file (same as slices 001–006), left alone.
- This step is plain IMPLEMENT (no contract id) — no `.contracts-json` change.
