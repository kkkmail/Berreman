# Impl log — spec 0033, slice 010 (IMPLEMENT)

## Progress

- [x] Read task file, system prompt (`implement_worker.system-md` +
  `arc-runner.system-md` base), project prompt, slice spec, gates snapshot.
  Operator note in the project prompt is empty (no reply in flight).
- [x] Read `Dispersion.fs` (eps + rho trees), `MaterialProperties.fs`
  (MuValue:101, Mu.create:112), `Active.fs` (slice-009 assembleRho pattern),
  `MatrixComparison.fs`, `RhoWithDispValueTests.fs`, `BerremanTests.fsproj`.
- [x] Impl-plan written (`010-impl-plan.md`).
- [x] TDD red: `MuWithDispValueTests.fs` (8 facts) + fsproj registration +
  `verifyMatrixEqualityMu` helper; test-project build fails FS0039 on
  `GyrationAxis` / `PolderValue` / `MuWithDispValue` (the production symbols
  do not exist) — captured to `.artifacts/010-red-tdd.log`.
- [x] Implement the mu tree in `Dispersion.fs` (appended after the rho tree;
  pure addition, +111 lines, engine unions byte-identical).
- [x] Local ADVISORY verification (build + 5 suites) — all green, logs in
  `.artifacts/010-*.log`. The arc-runner gate engine re-runs the roster
  authoritatively after exit.
- [x] LF audit: touched tracked files all `i/lf w/lf`; `git diff --numstat`
  identical with and without `--ignore-cr-at-eol` (no CRLF churn); the new
  test file is CR=0.
- [x] State-of-the-world written; this log finalized.

## Files modified

- `Berreman/Berreman/Dispersion.fs` — appended the serializable mu (Polder /
  gyromagnetic) tree: `GyrationAxis` (AlongX | AlongY | AlongZ,
  `defaultValue = AlongZ`), generic `PolderValue<'g>` record with `map`,
  private `polderMu : PolderValue<MuValue> -> Mu` (the single place the
  Polder tensor is written down, via `Mu.create`), `ConstantMuValue`
  (ScalarMu | GyromagneticMu) with `toMu`, and `MuWithDispValue`
  (`MuWithDispValue of PolderValue<DispersionFormula>` |
  `MuWithoutDispValue of ConstantMuValue`) with `toMuWithDisp : MuWithDisp`.
- `Berreman/BerremanTests/MuWithDispValueTests.fs` — NEW: 8 facts (three
  axis-permutation pins against independent `Mu.create` literals, scalar
  identity, structural short-circuit both ways, default axis, dispersive =
  constant assembly at the evaluated wavelength).
- `Berreman/BerremanTests/MatrixComparison.fs` — one-line
  `verifyMatrixEqualityMu` (the Mu variant of the existing Eps/Rho helpers).
- `Berreman/BerremanTests/BerremanTests.fsproj` — registered the new test
  file after `RhoWithDispValueTests.fs`.

## Decisions

- **Assembly lives in `Dispersion.fs`** (unlike rho, which needed the
  crystal-class builders in `OpticalProperties/Active.fs`): `Mu.create` and
  `MuValue` are in `MaterialProperties`, already visible to the core, so the
  spec's instruction to put the whole tree incl. `toMuWithDisp` in
  Dispersion.fs is directly implementable.
- **Axis permutation is the cyclic one** (off-diagonal
  `mu_jk = i·g·ε_jkl·n_l`): AlongX → muParallel at (1,1), +i·g at (2,3);
  AlongY → muParallel at (2,2), +i·g at (3,1) and −i·g at (1,3). This keeps
  the gyration right-handed about the magnetization axis for all three cases
  (the standard Polder convention); a naive block copy for AlongY would flip
  the sense.
- **`GyrationAxis.defaultValue = AlongZ`** — the spec's "(default AlongZ)"
  gets a named static member so later serialization/UI slices reference the
  default instead of re-hardcoding it; a test pins it.
- **`polderMu` takes `PolderValue<MuValue>`** — components stay elevated
  through the assembly seam; the dispersive case is one
  `map (evaluate w |> MuValue)`, not a second copy of the assembly — mirrors
  slice-009's `assembleRho` exactly.
- **ScalarMu builds `Complex m * ComplexMatrix3x3.identity |> Mu`** — the
  `Eps.fromRefractionIndex` scalar-identity precedent, not a hand-written
  3-row literal.

## Testing state

TDD red → green. All five gates in the slice roster pass in this worker's
local ADVISORY run (the deterministic gate engine is the sole gate
authority and re-runs them after exit):

- `build` — `dotnet build Berreman.slnx -c Release` (cwd `Berreman/`):
  0 errors. 94 warnings, all pre-existing (MSB3277 WindowsBase / NU19xx
  advisories; the only Dispersion.fs warning is the slice-007 FS3873 at
  line 205, untouched this round). No warnings from files touched this round.
- `unit-tests` — `dotnet test --no-build -c Release` (cwd
  `Berreman/BerremanTests/`): **119 passed**, 0 failed, 5 skipped
  (pre-existing skips); +8 over the 111 baseline.
- `constructor-unit-tests` — **350 passed**, 0 failed (= baseline; project
  untouched).
- `ui-smoke` — **54 passed**, 0 failed (= baseline).
- `ui-tests` — **249 passed**, 0 failed (= baseline).

Nothing deferred; every requirement in the slice spec landed this round.

## Artifacts

- `specs/0033/.artifacts/010-red-tdd.log` — TDD red build (FS0039 on the
  missing production symbols).
- `specs/0033/.artifacts/010-build.log` — full solution Release build.
- `specs/0033/.artifacts/010-unit-tests.log` — BerremanTests run (119).
- `specs/0033/.artifacts/010-constructor-unit-tests.log` — 350.
- `specs/0033/.artifacts/010-ui-smoke.log` — 54.
- `specs/0033/.artifacts/010-ui-tests.log` — 249.

## Gotchas

- **Pipe-into-match precedence**: `match … with | Case -> [rows] |> Mu.create`
  would bind the pipe to the LAST branch only — the rows are bound to a `let`
  and piped once, after the match.
- **`im (-gyr)` needs parens in tests** — `im -gyr` parses as binary
  subtraction over the `im` function value.
- **The TDD red log doubles as a field-overlap proof**: before the production
  types existed, the four-field `PolderValue` literals inferred
  `GyrotropicValue` (FS0764 "no assignment for field 'hand'") — the
  slice-009 field-label-overlap gotcha in action; with `PolderValue` declared
  (all four fields unique to it) plus explicit annotations at construction
  sites, inference is unambiguous.
- **`Complex * ComplexMatrix3x3` exists** (Geometry.fs:421) — no need for
  `cplx` (MathNetNumericsMath is not opened in Dispersion.fs; the file's
  precedent is `Complex (x, 0.0)`).
- `.manifest.state.json` shows as modified in `git status` — the
  arc-runner's own file (same as slices 001–009), left alone.
