# Impl plan — spec 0033, slice 010 (IMPLEMENT)

## Goal

Add the serializable mu (Polder / gyromagnetic) tree to
`Berreman/Berreman/Dispersion.fs` — the mu counterpart of the slice-008 eps
tree and slice-009 rho tree — plus BerremanTests coverage.

## Approach

Append a new section to `Dispersion.fs` after the rho tree (pure addition,
engine unions untouched):

1. `GyrationAxis = AlongX | AlongY | AlongZ` with
   `static member defaultValue = AlongZ` (the Faraday geometry; the spec says
   "default AlongZ", so the default gets a named home a serializer/editor can
   reference; transverse axes = Voigt).
2. `PolderValue<'g>` — ONE generic record
   (`muDiagonal : 'g`, `muParallel : 'g`, `gyration : 'g`,
   `axis : GyrationAxis`) with a `map` combinator, reused by both the constant
   (`'g = MuValue`) and dispersive (`'g = DispersionFormula`) cases — the
   slice-009 pattern (dispersive evaluation is one `map`, not a second copy of
   the assembly).
3. Private `polderMu : PolderValue<MuValue> -> Mu` — the single place the
   Polder tensor is written down, through `Mu.create`
   (MaterialProperties.fs:112). AlongZ rows: `[mu, +i·g, 0]`, `[-i·g, mu, 0]`,
   `[0, 0, muParallel]`; AlongX / AlongY are the cyclic permutations of that
   tensor (equivalently `mu_jk = i·g·ε_jkl·n_l` off the diagonal), so the
   ±i·g pair keeps its right-handed sense about the axis:
   - AlongX rows: `[muParallel, 0, 0]`, `[0, mu, +i·g]`, `[0, -i·g, mu]`
   - AlongY rows: `[mu, 0, -i·g]`, `[0, muParallel, 0]`, `[+i·g, 0, mu]`
4. `ConstantMuValue = ScalarMu of MuValue | GyromagneticMu of PolderValue<MuValue>`
   with `toMu` — ScalarMu builds mu × `ComplexMatrix3x3.identity` (the
   `Eps.fromRefractionIndex` precedent); GyromagneticMu routes through
   `polderMu`.
5. `MuWithDispValue = MuWithDispValue of PolderValue<DispersionFormula> | MuWithoutDispValue of ConstantMuValue`
   with `member toMuWithDisp : MuWithDisp` — the constant cases short-circuit
   to `MuWithoutDisp`; the dispersive case wraps a `WaveLength -> Mu` closure
   that maps each formula through `evaluate w |> MuValue` and assembles via
   `polderMu`.

No solver work — the Berreman matrix already reads off-diagonal mu.

## Tests (TDD — red first)

New `Berreman/BerremanTests/MuWithDispValueTests.fs` (added to the fsproj
after RhoWithDispValueTests.fs), plus a one-line `verifyMatrixEqualityMu`
helper in `MatrixComparison.fs` (Mu variant of the existing Eps/Rho helpers):

- each axis produces the correctly permuted tensor — three facts pinning the
  assembled Mu against `Mu.create` literals built independently of the
  assembly (distinct mu / muParallel / g magnitudes so a wrong permutation
  cannot alias a right one);
- ScalarMu equals the scaled identity;
- a dispersive Polder value evaluated at a wavelength equals the constant
  assembly with the same magnitudes (linear formulas over reduced µm
  wavelength, the slice-009 shape);
- the constant cases short-circuit structurally to `MuWithoutDisp`;
- `GyrationAxis.defaultValue = AlongZ`.

Red = FS0039 (production symbols missing) on the test project build, captured
to `.artifacts/010-red-tdd.log` — the slice-009 procedure.

## Files to modify

- `Berreman/Berreman/Dispersion.fs` — append the mu tree (pure addition).
- `Berreman/BerremanTests/MuWithDispValueTests.fs` — new test file.
- `Berreman/BerremanTests/MatrixComparison.fs` — add `verifyMatrixEqualityMu`.
- `Berreman/BerremanTests/BerremanTests.fsproj` — register the new file.

## Risks

- **Match-expression pipe precedence**: `match … | Case -> [rows] |> Mu.create`
  binds the pipe to the last branch only — bind the rows to a `let` first.
- **AlongY sign convention**: must be the cyclic permutation
  (`mu_xz = -i·g`, `mu_zx = +i·g`), not a naive copy of the AlongZ block.
- **Record field-label overlap** (`gyration` also lives in
  `GyrotropicValue<'g>`): PolderValue construction sites carry all four fields
  (unique), but test sites get explicit type annotations anyway (slice-009
  gotcha).
- **Negative literals after a function need parens** (`im (-g)`).
- **CRLF churn**: verify LF endings on every touched file after editing.
