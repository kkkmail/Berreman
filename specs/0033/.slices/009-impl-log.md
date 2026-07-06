# Slice 009 — impl log

## Progress

- [x] Read the worker system prompt (implement_worker + arc-runner base), project prompt (Operator note: empty), slice spec, gates snapshot, spec-md Part B, and the current `Dispersion.fs` / `MaterialProperties.fs` / `Active.fs` / `MatrixComparison.fs` / slice-008 outputs.
- [x] Impl-plan written.
- [x] Red: `RhoWithDispValueTests.fs` written against the not-yet-existing rho tree (+ fsproj entry, + `verifyMatrixEqualityRho` in `MatrixComparison.fs`) — build failed with `FS0039: The type 'RhoWithDispValue' is not defined` / `'Handedness'` / `'GyrationClass'` / `'UniaxialGyration'` / `'Orthorhombic222Gyration'` / `'Triclinic1Gyration'` and the case constructors — the missing-production-symbol red the worker base names (log `009-red-tdd.log`).
- [x] Green: the rho (gyration) tree appended to `Berreman/Berreman/Dispersion.fs` (engine unions, slice-007 formula blocks and slice-008 eps tree untouched — pure addition, 151 added / 0 deleted); four new crystal-class builders + private `assembleRho` + the `toRhoWithDisp` type extension added to `Berreman/OpticalProperties/Active.fs` (64 added / 0 deleted); solution builds clean; BerremanTests 111 passed / 0 failed (`009-unit-tests.log`).
- [x] Remaining suites run locally (ADVISORY — the arc-runner gate engine is the sole gate authority and re-runs after exit); all green; logs in `.artifacts/`.
- [x] Line endings verified: `git diff --numstat` equals `--ignore-cr-at-eol`; `Dispersion.fs` stays `w/lf` with its BOM preserved; `Active.fs` and `MatrixComparison.fs` were CRLF in the WORKING COPY (index was already LF — pre-existing editor drift, not this round's churn) and were converted back to whole-file LF with their BOMs preserved; the new test file is LF UTF-8 (no BOM); the fsproj stays LF.
- [x] State-of-the-world written.

## Files modified

- `specs/0033/.slices/009-impl-plan.md` (new)
- `specs/0033/.slices/009-impl-log.md` (new, this file)
- `Berreman/Berreman/Dispersion.fs` — the serializable rho (gyration) tree
  appended after the slice-008 eps tree: `Handedness = LeftHanded | RightHanded`
  with `member sign : double` (RightHanded → +1.0, LeftHanded → −1.0 — the
  enantiomorph is ONE overall sign flip of g); the five NAMED generic gyration
  records, each with a `map` combinator — `UniaxialGyration<'g>` (g11, g33),
  `Orthorhombic222Gyration<'g>` (g11, g22, g33), `Monoclinic2Gyration<'g>`
  (g11, g22, g33, g13), `MonoclinicMGyration<'g>` (g12, g23),
  `Triclinic1Gyration<'g>` (g11, g22, g33, g23, g13, g12) — never anonymous
  tuples; `GyrationClass<'g>` = `CubicActive of 'g | UniaxialActive of
  UniaxialGyration<'g> | PlanarActive of 'g | Orthorhombic222 of
  Orthorhombic222Gyration<'g> | Monoclinic2 of Monoclinic2Gyration<'g> |
  MonoclinicM of MonoclinicMGyration<'g> | Triclinic1 of
  Triclinic1Gyration<'g>` with a generic `map`; `GyrotropicValue<'g>`
  (`gyration : GyrationClass<'g>`, `hand : Handedness`); `RhoWithDispValue =
  RhoWithDispValue of GyrotropicValue<DispersionFormula> | RhoWithoutDispValue
  of GyrotropicValue<RhoValue>`. No new opens needed.
- `Berreman/OpticalProperties/Active.fs` — `open Berreman.Dispersion`; four new
  crystal-class builders inside the existing `type Rho with` block, all via
  `Rho.fromIm`: `type_222_Crystal g11 g22 g33` (diag), `type_2_Crystal g11 g22
  g33 g13` (diagonal + g13 = g31, two-fold axis along x2), `type_m_Crystal g12
  g23` (g12 = g21, g23 = g32, mirror normal to x2), `type_1_Crystal g11 g22 g33
  g23 g13 g12` (full symmetric tensor); private `assembleRho (hand :
  Handedness) (gyration : GyrationClass<RhoValue>) : Rho` applying `hand.sign`
  to every component then routing each class to its builder (CubicActive →
  `cubicCrystal`; UniaxialActive → the diagonal `type_3_4_6_Crystal`;
  PlanarActive → `planarCrystal`; the four new classes → the four new
  builders); `type RhoWithDispValue with member toRhoWithDisp : RhoWithDisp` —
  constant case short-circuits to `RhoWithoutDisp`, dispersive case wraps a
  `WaveLength -> Rho` closure evaluating each component's `DispersionFormula`
  at the wavelength before assembly.
- `Berreman/BerremanTests/MatrixComparison.fs` — one line:
  `verifyMatrixEqualityRho`, mirroring `verifyMatrixEqualityEps` (delegates to
  the existing relative-norm `verifyMatrixEquality` — no new epsilon logic).
- `Berreman/BerremanTests/RhoWithDispValueTests.fs` (new) — 11 facts: the
  quartz class-32 example (g11 = +5.9e-5, g33 = −10.1e-5) assembles
  diag(g11, g11, g33); cubic / planar / 222 / monoclinic-2 / monoclinic-m /
  triclinic-1 each assemble the expected imaginary gyration matrix (expected
  matrices built via `Rho.fromIm` from raw literals, independently of the
  builders under test); LeftHanded negates the quartz tensor and every slot of
  the triclinic tensor; the dispersive case evaluates each component's formula
  at the wavelength (uniaxial: linear g11(x) = 2.0e-5 + 1.0e-5·x, constant
  g33 = −1.5e-5, at 500 nm → diag(2.5e-5, 2.5e-5, −1.5e-5)); a dispersive
  LeftHanded cubic negates the evaluated tensor.
- `Berreman/BerremanTests/BerremanTests.fsproj` — one
  `<Compile Include="RhoWithDispValueTests.fs" />` entry (after
  `EpsWithDispValueTests.fs`, before `OptimizationTests.fs`).

## Decisions

- **The compressed slice DU text is read as "cases over those records"**: the
  slice writes `… | Orthorhombic222 | Monoclinic2 | MonoclinicM | Triclinic1
  over those records`; the four bare-looking cases carry their matching named
  records (`Orthorhombic222 of Orthorhombic222Gyration<'g>`, etc.) — payloadless
  cases could not "assemble the expected imaginary gyration matrix" and the
  spec-md pins "GyrationClass<'g> with named multi-component records".
- **RightHanded is the identity, LeftHanded the sign flip.** The slice pins only
  "flipping Handedness negates the tensor"; keeping the as-specified components
  on RightHanded matches the quartz example being quoted with its literature
  signs. `Handedness.sign : double` (+1.0 / −1.0) is the single seam.
- **Sign is applied per component BEFORE assembly** (`gyration.map signed`) —
  `Rho` has no scalar multiplication, and one overall flip of every component
  equals one overall flip of the assembled linear matrix.
- **UniaxialActive routes through `type_3_4_6_Crystal`** (diagonal), NOT
  `type_32_42_62_Crystal` — the latter needs a g12 the symmetric two-component
  uniaxial record cannot supply (pinned by the slice).
- **Monoclinic-2 matrix shape** (two-fold axis along x2): diagonal + g13 = g31;
  **monoclinic-m** (mirror normal to x2): only g12 = g21 and g23 = g32 — the
  standard gyration pseudo-tensor forms for those point groups (spec-md
  reference arXiv:2501.03684); the new builders emit symmetric matrices, unlike
  the antisymmetric `planarCrystal`.
- **New builder naming follows the existing `type_…_Crystal` convention**:
  `type_222_Crystal`, `type_2_Crystal`, `type_m_Crystal`, `type_1_Crystal`.
- **Each record carries its own `map` member** (return type annotated) instead
  of one inline mapping in `GyrationClass.map` — the records share field labels
  (g11/g22/g33/g23/g13/g12 subsets), and construction inside the owning type
  with an annotated return type is immune to last-declared-record inference.
- **`toRhoWithDisp` is a property** (no arguments), matching slice-008's
  `toEpsWithDisp`; the dispersive case builds the tensor per call inside the
  `WaveLength -> Rho` closure — no caching, mirroring the eps tree.
- **`assembleRho` is `let private`** in the `Active` module — it is the single
  assembly seam shared by both `toRhoWithDisp` cases and not part of the
  public surface.

## Testing state

TDD sequence: red first (`FS0039` on `RhoWithDispValue` / `Handedness` /
`GyrationClass` / the gyration records and case constructors — the
missing-production-symbol red the worker base names, `009-red-tdd.log`), then
the implementation, then green. Local runs are ADVISORY — the arc-runner gate
engine is the sole gate authority and re-runs the roster after exit:

- `build` — `dotnet build Berreman.slnx -c Release -nologo -v:m` (cwd
  `Berreman/`): exit 0, 0 occurrences of "error" in the log (the gate regex);
  only pre-existing NU19xx package-vulnerability warnings, none from touched
  files.
- `unit-tests` (BerremanTests, `--no-build`): 111 passed, 5 skipped
  (pre-existing skips), 0 failed — +11 over the 100 baseline.
- `constructor-unit-tests`: 350 passed, 0 failed (= baseline; nothing in the
  OpticalConstructor projects was touched).
- `ui-smoke`: 54 passed, 0 failed (= baseline).
- `ui-tests`: 249 passed, 0 failed (= baseline).

No CRLF churn: `git diff --numstat` equals `--ignore-cr-at-eol`; all four
tracked touched files end this round `i/lf w/lf`; `Dispersion.fs` and the two
converted files keep their BOMs; the new test file is LF UTF-8 without BOM.

## Artifacts

All in `specs/0033/.artifacts/` (ANSI escapes stripped):

- `009-red-tdd.log` — the failing (red) build naming the missing production symbols (25 "error" occurrences).
- `009-build.log` — the build-gate command output (green, 0 "error" occurrences).
- `009-unit-tests.log` — BerremanTests run (111 passed).
- `009-constructor-unit-tests.log` — 350 passed.
- `009-ui-smoke.log` / `009-ui-tests.log` — the two headless UI gate runs (54 / 249 passed).

## Gotchas

- **The gyration records share field labels** — a bare record literal with
  `{ g11 = …; g33 = … }` infers the LAST-declared record carrying those labels
  (`Triclinic1Gyration`) and then fails on missing fields; annotate every
  construction site (`let quartz : UniaxialGyration<RhoValue> = …`) as the
  tests do. The records' own `map` members are safe (annotated return types).
- **Negative literals after a constructor or function need parens** —
  `RhoValue -10.1e-5` and `quartzExpected -1.0` parse as subtraction; write
  `RhoValue (-10.1e-5)` / `quartzExpected (-1.0)`.
- **`Active.fs` and `MatrixComparison.fs` sat as CRLF in the working copy
  BEFORE this round** (index was LF; `.gitattributes` normalization hid it from
  `git status`) — they were converted back to whole-file LF (BOMs preserved),
  which produces zero diff noise since the stored content is LF either way.
  `git ls-files --eol` is the tool that exposes this; `git status` will not.
- **`planarCrystal` is antisymmetric, the four new builders are symmetric** —
  the planar (uniaxial optically-active film) form keeps the existing engine
  convention `[[0, g12, 0]; [−g12, 0, 0]; [0, 0, 0]]`; do not "fix" it to match
  the symmetric monoclinic-m shape.
- **Handedness lives in the CORE (`Dispersion.fs`), assembly in
  `OpticalProperties/Active.fs`** — the core cannot reference
  `OpticalProperties`, so `toRhoWithDisp` (and any future consumer of the
  crystal-class builders) must stay in `Active.fs`; a core-side `toRho` would
  not compile without duplicating the builders.
- The engine `…WithDisp` unions, the slice-007 formula blocks and the slice-008
  eps tree are byte-identical — the `Dispersion.fs` diff is pure addition
  (151 added / 0 deleted), no new `open`s; `Active.fs` is pure addition
  (64 added / 0 deleted) plus one new `open Berreman.Dispersion`.
- `.manifest.state.json` shows as modified in `git status` — the arc-runner's
  own file (same as slices 001–008), left alone.
- This step is plain IMPLEMENT (no contract id) — no `.contracts-json` change.
