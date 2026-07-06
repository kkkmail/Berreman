# Slice 009 — impl plan

## Goal

Spec 0033 Part B, rho leg: the serializable gyration tree in
`Berreman/Berreman/Dispersion.fs` (pure data, symmetry-class only — no free
3x3), plus the missing crystal-class `Rho` builders and the
`RhoWithDispValue.toRhoWithDisp` type extension in
`Berreman/OpticalProperties/Active.fs` (core cannot reference
`OpticalProperties`, so assembly lives there). TDD: tests first (red on
missing production symbols), then the implementation, then green.

## Approach

1. **Red** — new `Berreman/BerremanTests/RhoWithDispValueTests.fs` (+ fsproj
   entry, + a `verifyMatrixEqualityRho` helper in `MatrixComparison.fs`
   mirroring `verifyMatrixEqualityEps`) against the not-yet-existing types.
   Build fails with FS0039 on `RhoWithDispValue` / `GyrationClass` /
   `Handedness` / `Rho.type_222_Crystal` etc.; capture to
   `.artifacts/009-red-tdd.log`.
2. **Dispersion.fs** — append (pure addition, engine unions untouched):
   - `Handedness = LeftHanded | RightHanded` with `member sign : double`
     (RightHanded → +1.0, LeftHanded → −1.0): the enantiomorph is ONE overall
     sign flip of g.
   - Named generic records, each with a `map` member (the return-type
     annotation disambiguates the shared field labels):
     `UniaxialGyration<'g>` (g11, g33), `Orthorhombic222Gyration<'g>`
     (g11, g22, g33), `Monoclinic2Gyration<'g>` (g11, g22, g33, g13),
     `MonoclinicMGyration<'g>` (g12, g23), `Triclinic1Gyration<'g>`
     (g11, g22, g33, g23, g13, g12). Never anonymous tuples.
   - `GyrationClass<'g>` = `CubicActive of 'g | UniaxialActive of
     UniaxialGyration<'g> | PlanarActive of 'g | Orthorhombic222 of
     Orthorhombic222Gyration<'g> | Monoclinic2 of Monoclinic2Gyration<'g> |
     MonoclinicM of MonoclinicMGyration<'g> | Triclinic1 of
     Triclinic1Gyration<'g>` with a generic `map` combinator. (The slice text
     compresses the last four cases; "over those records" pins the payloads.)
   - `GyrotropicValue<'g>` = `{ gyration : GyrationClass<'g>; hand :
     Handedness }` — reused by both `RhoWithDispValue` cases.
   - `RhoWithDispValue = RhoWithDispValue of GyrotropicValue<DispersionFormula>
     | RhoWithoutDispValue of GyrotropicValue<RhoValue>` (mirrors
     `EpsWithDispValue`'s case naming).
3. **Active.fs** —
   - `open Berreman.Dispersion`.
   - New builders inside the existing `type Rho with` block (naming follows
     `type_3_4_6_Crystal` / `type_32_42_62_Crystal`):
     `type_222_Crystal g11 g22 g33` → diag; `type_2_Crystal g11 g22 g33 g13`
     → diagonal + g13 = g31 (two-fold axis along x2); `type_m_Crystal g12 g23`
     → g12 = g21, g23 = g32 (mirror normal to x2); `type_1_Crystal g11 g22 g33
     g23 g13 g12` → full symmetric tensor. All via `Rho.fromIm`.
   - Private `assembleRho (hand : Handedness) (gyration :
     GyrationClass<RhoValue>) : Rho` — applies `hand.sign` to every component,
     then routes: CubicActive → `Rho.cubicCrystal`; UniaxialActive →
     `Rho.type_3_4_6_Crystal` (NOT `type_32_42_62_Crystal` — needs a g12 the
     two-component record cannot supply); PlanarActive → `Rho.planarCrystal`;
     the four new classes → the four new builders.
   - `type RhoWithDispValue with member toRhoWithDisp : RhoWithDisp` —
     constant case short-circuits to `RhoWithoutDisp`; dispersive case wraps a
     `WaveLength -> Rho` closure that maps `DispersionFormula.evaluate w |>
     RhoValue` over the class and assembles.
4. **Green** — build the solution, run BerremanTests, then the remaining
   roster suites (ADVISORY — the arc-runner gate engine is the sole gate
   authority and re-runs after exit). Capture logs to `.artifacts/009-*.log`.

## Tests (11 new facts)

Quartz class-32 example (g11 = +5.9e-5, g33 = −10.1e-5) through
`UniaxialActive` → diag(g11, g11, g33); cubic / planar / 222 / monoclinic-2 /
monoclinic-m / triclinic-1 constant assemblies each against an independently
built `Rho.fromIm` literal; LeftHanded negates the quartz and the triclinic
(all six slots) tensors; the dispersive case evaluates each component's
formula at the wavelength (uniaxial, RightHanded) and LeftHanded negates the
evaluated tensor (cubic). All comparisons via the new
`verifyMatrixEqualityRho` (delegates to the existing `verifyMatrixEquality`
relative-norm helper — no new epsilon logic).

## Files to modify

- `Berreman/Berreman/Dispersion.fs` (append rho tree)
- `Berreman/OpticalProperties/Active.fs` (open, 4 builders, assembleRho, extension)
- `Berreman/BerremanTests/MatrixComparison.fs` (verifyMatrixEqualityRho)
- `Berreman/BerremanTests/RhoWithDispValueTests.fs` (new)
- `Berreman/BerremanTests/BerremanTests.fsproj` (one Compile entry)

## Risks

- Record field-label collisions (g11/g33 subsets of Triclinic1Gyration's
  labels): annotate every bare record literal in tests and keep each record's
  `map` return-type annotated.
- Negative literals after a DU constructor (`RhoValue (-10.1e-5)`) need parens.
- LF endings + Dispersion.fs BOM must be preserved (Edit preserves; new files
  written LF without BOM).
