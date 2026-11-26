# 0333 — impl plan

The triclinic tensor construction moves onto its parameter record (spec 0050 §3.16, S10.1), the same move step 0332
made for `UniaxialParameters`.

1. `MuellerInverse.fs`: `TriclinicParameters` gains, after `axes`,
   `member this.opticalProperties : OpticalProperties` whose body is `buildTriclinic`'s record expression
   (`Eps.fromRefractionIndex (this.index1, this.index2, this.index3)`, `Mu.vacuum`,
   `Rho.type_1_Crystal this.g11 … this.g12`). `open OpticalProperties.Active` is already there (step 0332).
   A two-line doc citing spec 0050 §3.16.
2. `BiaxialSample.fs`: `buildTriclinic (p : TriclinicParameters) : OpticalProperties` stays, signature unchanged;
   its body becomes `p.opticalProperties`; the old tensor body is deleted. Its doc shrinks to a short contract that
   describes the builder without naming a product destination. `open OpticalProperties.Active` goes dead with the
   deleted body and is removed (as step 0332 did for its three test files).
3. `BiaxialInverseTests.fs` / `FeasibleBiaxialInverseTests.fs`: no edit needed — they keep calling `buildTriclinic`.

Risks: none numerical — the member evaluates the same expression with the same arguments, so every forward-model
number is bit-identical. Elevation: the member is declared on its own type (members may speak their type's
primitives) and returns an engine type; no new free function.

Verification: build, elevation, constructor-unit-tests, unit-tests over `BerremanTests.FeasibleBiaxialInverseTests`,
and the nineteen s33 invariants (development runs only — the gate engine is the authority).
