# 0334 — impl plan

The absorbing triclinic tensor construction moves onto its parameter record (spec 0050 §3.16, S10.1), the same move
steps 0332 and 0333 made for `UniaxialParameters` and `TriclinicParameters`.

1. `MuellerInverse.fs`: `AbsorbingTriclinicParameters` gains, after `axes`,
   `member this.opticalProperties : OpticalProperties` whose body is `buildAbsorbing`'s with `p` read as `this`: the
   local `e re im = Complex(re, im)`, `nᵢ² = this.indexᵢ.value * this.indexᵢ.value`, ε through `Eps.create` from the
   complex symmetric 3x3 (diagonal `nᵢ² + iε″ᵢᵢ`, off-diagonal `iε″ᵢⱼ`), `Mu.vacuum`, and ρ through `Rho.create`
   from the purely imaginary symmetric 3x3 of the six gyration components — same entries, same order. `open
   System.Numerics` and `open Berreman.MaterialProperties` are already there. A short doc citing spec 0050 §3.16 and
   `specs/0044/.manual/018-absorbing-biaxial-inverse.md`.
2. `AbsorbingBiaxialSample.fs`: `buildAbsorbing (p : AbsorbingTriclinicParameters) : OpticalProperties` stays, signature
   unchanged; its body becomes `p.opticalProperties`; the old tensor body is deleted. Its doc shrinks to a short
   contract describing the builder without naming a product destination. `open System.Numerics` goes dead with the
   deleted body (`Complex` was its only use) and is removed.
3. `AbsorbingBiaxialInverseTests.fs` / `AbsorbingBiaxialNoisyTests.fs`: no edit needed — both keep calling
   `buildAbsorbing` for their forward models (`createBerremanForward buildAbsorbing solverParameters`).

Risks: none numerical — the member evaluates the same `Complex` constructions from the same `double` products in the
same order, so every forward-model number is bit-identical. Elevation: the member is declared on its own type
(members may speak their type's primitives) and returns an engine type; the local `e` lives inside the member, no new
free function.

Verification: build, elevation, constructor-unit-tests, unit-tests over `BerremanTests.AbsorbingBiaxialInverseTests`,
and the nineteen s33 invariants (development runs only — the gate engine is the authority).
