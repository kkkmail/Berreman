# Step 0353 — impl plan

## Approach

This step is a move, not a rewrite. `UniaxialParameters`, `TriclinicParameters` and `AbsorbingTriclinicParameters`
move with their `opticalProperties` builders (steps 332–334) out of `MuellerInverse.fs` into a new
`OpticalConstructor.Domain/CrystalPhysics.fs` (`module CrystalPhysics`). That file compiles directly after
`DispersionModels.fs` and before `MaterialLibrary.fs`, so a material version can later hold one of the records as
its physics (S11.1, .spec-md §3.16).

- **The bodies move verbatim:** each record's doc, fields and builder.
- **Each `axes` stays in `MuellerInverse.fs`.** `ParameterAxis` is declared there and `ParameterName` in
  `ItemModels.fs`, both after `MaterialLibrary.fs`. Each `axes` becomes a static type extension
  (`type UniaxialParameters with static member axes …`), placed directly after `ParameterAxis` with the same doc and
  body. So `UniaxialParameters.axes` reads as before wherever `MuellerInverse` is open.
- **`MuellerObservation` stays where it is** in `MuellerInverse.fs`.

## Files

1. `OpticalConstructor.Domain/CrystalPhysics.fs`: new. A module doc, the opens the moved bodies need, then the
   three records.
2. `OpticalConstructor.Domain/OpticalConstructor.Domain.fsproj`: one `<Compile Include="CrystalPhysics.fs" />` line
   after `DispersionModels.fs`, with no comment.
3. `OpticalConstructor.Domain/MuellerInverse.fs`:
   - opens `OpticalConstructor.Domain.CrystalPhysics`;
   - its module doc's builder sentence adds that the records live in `CrystalPhysics` (spec 0050 S11.1);
   - the three record declarations are deleted;
   - the three `axes` extensions go after `ParameterAxis`.
4. These consumers add `open OpticalConstructor.Domain.CrystalPhysics` and nothing else:
   - `BerremanTests/`: `MuellerInverseTests`, `LangasiteInverseTests`, `BiaxialInverseTests`,
     `AbsorbingBiaxialInverseTests`, `AbsorbingBiaxialNoisyTests` and `FeasibleBiaxialInverseTests`;
   - `BerremanTestSupport/`: `BiaxialSample` and `AbsorbingBiaxialSample`;
   - `OpticalConstructor.Tests/ItemEvaluationTests.fs`, which the effects list omits. It names `UniaxialParameters`,
     so the build needs the open. Steps 0259, 0291 and 0313 set the precedent for this kind of edit.
5. `OpticalConstructor.Tests/MaterialComplexityTests.fs`: the new fact
   `each crystal record's builder composes its permittivity from its own fields`. It builds test-local literal
   records and checks each `eps` through the file's existing `closeC` at 1e-15:
   - uniaxial: diag(n_o², n_o², n_e²);
   - triclinic: diag(n₁², n₂², n₃²);
   - absorbing: ε₁₁ = n₁² + i·ε″₁₁ and ε₁₂ = i·ε″₁₂.

## Risks

- **Record-label resolution:** `g11` is shared by all three records, and `index1..3` by
  `MaterialComplexityEditor`'s state record. The three records keep their order inside one module, and every
  consumer opens `CrystalPhysics` right beside `MuellerInverse`, so resolution is unchanged. The new fact annotates
  its literals.
- **ELEV:** the new module adds only the three moved types (ELEV014: one declaration each). The extension members
  `axes` sit on three different receivers (ELEV019).
- **Bit-identity:** the expressions are unchanged, so the numbers are unchanged.
