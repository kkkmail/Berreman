# Step 0345 — impl plan

Move the transparent triclinic ground truth `BiaxialSample` from `BerremanTests` into `BerremanTestSupport`
(spec 0050 S10.2, 001 §Q AT-2), as a move: one `BiaxialSample` module exists afterwards.

1. Move `Berreman/BerremanTests/BiaxialSample.fs` to `Berreman/BerremanTestSupport/BiaxialSample.fs` (plain `mv`:
   the worker protocol forbids `git mv`). Namespace `BerremanTests` → `BerremanTestSupport`. Every declaration and
   number, and the opens, unchanged; `buildTriclinic` still calls `TriclinicParameters.opticalProperties`.
   Doc: "every biaxial suite in this assembly shares" → "every biaxial suite shares"; that paragraph gains one
   sentence citing spec 0050 S10.2 (001 §Q AT-2); "in the TEST assembly" → "in test code". Nothing else.
2. `BerremanTestSupport.fsproj`: `<Compile Include="BiaxialSample.fs" />` after `CommittedKits.fs`; project
   reference to `OpticalConstructor.Optimization.fsproj` after the Domain one. No comment.
3. `BerremanTests.fsproj`: delete the `BiaxialSample.fs` compile with its comment, and the comment above
   `InverseFitHarness.fs` (the line itself stays).
4. `open BerremanTests.BiaxialSample` → `open BerremanTestSupport.BiaxialSample` (trailing comments kept) in the
   five consumers.

Risks: the Optimization reference must not create a cycle (Optimization references only Domain); ELEV duplicate-name
rules see `BiaxialSample` declarations from a new assembly (no namesake remains, since the old file is deleted);
ELEV rules now apply to `BerremanTestSupport` as a product-scoped project — the module's public signatures
(`epsDiagonal : OpticalProperties -> float list`, `rhoImaginary`, `opticAxisFromLargest`, ...) must be checked by
the elevation run, as the test project was out of the analyzer's scope.

Verification: build, elevation, the two selected unit-test classes (BiaxialInverseTests ~997 s,
FeasibleBiaxialInverseTests ~104 s), and the s34 invariant greps.
