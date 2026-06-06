# Reuse critique -- 001.slice-md cycle 1

## Coverage

- Helper roots walked: repo root `C:\GitHub\Berreman` — focused on `OpticalConstructor.Domain/`, `OpticalConstructor.Tests/`, `OpticalConstructor.Ui/`, and the engine `Berreman/Berreman/Geometry.fs`.
- Files inspected: ~28/200 (cap not reached).
- Extensions: task bound was `.py,.md,.json` (a generic stub default; no `.py` exists in this F# repo). Most defensible reading: walk the `.fs` sources plus the `.json` schema so the critique is meaningful; deviation recorded here.

## Findings

### F1: New `Placement.Vector3` is a third parallel 3-vector type in the same solution

- **Worker added:** `Placement.Vector3` — a `{ x; y; z }` record with `create`/`dot`/`cross`/`norm`/`normalized` (`Berreman/OpticalConstructor/OpticalConstructor.Domain/Placement.fs:25-47`).
- **Existing helper:**
  - `SystemView3D.Vec3`, a structurally identical pure `{ x; y; z }` record introduced for the *same* documented reason ("plain floats … NOT the Math.NET `RealVector3` … keeps geometry pure/headless"), at `Berreman/OpticalConstructor/OpticalConstructor.Ui/SystemView3D.fs:40-45`.
  - The engine `RealVector3`, which already supplies the vector algebra the worker re-coded by hand: `cross` (`Berreman/Berreman/Geometry.fs:93-99`), dot via `(*)` (`:87`), `norm` (`:83-85`), `create` (`:91`), `zeroVector` (`:101`).
- **Why it matters:** the diff brings the solution to **three** 3-vector representations. The UI `Vec3` and the new `Vector3` are byte-for-byte the same shape with the same justification, and the names collide — `SystemView3D.fs:35` literally says "the deferred OpenTK view maps `Vec3` to a `Vector3`", so a future reader now has two unrelated `Vec*` records to disambiguate. The hand-rolled `cross`/`dot`/`normalized` duplicate logic the engine already owns, so a future fix (e.g. a normalize edge case) can land in one copy and silently diverge in the others.
- **Suggested action:** *suggestion only — judge decides.* The serialization rationale for not storing a `RealVector3` is recorded in the SoW and is defensible for the **stored** box normals. Two lighter options exist: (a) host a single pure `{ x; y; z }` type in the Domain layer (which `SystemView3D` already references) so Placement and the UI viewport share it instead of carrying near-duplicates; or (b) keep the plain record purely as the serialization DTO and route the vector **algebra** through the engine `RealVector3`. Either removes the third hand-rolled algebra copy. Leaving as-is is acceptable only if the duplication is documented as intentional.

### F2: Hand-rolled Rodrigues `rotateAbout` duplicates the engine rotation subsystem

- **Worker added:** `rotateAbout` — a hand-coded Rodrigues' rotation formula (`Berreman/OpticalConstructor/OpticalConstructor.Domain/Placement.fs:239-249`) plus the manual rest-normal composition in `orientedBasis` (`:259-273`).
- **Existing helper:** the engine `Rotation` type (`Berreman/Berreman/Geometry.fs:601-622`) with `rotateX`/`rotateY`/`rotateZ` (`:617-619`) and the `RealVector3.rotate` member (`:628-630`), composed from `RotationConvention` (`:578-598`).
- **Why it matters:** this slice's own Risk note says to "reuse the engine `Angle` type to avoid a parallel rotation representation." The worker reused `Angle` for the scalar but introduced a parallel rotation **implementation**. The R2 step (about the table normal `+Z`) and the R3-at-rest step (about `N3`) are fixed-axis rotations that map onto the engine's `rotateZ`/fixed-axis helpers; a second rotation code path is exactly the numerical-divergence risk the spec flags around the AC-A4 non-orthogonality math.
- **Suggested action:** *suggestion only — judge decides.* Reuse `RealVector3.rotate`/`Rotation` for the fixed-axis (R2 and R3-at-rest) steps at minimum. **Honest nuance:** the engine exposes only fixed-axis rotations composed via `RotationConvention`; it does **not** expose a direct arbitrary-axis Rodrigues, and R1 spins about the *oriented* face normal (`n1b`), an arbitrary axis. So this is a near-miss, not a drop-in replacement — if the oriented-axis spin genuinely cannot be expressed through the engine conventions, the right outcome is to document that as the reason a local rotation helper survives, rather than leaving it as an undocumented parallel path.

### F3 (low confidence): `centralRayDirection`/`tableNormal` restate `RealBasis3.defaultValue`

- **Worker added:** `centralRayDirection = (1,0,0)` and `tableNormal = (0,0,1)` rest axes (`Berreman/OpticalConstructor/OpticalConstructor.Domain/Placement.fs:51-55`), with `orientedBasis` returning an `(N1,N2,N3)` triple (`:259-273`).
- **Existing helper:** the engine `RealBasis3` orthonormal-basis type and its `defaultValue` identity triad (`vX=+X, vY=+Y, vZ=+Z`) at `Berreman/Berreman/Geometry.fs:104-125`.
- **Why it matters:** the canonical `+X`/`+Z` axes and the basis concept are restated rather than referenced. Low cost, and called out as *low confidence* deliberately: the worker's oriented basis is intentionally **non-orthonormal** under R3 (A.4.4), which `RealBasis3` (orthonormal) cannot model, so a full reuse is impossible. Listed for completeness; not a re-spawn driver on its own.

## Bottom line

The reused surfaces the spec named are honoured well — `Angle`, the `Units` spine, `ProjectJson.serializeProject`/`deserializeProject`, and the schema-validation path are all consumed, not re-typed, and the test-scaffold duplication (`vacuumSystem`/`okOr`/`tol`) merely follows this test project's established per-module-private convention (every sibling test module does the same; no shared fixtures module exists), so it is **not** a finding. The substantive observations are F1 and F2: a third pure 3-vector type and a parallel rotation implementation, both in domain code, both with partial but real overlap against existing engine helpers and a recorded (F1) or arguable (F2) justification. My read: substantive enough to warrant the judge's attention but not obviously re-spawn-forcing — each has a defensible engineering reason, so the call is whether "documented duplication" clears this project's bar. I have no authority to bind that; the code judge decides routing.
