# Step 0466 — impl plan

## Approach

Append to `PlannerSymmetry.fs`, after step 465's declarations, the layer-1 "what each cut sees" vocabulary
(spec 0050 S14.1):

1. `let private markerTolerance : float = 1.0e-9` — the one tolerance of the module (precedent:
   `RayModel.fs:grazingCosine`).
2. `CrystalAxis` (`FirstCrystalAxis | SecondCrystalAxis | ThirdCrystalAxis`) with
   `static member marker : OpticalProperties` (a `TriclinicParameters` probe, n = 1, 2, 3, every g = 0, through
   its `opticalProperties`, so ε = diag(1, 4, 9)) and `static member normalOf (oriented : OpticalProperties) :
   CrystalAxis option` (the axis whose marker value equals Re ε₃₃ within the tolerance while |ε₁₃| and |ε₂₃| are
   below it). The marker values are read back from `CrystalAxis.marker` itself, so 1, 4 and 9 are written once.
3. `CrystalAxisPair = { firstAxis; secondAxis }` (a record: a repeated-type tuple is ELEV011).
4. `TensorComponent` (five cases) with `member this.axes : CrystalAxis list`.
5. `CrystalComponent = { name : ParameterName; ``component`` : TensorComponent }`.
6. `type CrystalParameters with member this.components`: `List.map2` over `this.parameterAxes` (Studies.fs) and
   the case's component list, so no parameter name is re-written.
7. `type SampleCut with member this.normalAxisOpt` = `normalOf (orientForCut this 0° marker)`.
8. `type CrystalOrientation with member this.normalAxisOpt` = `normalOf (marker.rotate this.toRotation)` — the
   rotation `ResolvedLayer.getLayer` applies through `Layer.rotate` → `OpticalProperties.rotate`.

Tests: three facts added to `PlannerSymmetryTests` with exactly the brief's names, over `CrystalMaterialFixtures`.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/PlannerSymmetry.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/PlannerSymmetryTests.fs`

## Risks

- `component` is an F#-reserved identifier (FS0046 warning). Zero warnings is binding, so the field is written
  `` ``component`` `` (the repo's precedent: `EntryFileJson.fs` `` member this.``sealed`` ``), keeping the brief's
  name with no warning.
- A new record with a `name : ParameterName` field could capture record-label inference in files that open
  `PlannerSymmetry`; only the test file opens it, and every use is annotated.
- ELEV uniqueness: every new type, case and extension-member name was grepped and is free.
