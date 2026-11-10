# Step 0255 — impl plan

## Approach

Pure Domain data for the identities a setup is stored and referenced under (spec 0050 S8.2), plus the one dial
rule its slots read. Nothing consumes them before step 256.

1. `OpticalConstructor.Domain/ItemModels.fs` — on `DialSign`, add `physicalAngleOf zero reading` =
   `Angle (this.value * (reading.value - zero.value))` (the exact expression `MuellerReconstruction.physicalAngle`
   holds today, so the reference numbers do not move) and its inverse `readingOf zero angle` =
   `Angle (zero.value + this.value * angle.value)`.
2. `OpticalConstructor.Domain/MuellerReconstruction.fs` — `physicalAngle`'s body becomes
   `analyzer.dialSign.physicalAngleOf analyzer.zeroDial dial`; signature kept, doc says the rule is `DialSign`'s.
3. `OpticalConstructor.Domain/Kit.fs`
   - (a) directly before `type ItemLabel with … nextFreeAmong`: `type FileStem with static member firstFreeText`
     — the `<text>`, `<text> (2)`, … sequence moved out of `nextFreeAmong`, whose body becomes
     `ItemLabel (FileStem.firstFreeText this.value takenStems)`.
   - (b) appended at the end: `SetupError` (7 `*Err` cases of `reason : string`, `describe`), `SetupName`
     (`tryCreate`, `fileStem`, `nextFreeAmong`, `firstFree`), `SetupRevision` (`first`, `next`), `SetupReference`
     (`describe`). The `ItemDial` doc's cross-reference moves to `DialSign.physicalAngleOf`.
4. Tests: 4 new facts in `KitTests`, 1 new fact in `ItemModelsTests` (the census names exactly these five).

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/ItemModels.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/MuellerReconstruction.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/Kit.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/ItemModelsTests.fs`

## Risks

- Numbers of `BerremanTests.MuellerReconstructionTests` must stay bit-identical: keep the exact floating-point
  expression of the old `physicalAngle` (no switch to `Angle`'s `(-)` operator).
- ELEV: new names checked unique across the repo and STL (none found). `firstFreeText` is a member on `FileStem`,
  so its `string` surface is a member speaking its type's primitives. No `Ctor other.value` shape (ELEV001/005).
- `firstFree` walks `Setup 1`, `Setup 2`, … — a different sequence from `firstFreeText`'s `(n)` suffix, so it has
  its own walk; the spec adds no other member to share the case-insensitive stem test.

## Revised during implementation

- `FileStem.firstFreeText` takes `taken : string list` (the taken names' texts), not `FileStem list`: ELEV010
  refuses a `string` beside a `FileStem` in one signature. `SetupName.firstFree` reuses `firstFreeText`. See the
  impl-log's Decisions.
