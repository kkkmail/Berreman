# Step 0427: a compound held as an analyzer is seen from behind

2026-11-15. After R0001 (`.manual/030`) AT-1 reaches the linear engine, which refuses
its objects: QZ, LR and the series each have design rank 12, never 16 (measured in a
throwaway clone with the 0427 tree; the rows equal the reference's, 228/227/228). No
row reaches the Mueller matrix's bottom row: the run law evaluates every compound in
its stored light order, so CPL2 as an analyzer is an LP then a retarder and reads no
S₃. The reference's circular analyzer is an LP behind its retarder
(`data/optics-mueller/data/reference/matrix_glue.py:161`), and stage 1 already
calibrates CPL2 with that model (`MuellerReconstruction.AnalyzerModel.circularAt`:
the retarder at +45° with phase −δ). `StudyEngineTests`' families avoided the defect
with a quarter-wave plate ahead of CPL2.

## Binding ruling

At 0427, authorize editing `Berreman/OpticalConstructor/OpticalConstructor.Domain/ItemEvaluation.fs`,
`Berreman/OpticalConstructor/OpticalConstructor.Domain/StudyForward.fs` and
`Berreman/OpticalConstructor/OpticalConstructor.Ui/TableAndElementRotationView.fs`, and
correcting any existing test fact whose expectation encodes the old analyzer law, without
adding, renaming or removing a test identity.

The rule: a compound held in an analyzer slot (an optic behind an object, as
`SetupSlot.inLightOrder` assigns) is traversed as the film seen from behind: light
meets its components last first, each component's azimuth mirrored, so its matrix is
F · (A₁ · … · Aₙ) · F with F = diag(1, 1, −1, −1), then turned by the item's rotation.
For CPL2 this is exactly stage 1's circular analyzer. Every run path (the study's
forward model and the table's experiment run) binds a slot through the one member that
applies this rule; prep, object and source slots are unchanged. Stored items, models
and file formats are unchanged.

Verify before commit: the 26 gates of `.slices/0427.gates` as declared, plus the
`constructor-unit-tests` gate as declared in `.slices/0426.gates`. Resume normally
with executability profile 1 and the operator reply, without `--accept-current-slice`.
At 0428-0494 preserve this rule; the additional authority is confined to 0427. No
gate, floor, deadline, census, durable format or published-spec amendment is
authorized.
