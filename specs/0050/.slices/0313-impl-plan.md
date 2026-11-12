# Step 0313 — impl plan

## Approach

Spec 0050 S9.3: the experiment file records its data's point exclusions and its setup's corrections.

1. **Domain, `Kit.fs`**, directly before `MappedAttachment`:
   - `MappedPointNumber of int`, with `.value` and a `tryCreate` that answers `None` below 1.
   - `ExclusionReason of string`, with `.value` and a `tryCreate` that answers `None` for a blank text.
   - The record `PointExclusion = { point; reason }`.
   - `MappedAttachment` gains `exclusions : PointExclusion list` after `rows`.
   - Three members: `withExclusion` (replace, or insert in point order), `withoutExclusion` and `tryFindExclusion`.
     Point order sorts on the `MappedPointNumber` itself, never on its `.value`.
2. **Domain, `Experiments.fs`**:
   - After `ExperimentNotes`: `CorrectionNote of string` (`.value`, a `tryCreate` that refuses a blank text) and the
     record `SetupCorrection = { previous; corrected; note }`.
   - `Experiment` gains `corrections : SetupCorrection list` after `dataOpt`.
   - `commit` appends with `corrections = []`. Its edit arm already copies with `{ e with … }`, so it keeps them.
3. **Storage**:
   - `experiment.schema.json`: `dataOpt` requires `exclusions` (objects of exactly `point` ≥ 1 and a non-blank
     `reason`); `experiment` requires `corrections` (objects of exactly `previous`, `corrected` and a non-blank
     `note`). The setup-reference object moves into a local `$defs/setupReference`, so `setup`, `previous` and
     `corrected` share one definition instead of three copies.
   - `KitContentFiles.copyExperimentsFrom` already copies each experiment with `{ … with }`, so the new fields ride
     along as written. Only its doc changes, citing S9.3.
4. **Ui, `TableAndElementRotationView.fs`**: the `AttachDataFile` arm sets `exclusions = []`. The scan-description
   `Experiment` literal gets `corrections = []`; this is a compile repair only.
5. **Fixtures**: every `MappedAttachment` literal gets `exclusions = []` and every `Experiment` literal gets
   `corrections = []`. The build finds every site.
6. **Tests**:
   - Two new facts: `KitTests` (point exclusion) and `ExperimentProxyTests` (correction note and commit).
   - Three changed facts: the round-trip and refusal facts in `KitContentFileJsonTests`, and the profile and
     row-selection copy fact in `ExperimentCopyTests`.

## Files

- Domain: `Kit.fs`, `Experiments.fs`.
- Storage: `schema/experiment.schema.json`, `KitContentFiles.fs`.
- Ui: `TableAndElementRotationView.fs`.
- Tests named by the slice, plus any unlisted fixture the build finds holding a record literal. These are compile
  repairs only, as steps 261, 288 and 291 did for `KitOpeningTests`, `KitStartTests`, `KitStartWindowTests` and
  `LauncherTests`.

## Risks

- **Unlisted fixtures.** Four test files outside the edit list build `Experiment` literals, so the build needs them
  fixed. Precedent: steps 261, 288 and 291.
- **Schema `$ref`.** A local `$ref` must resolve against the schema's `$id`. A fact that refuses a bad `setup`
  revision already guards this.
- **ELEV.** Members are exempt from the primitive rules. The new names must be unique: grep before naming.
