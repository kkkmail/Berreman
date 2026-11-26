# Step 0291 — impl plan

## Approach

An experiment's data becomes a kit file mapped through a named import profile to a row selection (spec 0050 S9.1;
001 §K).

1. **Domain, `Kit.fs`, after `DataAttachmentStatus` / `DataAttachment`:**
   - `ImportProfileName` (`tryCreate`, `intensitySeries`, `ellipsometricSeries`);
   - `DataRowKey`;
   - `DataRowSelection = AllDataRows | KeyedDataRows of key : DataRowKey`;
   - `MappedAttachment = { attachment; profile; rows }`.
2. **Domain, `Experiments.fs`:** `dataFileOpt` becomes `dataOpt : MappedAttachment option`, and `attachDataFile` takes
   a `MappedAttachment`. `commit` already keeps an edited experiment's field and gives an appended one `None`.
3. **Domain, `ExperimentDataLoad.fs`:**
   - `DetectorKind.builtInProfile` is a type extension.
   - `loadAndValidate` takes `(data : MappedAttachment)` and checks before it reads anything:
     - a `KeyedDataRows` selection is refused with `MalformedDataFileErr` naming the key;
     - an unknown profile is refused with `MalformedDataFileErr` naming the profile.
   - Otherwise it reads through the profile's loader and validates against the detector exactly as before.
   - The detector no longer picks the loader; the profile does.
4. **Storage:**
   - The schema's `dataFileOpt` becomes `dataOpt`, an object holding `attachment`, `profile` and `rows`.
   - `copyExperimentsFrom` reads `dataOpt` paths and re-points `attachment`, keeping `profile` and `rows`.
5. **Ui:**
   - The `AttachDataFileTo` arm attaches the detector's built-in profile over every row, and
     `validateExperimentAttachment` takes the mapped attachment.
   - `dataFileRow` shows `file: <path> (<profile>)`.
   - `SolverHandoffView` reads `dataOpt`, and its caption adds the profile.
6. **Tests:** fixtures map their attachments, and the slice adds three new facts and changes four expectations.

## Files

- **Domain:** `Kit.fs`, `Experiments.fs`, `ExperimentDataLoad.fs`.
- **Storage:** `schema/experiment.schema.json`, `KitContentFiles.fs`.
- **Ui:** `TableAndElementRotationView.fs`, `SolverHandoffView.fs`.
- **Tests (listed):**
  - KitTests, ExperimentProxyTests, ExperimentCollectionRoundTripTests and KitContentFileJsonTests;
  - ExperimentCopyTests, KitWindowModelTests and ExperimentDataProxyTests;
  - InverseConstructorTests, SolverHandoffWindowTests, ScrollExtentTests (no attachment, unchanged) and KitWindowTests.
- **Tests outside the list, for the build:** each holds an `Experiment` literal or reads `dataFileOpt`, which is
  renamed.
  - KitLibraryTests, KitOpeningTests, KitStartTests, OutOfBandDiagnosticTests, SetupModelTests and SetupTests;
  - KitStartWindowTests and LauncherTests.

## Risks

- **`rows` and the serializer.** `WithUnionUnwrapFieldlessTags` writes `AllDataRows` as the JSON string
  `"AllDataRows"`, and only `KeyedDataRows` as a `{Case, Fields}` object. A schema that requires `rows` to be an object
  would refuse every attachment. Probe: `.artifacts/0291-probe/probe.fsx`. The schema therefore takes `rows` in the
  serializer's case form, a string or an object, recorded as a deviation.
- **The extension member needs its module opened.** `builtInProfile` is an extension in `ExperimentDataLoad`, so the
  Ui file and KitTests must open that module.
- **SolverHandoffWindowTests' ellipsometric fixture** must be mapped by `ellipsometricSeries`. With the intensity
  profile it would read the intensity path.
