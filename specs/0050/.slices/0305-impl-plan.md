# Step 0305 — impl plan

## Approach

The strict `X,Y` seam is replaced by attach-with-profile (spec 0050 S9.2; 001 §R, §K).

1. **Domain.**
   - `ImportProfiles.fs` gains `MappedPoints.selectedBy (rows : DataRowSelection)`, after `validatedFor`.
     - `AllDataRows` keeps every point.
     - `KeyedDataRows key` keeps the capture rows whose `experiment` label's text is the key, the observations of that
       key, and the mapped rows whose `keyOpt` is it.
     - A series, and rows of which none holds a key, are `MalformedDataFileErr`.
     - An empty selection is `EmptyDataFileErr`.
   - `ExperimentDataProxy` becomes `{ readPickedFile; attachDataFile; attachmentStatus; readMapped }`.
   - `ExperimentDataLoad.loadAndValidate` becomes `readMapped` bound to `validatedFor`.
2. **Storage.**
   - The `ExperimentDataStore.fs` `<Compile>` line moves to directly after `LabFileImport.fs`, and its comment is
     deleted.
   - The picked-file read becomes one private extension member on `FileName`, shared by `readPickedFile` and
     `attachDataFile`. It is a member and not a private free function because ELEV013P flags a private one-parameter
     function over one of our types (`enforcePrivateSignatures` defaults on).
   - `readMapped` reads the kit file and resolves `ChosenImportProfile.tryOfName`. It then reads the data file, maps
     it by `ChosenImportProfile.mapText` and answers `selectedBy`. A `KitError` of either read becomes one
     `MalformedDataFileErr`.
   - `DataFileKind` and `loadThrough` are deleted.
3. **Tests**, written first (red).
   - `ImportProfileTests`: one new fact.
   - `ExperimentDataProxyTests`: a new mock and three renamed facts.
   - `ExperimentDataStoreTests`: kits are initialised, four facts change or are renamed, and three facts are new.
   - The three UI classes' mocks move to the new fields.

## Files

Inside the effects:
- `ImportProfiles.fs`, `ExperimentDataProxy.fs`, `ExperimentDataLoad.fs`;
- `ExperimentDataStore.fs` and the Storage fsproj;
- `ImportProfileTests.fs`, `ExperimentDataProxyTests.fs`, `ExperimentDataStoreTests.fs`;
- `InverseConstructorTests.fs`, `SolverHandoffWindowTests.fs`, `ScrollExtentTests.fs`.

The UI production files listed in the effects need no change: they hold the proxy and build none.

Outside the effects: `OpticalConstructor.Tests.fsproj`. Its comment on `ExperimentDataProxyTests.fs` names
`tryLoadIntensity / tryLoadEllipsometric`, so the acceptance grep (`git grep … -- Berreman/` MUST find nothing) cannot
pass without deleting it. The project is in `touches`. The deviation is recorded in the impl-log.

## Risks

- `readMapped` now needs an initialised kit, because it reads the kit file. The app's kits are always initialised.
  The store tests initialise theirs, as the slice says.
- ELEV: the new signatures are all elevated, and the shared picked-file read is a member (see above).
- The UI mocks lose the old per-profile split, because one `readMapped` answers for any profile. No fact in the three
  UI classes attaches under a profile its mock did not answer for (checked fact by fact).
