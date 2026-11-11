# Step 0306 — impl plan

## Approach

A copy now brings the kit import profiles its experiments' data is mapped by (spec 0050 S9.2; 001 §H "Between kits";
.spec-md §3.15).

1. **Domain (`Kit.fs`, at its end).** `type ImportProfileName with member this.nextFreeAmong (taken : ImportProfileName
   list) : ImportProfileName`: the name of `FileStem.firstFreeText this.value` over the taken names' texts, the suffix
   sequence items, setups and experiments already share.
2. **Storage (`KitContentFiles.fs`).**
   - `ExperimentCopyReport` gains `profiles : ImportProfile list` and `kitFile : KitFile` as its last fields.
   - The private `ExperimentCopyPlan` gains `profiles`, and a pure `placeProfiles` step runs after `renumber`:
     - it takes the distinct profile names of the kept experiments' `dataOpt`, in order of first appearance;
     - it keeps those `ChosenImportProfile.tryOfName source.kitFile.importProfiles` answers as `KitProfileChoice`;
     - each one is reused when held under its name with equal content, added as `nextFreeAmong` when held under its
       name with other content (the experiments mapped by it follow the new name), and added as it is otherwise.
   - `writeAll` saves `this.saveKitFile { this.kitFile with importProfiles = this.kitFile.importProfiles @ added }`
     right after the data files, before items, setups and experiments, only when something is added. The report's
     `kitFile` is the saved kit file, else `this.kitFile`.
3. **UI (`TableAndElementRotationView.fs`).** `CopyChosenExperiments` sets the window content over
   `{ model.kit with kitFile = report.kitFile }`.
4. **Tests.**
   - `KitTests`: new fact `a profile name takes the first free suffix among the taken ones`.
   - `ExperimentCopyTests`: new fact `a copy brings the kit profiles its experiments use, reusing an equal one and
     renaming a different one`. The one full-report literal gains the two new fields.
   - `KitWindowModelTests`: the copy source fixture is parametrised by an optional kit profile, and the changed fact
     uses `Lab scan` and asserts the window's and the disk's kit file.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/Kit.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/KitContentFiles.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/TableAndElementRotationView.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/ExperimentCopyTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitWindowModelTests.fs`

## Risks

- **A renamed profile could take a name a later copied profile carries.** Then the kit file would hold two profiles
  of one name. To prevent this, "held" covers the target's profiles and the ones added before, so a later clash is
  reused or renamed too.
- **The window fact's `Assert.Equal<KitContext>(model.kit, copied.kit)` changes.** The kit file now differs, so the
  fact compares against `{ model.kit with kitFile = <read back> }`.
- **ELEV.** The new member is an intrinsic extension on our type, with no new free functions. The Storage locals
  follow the existing closures.
