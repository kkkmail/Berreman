# Step 0320 — impl plan

## Approach

`MatchByTemplate` applies the split that step 319's `SplitByColumn` holds (spec 0050 S9.3; 001 §K, §P).

1. **Guard, then the held split.** The handler runs behind step 185's `guardKitWrite`, so a read-only kit answers with
   the read-only status. With `dataSplitOpt = None` it answers the model unchanged.
2. **Refusal.** With offers ticked (`creating` not empty) and a draft that cannot record (no `canCommit`, or no
   `draftCapture`), it answers the `Not split — …` status and writes nothing. This is `RecordExperiment`'s condition.
3. **Attach once.** `model.experimentData.attachDataFile s.file` copies an outside file into `data/` or records an inside
   one in place. An error answers `Not attached — <describe>`.
4. **Plan purely.** A private fold over `s.split.groups` in order builds the kit writes, a new private DU `SplitWrite`
   (`SplitSetupWrite setup | SplitExperimentWrite experiment`):
   - a matched group's experiment, found by name in the collection, takes `Experiments.attachDataFile` of the keyed
     `MappedAttachment`;
   - a ticked offer adds its setup when neither the kit nor an earlier offer holds it (by equality). It then appends one
     experiment of the draft's scan, as `RecordExperiment` does (`chooseMeasurement capture` then `commitOverSetups`
     over `offer.setup.reference` and `offer.experiment`). The draft is put back unchanged, and the new experiment takes
     the keyed attachment before it is written, so each file is written once;
   - an unticked offer or an unmatched group plans nothing.
5. **Write in order.** The existing `writtenInOrder` (GenerateSetupGrid's, step 276) records the writes through
   `KitLibraryContext.recordSetup` / `recordExperiment`. The model holds each write once it is recorded:
   - a setup joins `kitSetups` in name order, and the active setup is kept;
   - an experiment takes its id's place or is appended, with `nextId` past it, and its badge comes from
     `checkExperimentData`.

   The first failure stops the run with what was written, and the status is `Not saved to the kit — …`.
6. **Success.** `dataSplitOpt = None`. The status counts the written experiments (matched and created), the new setups,
   and the groups left unattached.

## Files

- `OpticalConstructor.Ui/TableAndElementRotationView.fs`: the `MatchByTemplate` case, the private `SplitWrite` DU, the
  planning fold and the handler.
- `OpticalConstructor.Tests/DataSplitModelTests.fs`: three new facts over step 319's fixture.
- `OpticalConstructor.Tests/KitWindowModelTests.fs`: the read-only fact also sends `MatchByTemplate`.

## Risks

- **Record-label inference.** `DataSplits` brings in the labels `key`, `points`, `setup`, `table` and `items`, so every
  record construction is annotated.
- **ELEV rules on new names.** The DU cases have two or more words and unique names, and nothing new takes a bare
  primitive in its signature.
- **The draft must be unchanged** (E1's `editingId` is kept). `commitOverSetups` clears `editingId`, so the planned
  collection takes the model's draft back.
