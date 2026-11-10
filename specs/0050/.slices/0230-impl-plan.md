# Step 0230 — impl plan

## Approach

Open kit from zip… lands as semantic messages of the start screen (spec 0050 S6), over the seams steps 197–200
already built: `KitArchiveProxy.receiveKit`, `KitArchiveJobHost` and `KitArchiveJobState`. No view change: step 231
adds the button and progress row, and step 232 the real zip dialog.

1. `KitStartView.fs`:
   - `KitStartContext` gains `zipPicker : KitZipPicker`, `archive : KitArchiveProxy` and `archiveJobs : KitArchiveJobHost`.
   - `KitZipOpening = { zip : FileName; target : KitFolder }` is declared before the model.
   - `KitStartModel` gains `zipJob : KitArchiveJobState` and `zipOpeningOpt : KitZipOpening option`, set to `idle` and
     `None` by `initKitStart`.
   - `KitStartMsg` gains `OpenKitZip`, `OpenKitZipProgressed`, `OpenKitZipEnded` and `CancelOpenKitZip`.
   - `updateKitStart` handles them as the brief says. The finished end reuses OpenKit's one path: `kitStartChosen` over
     `openExistingKitAt`, then `kitStartOpened`. That path is pulled into one private `kitStartOpenExisting`, which
     `OpenKit` also calls, so the open is not written twice.
2. `KitStartWindow.fs` composes `KitZipPicker.none`, `KitArchiveProxy.create ()` and
   `KitArchiveJobHost.onUiThread` over the captured `dispatch`. Each callback reads `dispatch` lazily in a lambda,
   because `>>` would capture the initial `ignore`.
3. The test contexts (`KitStartTests`, `KitStartWindowTests`) gain `KitZipPicker.none`, an in-memory archive and
   `KitArchiveJobHost.inert`.
4. `KitStartTests` adds five facts:
   - immediate host + recorders: the sent kit is extracted, opened read-write and the history bumped; a non-empty
     target is refused; a zip without `.kit-json` is told;
   - test-local deferred host: a cancel leaves no folder; a second open waits while the first runs, and the progress
     is shown.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Ui/KitStartView.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/KitStartWindow.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitStartTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/KitStartWindowTests.fs`

## Risks

- Record-label inference: `KitUnzipJob` also has `zip` / `target`, so every `KitZipOpening` expression is annotated.
- Equality of `KitStartModel` in the waiting fact: the context is reference-equal and the job state structural, so
  "unchanged" is a real `Assert.Equal`.
- The cancelled fact has to leave no target folder. The real `extractKitZip` removes the outermost folder it created,
  so `received/` goes too.
- ELEV: every new name must be unique (`KitZipOpening`, the four cases). The new fields are elevated types.
