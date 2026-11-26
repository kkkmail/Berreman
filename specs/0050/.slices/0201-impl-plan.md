# Step 0201 — impl-plan (IMPLEMENT: Send kit… as semantic messages of the kit window)

## Approach

The kit window's model gains the one send it runs, and four messages drive it. The step adds no button: step 202 adds
the Send kit… verb, its save dialog and its progress row to the kit bar.

1. `KitWindowShell` (`TableAndElementRotationView.fs`) gains three fields:
   - `zipPicker : KitZipPicker` (step 198);
   - `archive : KitArchiveProxy` (step 197);
   - `archiveJobs : KitArchiveJobHost` (step 200).

   `inert` holds `KitZipPicker.none`, an in-memory archive over a fresh `InMemoryArchiveDisk`, and
   `KitArchiveJobHost.inert`.
2. Every record expression that builds a shell in full becomes a record update of `KitWindowShell.inert`. There are
   four:
   - `ConstructorScene.mount` in `OpticalConstructor.App/Program.fs`;
   - `recordingShell` and `refusing` in `KitWindowModelTests.fs`;
   - `recordingShell` in `KitWindowTests.fs`.
3. `TableAndElementRotationModel.kitSend : KitArchiveJobState` comes after `kitChanges`, and `initWith` sets it to
   `KitArchiveJobState.idle`.
4. `TableAndElementRotationMsg` gains `SendKit of FileName`, `SendKitProgressed of JobProgress`,
   `SendKitEnded of Result<KitArchiveOutcome, KitArchiveError>` and `CancelSendKit`, after `KeepMyKitChanges`.
5. The update arms follow the slice word for word:
   - a scratch kit is refused;
   - a second send while one runs is ignored;
   - otherwise the shell's job host starts `archive.sendKit`.

   `SendKitProgressed` and `SendKitEnded` drive `kitSend` and `kitStatus`, and `CancelSendKit` cancels through the
   state. The read-only guard is not used: a send writes no kit file.
6. Six new facts in `KitWindowModelTests`, over real folder kits and the real `KitArchiveProxy.create ()`:
   - five use the `immediate` host over one message recorder;
   - the cancel fact uses a test-local deferred host.

   Zips go under `<fact folder>/exports`, the GUID folder that holds the kit folder.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Ui/TableAndElementRotationView.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.App/Program.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitWindowModelTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/KitWindowTests.fs`

## Risks

- **Opening `KitArchive` / `KitArchiveJob` in the big view file can capture record inference.** Their record labels
  include `kit`, `entries`, `zip`, `target`, `bar`, `text` and `cancel`. Build early; annotate or qualify where needed.
- **The real `writeKitZip` opens `<zip>.tmp` with `FileMode.Create`.** It needs the zip's folder to exist, so the facts
  create `exports` first, as a save dialog's folder always exists.
- **`KitWindowShell.inert` is a static property, so it builds a fresh record on every access.** It already did, so
  model equality is unchanged.
