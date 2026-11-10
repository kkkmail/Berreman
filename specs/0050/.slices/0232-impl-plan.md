# Step 0232 — impl plan

## Approach

Wire the real zip dialog into the kit start screen so Open kit from zip… works end to end in the app (spec 0050 S6,
obligation `S6-ZIP-WIRE`; 001 §F, §G). Steps 197–200 and 230–231 already built the archive, the job host, the
messages, the button and its progress row. This step only threads a `TopLevel -> KitZipPicker` through the window and
the launcher, as `pickerFor` already is.

1. `OpticalConstructor.Ui/KitStartWindow.fs`: the constructor gains `zipPickerFor : TopLevel -> KitZipPicker` after
   `pickerFor`. The composed context's `zipPicker` becomes `zipPickerFor this`, replacing step 230's
   `KitZipPicker.none`. The type doc gains one line naming both dialogs.
2. `OpticalConstructor.App/Program.fs`, `LauncherWindow`: one private `zipPickerFor owner = KitZipPicker.ofWindow owner
   locations.exportsFolder`, passed to both start screens (Main, Inverse), so the dialog starts in the exports folder,
   where Send kit… writes. The launcher's own signature is unchanged. The doc gains one line. Program.fs opens
   `OpticalConstructor.Domain.KitArchive` and `OpticalConstructor.Ui.KitArchiveJob` for the type and its extension.
3. Every other `KitStartWindow(…)` passes `fun _ -> KitArchive.KitZipPicker.none` (or the opened name), assertions
   unchanged:
   - `ScrollExtentTests`' `the kit start screen leaves everything reachable`;
   - `KitStartWindowTests`' two window facts;
   - `WireUiFinalCompositionTests`' launcher fact.
4. `KitStartWindowTests`: the new `ui-smoke` fact `acceptance: Open kit from zip extracts a sent kit and opens it in the
   constructor of the screen's mode`.
   - A kit made by `openKitAt` under the fact's root, given `E1` through `writeExperiment` and closed, is sent through
     `KitArchiveProxy.create ().sendKit` into `<root>/exports/<name>.zip`.
   - A `KitStartWindow` over `windowScope ()` (a fresh app context with a fresh in-memory history), in
     `ForwardConstructor`. Its picker answers `<root>/received/k`, its zip picker answers the zip, and its
     `showConstructor` records the mode and the kit.
   - Press `openKitZipButton` at its centre. The real `onUiThread` host extracts on a worker. The fact runs
     `Dispatcher.UIThread.RunJobs ()` until the recorder holds a kit or ten seconds pass, with no fixed sleep.
   - Asserts:
     - the recorder holds `ForwardConstructor` and one read-write kit whose kit file equals the original's;
     - the received folder holds every sent file;
     - the screen is closed;
     - the history's one entry is the received folder.
   - The recorded kit is closed at the end by `withKitStartRoot`, before its root is deleted.

`LauncherTests` needs no edit: the launcher's signature is unchanged, and no fact presses the real dialog.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Ui/KitStartWindow.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.App/Program.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/KitStartWindowTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/ScrollExtentTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/WireUiFinalCompositionTests.fs`

## Risks

- **"Every sent file"** needs the zip's file list. `KitArchiveStore.zippedFiles`, or reading the zip with
  `System.IO.Compression` in the test, gives the entry names to compare against the received folder. I will use
  whatever `KitWindowTests` already does, and if that is private, the BCL directly.
- **The wait loop must not hang.** It is bounded by a `Stopwatch` at ten seconds and asserts afterwards, so a missing
  kit fails the fact rather than spinning.
- **Import dialog.** A kit made over the window's own `catalogueImports` has no snapshot rows, so `resolveAndShow`
  shows it at once, as the New kit acceptance fact relies on.
- **ELEV.** The new constructor parameter is a function of Avalonia and our types; the launcher's local helper takes a
  `TopLevel`, not one of our types (ELEV013 does not apply).
