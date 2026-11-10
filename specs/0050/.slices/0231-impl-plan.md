# Step 0231 — impl plan

## Approach

Open kit from zip… joins the start screen's first row, with the extraction's progress row under it (spec 0050 S6,
obligation `S6-ZIP-VIEW`). Steps 200 and 230 already supply everything the view needs: `kitArchiveProgressView` and
`KitArchiveProgressIds` (step 200), the `zipPicker` / `archiveJobs` context seams, the `zipJob` model field and the
`OpenKitZip` / `CancelOpenKitZip` messages (step 230). This step only projects them.

1. `OpticalConstructor.Controls/UiIds.fs`, the start-screen group: `openKitZipButton`, `openKitZipProgressBar`,
   `openKitZipProgressText` and `openKitZipCancelButton`, each with a one-line doc.
2. `OpticalConstructor.Ui/KitStartView.fs`:
   - `kitStartActionsRow` gains `Open kit from zip…` after `Open kit…`, present while `m.zipJob.job.isRunning` does
     not hold. Its press asks `zipPicker.pickZipToOpenThen`, then `picker.pickThen`, then dispatches
     `OpenKitZip { zip; target }`; a cancel of either sends nothing (`startPick`).
   - A private `kitStartZipRows` puts `kitArchiveProgressView m.zipJob { bar; text; cancel } (fun () -> dispatch
     CancelOpenKitZip)` directly under the row while the extraction runs, nothing otherwise.
   - `kitStartView`'s doc names the new verb and row. Nothing else on the screen changes.
3. `OpticalConstructor.Ui.Tests/KitStartWindowTests.fs`, three facts:
   - `the Open kit from zip UiIds are the stable intent-named ids` (no trait): the four texts.
   - `a running extraction shows its progress bar, its count and Cancel, and hides Open kit from zip` (no trait):
     `zipJob` started with a recording handle and progressed by 3 of 4 files; idle model shows the button only.
   - `Open kit from zip asks for a zip, then a folder, and starts one extraction of that zip into that folder`
     (`ui-smoke`): recording zip and folder pickers, a deferred host, a recording of the messages the screen sends;
     press the button, then Cancel. The program mount gains an update parameter (one body, no copy) so the fact can
     see `OpenKitZip`'s zip and target.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Controls/UiIds.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/KitStartView.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/KitStartWindowTests.fs`

## Risks

- The zip button is a `verbButton` keyed by its id; it leaves the tree while the job runs and comes back fresh, so no
  stale press subscription.
- ELEV: the new members are intrinsic static members of `ConstructorUiId`; the new view function is private and takes
  our types only.
- ScrollExtentTests: the third verb sits in a horizontal StackPanel inside the existing `MinWidthPanel` + scroller,
  which already scrolls sideways.
