# Step 0223 — IMPLEMENT — impl log

## Progress

- [x] `OpenedKit.items` read at open; an unreadable item file refuses the open and unlocks.
- [x] `TableAndElementRotationModel.kitItems`; `withKitContent` / `loadKit` / `ReloadKitChanges` set it.
- [x] `KitOpeningTests` fact added.
- [x] `KitWindowModelTests` fact added.
- [x] Local build / tests / mutants / elevation / invariants (development runs; the gate engine gives the verdict).

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Ui/KitOpening.fs`
  - `OpenedKit` gains `items : Item list` after `experiments`, with a one-line doc.
  - The private `openedOver` reads `context.proxy.readItems ()` right after `readExperiments ()`.
    - A refusal joins the existing failure branch: the kit is closed (its `.oc/lock` released) and the error is
      the answer, as `KitLibraryKitErr`.
    - Its doc names the items.
  - `withRebindings` is a `{ this with overlay = … }` copy, so it keeps `items` with no code change.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/TableAndElementRotationView.fs`
  - `TableAndElementRotationModel` gains `kitItems : Item list` right after `savedScenes`, the other kit content
    the load and reload set. It has a one-line doc; `initWith` sets it to `[]`.
  - The private `withKitContent` takes `items` after `experiments` and sets `kitItems`. Its doc lists the items.
  - `loadKit` passes `opened.items`. Its doc now says it loads the items.
  - `ReloadKitChanges` reads `proxy.readItems ()` after `readExperiments ()`.
    - An `Error` lands in the existing branch: the model and its `kitChanges` offer are kept, and the status reads
      step 187's "The kit could not be reloaded — …".
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitOpeningTests.fs`
  - New fact: `opening a kit reads its items, and an item file that does not read refuses the open`.
  - Private fixtures `openingCpl1` / `openingLp1`; `open OpticalConstructor.Domain.ItemModels`.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitWindowModelTests.fs`
  - New fact: `a loaded kit's items are the window's, and Reload reads an item another writer wrote`.
  - Private fixtures `windowCpl1` / `windowLp1`; `open OpticalConstructor.Domain.ItemModels`.

## Decisions

- **Field placement.** `kitItems` sits beside `savedScenes`, the kit content that `withKitContent` sets. That is
  "beside the kit's loaded content (step 187)".
- **Nested binds, no tuple.** `openedOver` and `ReloadKitChanges` nest one more `Result.bind`, matching the existing
  chain. A first draft carried `(experiments, items)` as a tuple; it was replaced before the first build.
- **Fixtures per class.** `KitItemFileTests` holds private CPL1 / LP1 values, but it compiles after both touched
  test files. This step's effects authorize edits to these four files only. Each touched class therefore builds its
  own CPL1 / LP1 through `ItemDraft.toItem` over `BuiltinCatalogue.itemModel`, as every item test class does. The
  GUIDs differ per class.
- **The opening fact goes slightly past its brief.** Beyond what the step names, it:
  - checks the refusal under both accesses;
  - checks that a fresh kit opens with no items;
  - deletes `Broken.item-json` and reopens read-write. This proves the refusal is that file's alone, and that the
    refused open left the kit free to lock again.
- **The window fact** also checks that the offer leaves `kitItems` alone until Reload, and that Reload clears the
  offer and the status.

## Testing state

These are development runs over the final source. The gate engine gives the verdict (Invariant 6). Logs are in
`specs/0050/.artifacts/0223/`.

- **build** (`dotnet build Berreman.slnx -c Release -nologo -v:m`)
  - First full build: two errors, both mine — the test file needed `open OpticalConstructor.Domain.ItemModels`
    for `DialPositive` / `DialNegative`. Fixed. Its only warnings were the two existing vendored MathNet
    `SYSLIB0051`.
  - Then exit 0 with **0 warnings, 0 errors** (`build.log`).
  - Final rebuild after the mutants were restored: exit 0, **0 warnings, 0 errors** (`build-final.log`). It
    recompiled `OpticalConstructor.Ui`: the dll is newer than the restored source.
- **constructor-unit-tests** (all discovered): **1323** passed, 0 failed, 0 skipped.
  - That is 1321 (checkpoint 222) + 2.
  - Both new identities are `Passed` in the TRX (`constructor-unit-tests.trx`).
  - Re-run over the final build, `KitOpeningTests` + `KitWindowModelTests`: 35/35 passed
    (`final-touched-classes.log`).
- **ui-smoke** (`Category=ui-smoke`): **237** passed, 0 failed, in 3 m 10 s, inside its 600 s attempt timeout.
- **ui-tests** (selected `OpticalConstructor.Ui.Tests.KitWindowTests`): **8** passed, 0 failed.
- **elevation** (`ASG_ARC_BASE_COMMIT` = arc origin `f395597`, ELEV004 excluded, over the final build):
  - `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, 197 s (`elevation.log`);
  - the run covered all 12 projects because `Berreman.slnx` changed since the arc origin;
  - suppressed findings stay at 1311, unchanged from step 0222 (measured, not gated).
- **The fourteen `invariant-s22-*` must-find-nothing greps:** each finds nothing.
- **The facts bite.** Each mutant was applied to the production source, rebuilt, and both classes were run. Each
  was restored byte for byte (checked with `cmp`). Logs are in `.artifacts/0223/mutants/`.
  - **m1:** `openedOver` answering `items = []` fails both new facts.
  - **m2:** `ReloadKitChanges` passing `model.kitItems` instead of the read items fails the window fact alone.
  - **m3:** `openedOver` swallowing an item read failure (`Ok (readItems () |> Result.defaultValue [])`) fails the
    opening fact alone.
  - A first m3 form (`Result.orElse`) did not compile and tested nothing. It was replaced; see
    `mutants/m3-build.log`, which holds the second, successful build.

## Artifacts

`specs/0050/.artifacts/0223/`: `build.log`, `build-final.log`, `constructor-unit-tests.log`,
`constructor-unit-tests.trx`, `ui-smoke.log`, `ui-tests.log`, `final-touched-classes.log`, `elevation.log`,
`mutants/` (originals, per-mutant build and test logs).

## Gotchas

- `DialPositive` / `DialNegative` live in `OpticalConstructor.Domain.ItemModels`, not in `Kit`. A test that builds
  an `ItemDraft` opens both.
- `Result.orElse` is not in the pinned FSharp.Core.
- No change to `changedKitFiles` was needed. `.item-json` is already a `KitFileKind.all` kind (step 123), and the
  window fact proves `LP1.item-json` is offered.
- Step 225's copy message must pass `model.kitItems`; that is its work, not this step's.
