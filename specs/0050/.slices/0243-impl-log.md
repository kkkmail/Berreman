# Step 0243 — impl log

## Progress

1. Read the protocol (base and WIRE_UI), the project prompt, the slice and its gates, AGENTS.md, and the 0242 state
   of the world. Surveyed `Program.fs` (`ConstructorScene`, both windows), `TableAndElementRotationModel.itemModels`,
   `OpenAddItemForm` / `AddItem`, `ItemsPanelView`, `KitLibraryContext.recordItem`, `ItemModelProxy.overlaidWith`,
   `TestAppScope` and `KitWindowTests`.
2. **Test first.** Added the fact
   `acceptance: an item added in a constructor window is a kit file with its model's snapshot, and the window reopened
   lists it` (trait `ui-smoke`) at the end of `KitWindowTests`. It has three helpers: `circularOffer`,
   `cpl1RowCaption` and `assertItemRowShown`. A full build passed (`0243-build-red.log`).
   - **Red, as expected** (`0243-red-run.log`): `AddItemOffer_33486eec-836a-4cbf-ac79-e3f14439ad4b_v1 must be in the
     window`. The real window's `itemModels` was still `initWith`'s empty in-memory store, so the form offered no
     item model.
3. **Wiring** (`Program.fs`):
   - `ConstructorScene.withAppContext` takes the `OpenedKit` and sets
     `itemModels = context.itemModels.overlaidWith opened.overlay` by record update, beside `catalogueImports`. It
     carries a one-line comment citing S7.2 and 001 §E.
   - Both windows pass `opened`.
   - Each window's doc gains one line: "Its Items panel reads the app's item models (spec 0050 S7.2)."
4. **Second run: red at the offer press**, at the Add button (`AddItemConfirmButton must be in the window`).
   - A temporary diagnostic showed the cause. The window is 820 × 770 and the offer box is 1314 px wide in the
     headless font, at x = 196. Its centre, x ≈ 853, is outside the window, so the press chose nothing.
   - **Fix (test, in scope):** a helper `pressShownPart`. It presses the centre of the box's part that lies inside
     the window and every `ScrollViewer` around it, which is where a user reaches it. It is used for the offer
     alone. Every other press in the class keeps `pressAtCentre` and `pressBox` unchanged.
   - The fact also asserts that the offer reads as chosen (positive fill) after the press.
   - The diagnostic was removed.
5. **Green** (`0243-green-run.log`): 1 succeeded.
6. **Mutation check:** with the `itemModels` line removed and the solution rebuilt, the fact is red with the same
   message as step 2 (`0243-mutation-build.log`, `0243-mutation-run.log`). `Program.fs` was restored from
   `0243-Program.fs.bak` and verified by SHA-256 (`0243-Program.fs.sha256`: OK).

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.App/Program.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/KitWindowTests.fs`

Both are in the slice's authorized `edits`. The other authorized test files (`AppContextTests`, `LauncherTests`,
`TableAndElementRotationTests`, `WireUi0040CompositionTests`, `WireUiCompositionTests`,
`WireUiFinalCompositionTests`) needed no edit; their assertions are unchanged.

## Testing state

These are development runs over the final tree, after `Program.fs` was restored from the mutation and the solution
rebuilt. No gate verdict is claimed (Invariant 6).

- **build** (`0243-build.log`): exit 0, `0 Warning(s)`, `0 Error(s)`.
- **constructor-unit-tests** (`0243-constructor-unit-tests.log`): `Passed!`, 1377 succeeded, 0 failed, 0 skipped,
  in 66 s. That is checkpoint 241's 1377, unchanged.
- **ui-smoke** (`0243-ui-smoke.log`): `Passed!`, 245 succeeded, 0 failed, 0 skipped, in 199 s (attempt timeout
  600 s). That is checkpoint 241's 244 plus the new fact, which is `Passed` in the TRX under its declared id.
- **ui-tests over the four selected classes** (`0243-ui-tests.log`): `Passed!`, 65 succeeded, 0 failed, 0 skipped.
  - By class (TRX): AppContextTests 17, KitWindowTests 15, LauncherTests 8, TableAndElementRotationTests 25.
  - That is step 0233's 63 plus step 0242's two non-smoke KitWindowTests facts. This step adds none, so the count
    is unchanged.
- **elevation** from the arc origin `f395597` (`0243-elevation.log`): exit 0,
  `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, in 199 s. Known findings: 0.
- **The fourteen s24 invariant greps,** untracked files included: none finds anything.
- **Red before green:** red before the wiring (`0243-red-run.log`), and red again with the wiring line mutated
  away (`0243-mutation-run.log`).

No run came near a deadline. No test is removed or renamed, and no assertion is removed or weakened.

## Artifacts

All under `specs/0050/.artifacts/`:

- `0243-build-red.log`, `0243-red-run.log`: the test-first build and its red run;
- `0243-build-diag.log`: the test rebuild of the press-helper fix;
- `0243-green-run.log`: the new fact green;
- `0243-Program.fs.bak`, `0243-Program.fs.sha256`, `0243-mutation-build.log`, `0243-mutation-run.log`: the mutation
  check and its restore;
- `0243-build.log`, `0243-constructor-unit-tests.log`, `0243-ui-smoke.log`, `0243-ui-tests.log`,
  `0243-elevation.log`: the final development runs.

## Decisions

- **The overlay goes through `withAppContext`.** The brief asks for the record update "beside `catalogueImports`",
  which is set in `ConstructorScene.withAppContext`. That private function gains the `OpenedKit` parameter, and both
  windows pass theirs. No public seam changed.
- **Doc wording.** The brief says the docs say "in no more words" that the Items panel reads the app's item models.
  Each doc gains exactly that sentence with its spec citation. The overlay's reason is the one-line comment at the
  record update.
- **The fact reads CPL1's id from the kit.** The window's `itemIds` is `ItemIdProvider.system` (step 239), so the id
  is not known in advance. The fact asserts the kit holds exactly one item, labelled CPL1, and looks its row up by
  that id.
- **Row caption asserted.** Beyond "shows CPL1's `kitItemRow`", both windows' rows are checked for
  `CPL1 — Circular polarizer (linear polarizer and retarder) v1 — revision 1`. The model's name resolves through the
  window's `itemModels`, which reads `(model not in the library)` without the wiring.
- **Press helper.** `pressShownPart` exists for the offer alone (see Gotchas). It does not replace `pressAtCentre`
  or `pressBox`.

## Gotchas

- **An offer box can be wider than the window in the headless font.** The circular polarizer's offer is 1314 px wide
  in an 820 × 770 `MainConstructorWindow`, starting at x = 196. A centre press lands at x ≈ 853, off-window, and
  chooses nothing. `ItemsPanelTests` never met this because it mounts the panel alone at 1000 px.
- **What the fact does not show.** The app's database also holds the circular polarizer model, so the fact shows
  the app's models reach the window, not the snapshot fallback for a model the machine lacks. That fallback is the
  `ItemModelProxy.overlaidWith` contract of steps 220 and 222, covered in `KitLibraryTests`.
- **The reopened window opens its kit through `openedAt`.** That is the class's wrapper of `KitOpening.openKitAt`,
  as the brief asks. The first window's close releases the kit's lock (`ConstructorScene.mount`'s `Closed` handler),
  so the second open succeeds.
