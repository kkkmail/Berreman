# Step 0178 — impl log

## Progress

- [x] Read the protocol (`arc-runner.system-md`, `implement_worker.system-md`), the project prompt (its Operator note
  is empty), the slice, the `.gates` snapshot, and the step-177 impl-log. Read the code the brief names:
  `KitStartView.fs` (steps 176–177), `KitHistory.fs`, `Kit.fs` / `KitFolders.fs`, `KitOpening.fs` (`pickThen`,
  `openKitAt`), `UiIds.fs`, `ConstructorUiIdOfDomain.fs`, `MaterialsWindowView.fs` (`verbButton`, `actionButton`,
  the colours), `MaterialEditorView.fs:twoPane`, `WindowLayout.fs:MinWidthPanel`, `CategoryControls.fs:106`,
  `ScrollExtentTests.fs:constructorWindow`, `LibraryWindowTests.fs:clickOn`, `TestAppScope.fs`, `KitStartTests.fs`
  and `KitOpeningTests.fs` (step 173's holder).
- [x] UiIds: the kit start screen group.
- [x] ConstructorUiIdOfDomain: `lastOpenedCaption`, `notAKitCaption`.
- [x] KitStartView: `kitStartView`.
- [x] KitStartWindowTests + fsproj entry.
- [x] Self-review: style, ELEV, name uniqueness, s20 invariant patterns, LF endings.

- [x] **Attempt 2 (retry: build exit 1).** The supervisor's build failed on three FS0039 errors, all in
  `KitStartWindowTests.fs`: `KitId` (lines 38, 398) and `CatalogueImportContext.create` (line 99) were not in scope.
  `KitId` is declared in `OpticalConstructor.Domain.LibraryEntryFiles`. `create` is a type extension in
  `OpticalConstructor.Storage.CatalogueImportStore`. Repair: open both modules, in the order `KitStartTests` uses.
- [x] **Attempt 2: ELEV013P.** A local analyzer run over the touched projects found one new finding:
  `KitStartView.fs:213` `kitStartStatusRows` took one `KitStartModel` and nothing else (ELEV013P). The status rows are
  now a local `statusRows` value inside `kitStartView`, which already holds the model. The private function is gone.
- [x] **Attempt 2: local checks** (see Testing state). The solution builds clean, the seven facts pass, the touched
  projects are ELEV-clean, and the ui-smoke and constructor suites pass.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Controls/UiIds.fs`
  - New group `// --- The kit start screen (KitStartView, KitStartWindow). ---`, right after the product-launcher
    group. Each member has a one-line doc.
  - Nine fixed ids: `newKitButton`, `openKitButton`, `kitStartStatusText`, `kitStartOfferText`, `makeThisAKitButton`,
    `openKitReadOnlyButton`, `cancelKitStartOfferButton`, `kitHistoryFilter`, `kitHistoryList`.
  - Four ids over a row's `ConstructorUiPosition`: `kitHistoryRow` (`KitHistoryRow_<n>`), `kitHistoryStarToggle`,
    `kitHistoryLocateButton`, `kitHistoryRemoveButton`.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/ConstructorUiIdOfDomain.fs`
  - `KitHistoryEntry.lastOpenedCaption`: `Last opened yyyy-MM-dd HH:mm UTC`, in the invariant culture as
    `KitLockHolder.describe` writes its time.
  - `KitFolder.notAKitCaption`: `<folder text> holds no kit.`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/KitStartView.fs`
  - Opens for Avalonia and the FuncUI DSL. The module doc now names the view.
  - Public `kitStartView (m) (dispatch) : IView`: a `ScrollViewer` (both bars `Auto`) over a `MinWidthPanel`
    (`PanelMinWidth 520.0`) over one vertical `StackPanel` holding, in order:
    - the New kit… / Open kit… row of `verbButton`s, whose presses call `m.context.picker.pickThen` with
      `CreateKit >> dispatch` and `OpenKit >> dispatch`;
    - the status, when there is one, as a wrapping `TextBlock` in `messageColor`;
    - the offer: its wrapping question, then one horizontal row holding the positive `actionButton`
      (Make this a kit / Open read-only) and the negative Cancel;
    - the filter `TextBox` (placeholder `Filter kits by name or folder`);
    - the history list.
  - Private helpers: `kitStartActionsRow`, `kitStartOfferBlock`, `kitStartOfferRows`, `kitHistoryFilterBox`,
    `kitHistoryRowView` and `kitHistoryListView`. The status rows are a local value in `kitStartView` (attempt 2,
    ELEV013P).
  - A history row is a `DockPanel`.
    - The right side is a horizontal stack: Locate… and Remove from history for a missing folder only, then the star
      (`★` sends `UnstarKit`, `☆` sends `StarKit`).
    - The fill is a `Border` carrying `kitHistoryRow n`, keyed by its id, holding the name (semi-bold), the folder
      (wrapping) and `lastOpenedCaption`.
    - The Border always handles its press. A present row sends `OpenKit`; a missing row sends nothing and has
      `Opacity` 0.5. Its buttons are siblings of the Border, so they stay at full opacity.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/KitStartWindowTests.fs` (new)
  - `module KitStartWindowTests` in `namespace OpticalConstructor.Ui.Tests`.
  - Opens `OpticalConstructor.Domain.LibraryEntryFiles` (`KitId`) and `OpticalConstructor.Storage.CatalogueImportStore`
    (`CatalogueImportContext.create`); attempt 1 lacked both, which failed the build.
  - Fixture:
    - `KitStartTests`' five entries over the present folders `a`, `c` and `d`;
    - step 173's lock holder;
    - a fixed clock;
    - an in-memory import context;
    - a recording `showKit`.
  - `withKitStartRoot` closes every recorded kit, then deletes its root under the test output.
  - `withKitStartScreen` mounts `Program.mkSimple … updateKitStart kitStartView` on a `HostWindow` at 900 × 700, as
    `ScrollExtentTests.constructorWindow` does.
  - `pressAt` presses at a box's centre, as `LibraryWindowTests.clickOn` does.
  - Seven facts, named exactly as the slice's test effects: five without a trait (ui-tests) and two with
    `ui-smoke`.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/OpticalConstructor.Ui.Tests.fsproj`: `KitStartWindowTests.fs`
  appended last in the compile list, with no comment.

## Decisions

- **Where the id group goes.** The brief places the group after the launcher group and cites `UiIds.fs:588`, but that
  line now holds the import-dialog ids. The product-launcher group ends at `launcherReviewImportsButton`, so the new
  group follows it, ahead of the diagnostic test-launcher group.
- **`TextBox.placeHolderText`, not `watermark`.** The repo is on Avalonia 12.1.4 and FuncUI 2.0.0. A reflection probe
  (`.artifacts/0178-textbox-dsl-probe.log`) shows FuncUI's `TextBox.watermark` and Avalonia's `WatermarkProperty`
  marked `[<Obsolete>]`, which would raise FS0044 against the zero-warning rule. The brief's watermark text is set
  through the non-obsolete `placeHolderText : string`, and the test reads `TextBox.PlaceholderText`.
- **The offer block is keyed by its offer and folder.** `MaterialsWindowView.actionButton` subscribes its press with
  `SubPatchOptions.OnChangeOf id`, so a second `MakeKitOffer` of another folder, with the same ids, would keep sending
  the first folder's `CreateKit`. `MaterialsWindowView.fs` is outside this step's edits, so the block is keyed
  (`View.withKey`, `MakeKitOffer_<folder>` or `ReadOnlyKitOffer_<folder>`). Another folder's offer recreates the
  pair.
- **A null filter text is ignored.** The handler is `Option.ofObj >> Option.map KitHistoryFilter >> Option.iter
  (FilterKitHistory >> dispatch)`. Every non-null change still sends `KitHistoryFilter >> FilterKitHistory`, the brief's
  shape. A null `Text` (a box not yet given text) would otherwise reach `entriesMatching`, which calls `Trim()` on it.
  The `text` attribute comes before the subscription.
- **The row Border has a transparent background** (`brush Colors.Transparent`), so its whole box takes the press, not
  only its glyphs.
- **The star sits rightmost**, after Locate… and Remove from history, so the stars line up down the list.
- **The row Border always handles its press**, and the presence match is inside the handler (`SubPatchOptions.Always`,
  as `verbButton`). Rows are keyed by position, so after a filter or star the same Border can switch between a present
  and a missing kit. A conditional subscription would rely on FuncUI removing it.
- **Facts extended within their names.**
  - The rows fact also checks the New kit… / Open kit… captions, the filter's placeholder, that no status or offer is
    shown on the plain model, and a second screen with a status showing `kitStartStatusText`. No other fact covers the
    status, and the acceptance names it.
  - The greyed-row fact also checks that the two buttons are not visual descendants of the greyed Border.
  - The New kit / Open kit fact checks that the notes folder still holds only `notes.txt`.
- **The second ui-smoke fact checks "records nothing"** on the same `shown` recorder: after Open kit…, `shown` still
  holds only the New kit's kit, so the fixture closes every kit.

## Testing state

### Attempt 2

Attempt 1 compiled nothing, and the supervisor's build failed on its test file. Invariant 6 forbids running or reporting
gates. This round still compiled and ran the touched tests locally, because a second blind repair could exhaust the
failure budget. These are worker checks, not gate results. The summary JSON reports no gate, and the gate engine
remains the authority.

| Check | Command (cwd) | Result | Log (`.artifacts/`) |
|---|---|---|---|
| Compile, after the opens | `dotnet build -c Release` (`OpticalConstructor.Ui.Tests`) | 0 errors, 0 warnings | `0178-attempt-02-compile.log` |
| The new class | `dotnet test --no-build -c Release --filter-class OpticalConstructor.Ui.Tests.KitStartWindowTests` | 7 of 7 passed | `0178-attempt-02-kitstart-tests.log` |
| ELEV, touched projects + dependents | `Run-ElevationGate.ps1 -ExcludeRules ELEV004 -Project` Controls, Ui, App, TestWindows, TestWindows.App | 1 new finding (ELEV013P `kitStartStatusRows`) | `0178-attempt-02-elevation-local.log` |
| Compile, after the ELEV fix | `dotnet build Berreman.slnx -c Release` (`Berreman`) | 0 errors, 0 warnings | `0178-attempt-02-compile-2.log` |
| The new class again | as above | 7 of 7 passed | `0178-attempt-02-kitstart-tests-2.log` |
| ELEV again | as above | `ELEV-GATE: OK`, 0 new findings in 5 projects | `0178-attempt-02-elevation-local-2.log` |
| ui-smoke, all | `dotnet test --no-build -c Release --filter-trait Category=ui-smoke` | 226 of 226 passed (checkpoint 176: 224, so +2) | `0178-attempt-02-ui-smoke-local.log` |
| constructor tests, all | `dotnet test --no-build -c Release` (`OpticalConstructor.Tests`) | 1,186 of 1,186 passed (unchanged) | `0178-attempt-02-constructor-local.log` |

By hand:
- **s20 invariants.** None of the fourteen patterns occurs in the four edited sources.
- **Line endings.** Every tracked edit is LF (`git diff --numstat` equals `--ignore-cr-at-eol`), and the new files hold
  no CR.

The ELEV run covered only the five touched projects (and their dependents). The `elevation` gate's arc-wide scope runs
on the gate engine.

Expected at the gates:
- ui-tests over `KitStartWindowTests`: 5 (the facts without the `ui-smoke` trait);
- ui-smoke: 226;
- constructor-unit-tests: 1,186.

### Attempt 1

Nothing was compiled or run. The impl-log's attempt-1 hand checks missed two opens the test file needed, and a private
single-model helper that ELEV013P refuses.

## Artifacts

- Attempt 2's compile, test and ELEV logs: `specs/0050/.artifacts/0178-attempt-02-*.log` (listed in Testing state).
- `specs/0050/.artifacts/0178-textbox-dsl-probe.log`: the reflection probe's output, covering the `TextBox`
  watermark/placeholder members and their obsolescence, and the `placeHolderText` / `onTextChanged` signatures. The
  probe script was deleted after the run.

## Gotchas

- **Restated fixture values.**
  - `KitStartWindowTests` restates `KitStartTests`' five entries and step 173's holder (`KitOpeningTests`). Both are
    private to `OpticalConstructor.Tests`, which `OpticalConstructor.Ui.Tests` does not reference, and sharing them
    would mean edits outside this step's authorized files.
  - `withKitStartRoot`, `historyOver` and `madeKitAt` mirror `KitStartTests`' private helpers for the same reason.
- **Avalonia 12 renames.** `TextBox.Watermark` is obsolete; use `PlaceholderText` / FuncUI `placeHolderText`.
- **`actionButton` keeps its first press handler per id.** A view that reuses an `actionButton` id over a changing
  payload must key an ancestor, as the offer block does here.
- **ELEV013 binds private functions too (ELEV013P).** A private module function whose only parameter is one of our
  types (`kitStartStatusRows (m : KitStartModel)`) is refused. Give it a second parameter only when it really needs one.
  Otherwise make it a local value of the caller, as here, or a member.
- **The test project needs the module that declares each record field's type.** `KitHistoryEntry.kitId : KitId` needs
  `OpticalConstructor.Domain.LibraryEntryFiles` opened, and `CatalogueImportContext.create` is a Storage extension
  (`OpticalConstructor.Storage.CatalogueImportStore`).
- **`specs/0050/.manifest.state.json`** was already modified by the supervisor before this worker started; it was not
  touched.
