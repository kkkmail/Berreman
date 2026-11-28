# 008 — Implementation log: the kit window, one page and one state at a time (plan `007`)

2026-11-27. The operator ruled on plan `007`'s decisions and asked for the whole plan:

> Here are the comments and decisions. Agree means agree on recommendations. D1. Agree with Kit. D2. Agree. D3. Just
> rename into `.optics-kit` with no renaming inside. D4. Agree. D5. Agree. D6. Agree. D7. Agree. They must be on disk in
> full and be self-dependent D8. Agree. D10. Agree. D11. Agree. D12. Agree. D13. Agree. D14. A new setup is a source and
> detector (as before). D15. Agree. D16. Agree. Proceed with the implementation in full. Keep an implementation log in
> the next NNN MD. Make sure to drive through UI once you are done with the development. Remember that this is a
> hardened F# and so you must be able to unit test everything with proper proxying. Run ELEV gate once at the end.
> Remember that ELEV004 is excluded. The rest of the gates / tests is on you.

## The rulings as applied

| Decision | Ruling | Applied as |
|---|---|---|
| D1 | Kit | The last button of the page row is `Kit` |
| D2 | a page | Kit opens a page under the row, like every page |
| D3 | rename only | `.kit-json` is renamed `.optics-kit`; its fields keep their names and meaning (`created`, `modified`), and its `schemaVersion` stays 1 |
| D4 | the matrix is the list | — |
| D5 | Setup grid… on the Experiments page | — |
| D6 | chart the Mueller elements | — |
| D7 | shipped catalogue samples, "on disk in full and self-dependent" | The two samples are catalogue entries; each new kit snapshots every library version it uses, so it opens against an empty library |
| D8 | same setup name, any revision | — |
| D9 | not ruled | `007` says the arc applies the recommended default unless the operator rules otherwise: `SwapSlot` and the Select bay retire in kit windows |
| D10 | 1 nm and 1° steps | — |
| D11 | leaving asks | — |
| D12 | Show discards the draft | — |
| D13 | a study renames only without fit runs | — |
| D14 | **a new setup is a source and a detector, as before** | overrides `007`'s "the light source alone" |
| D15 | ` # <N>` for copies only | — |
| D16 | no drawn table under an experiment | — |

## How the work keeps the reference builds stable

The write messages of spec 0050 001 §P (`AddItem`, `SetParameterState`, `CreateSetup`, `SaveSetup`, `RecordExperiment`,
`ExpCommit`, `CreateStudy`, `ExcludePoint`, `CorrectSetup`, `GenerateSetupGrid`, …) stay what a test and AT-1 / AT-2
send. The new pages' Save and Create send them too, once, from a draft. The pages change; the commands that write the
kit do not.

## Phase 3 — the kit is a file (`.optics-kit`) and the last folder (done first: a stored shape)

- **The rename (D3).** `KitFile.extension` is `.optics-kit`; every source, test, schema description and AGENTS.md
  mention of `.kit-json` was changed with it. The file's fields and `schemaVersion` are unchanged, as ruled.
- **The committed kits.** `data/optics-mueller/.kit-json` and `data/absorbing-biaxial-noisy/.kit-json` became
  `.optics-kit`, byte for byte. AT-1's and AT-2's "the built … kit equals the committed kit file by file" facts both pass
  against them (2 of 2), so the committed files are exactly what the builds write.
- **Opening by file.** `KitFolderQuestion` gains `ChooseKitFile`. `KitFolderPicker.ofWindow` answers it with a file
  dialog filtered to "Optics kit (*.optics-kit)" and returns the file's folder. Open kit…, Locate… and Copy experiments
  from another kit… ask it; New kit… and the unpack target still ask for a folder.
- **The last folder (`006-10`).** `KitFolder.parentFolderOpt` is the folder above a kit. `UserEnvironment` keeps it under
  the picker purpose `kit-parent` (`rememberKitParent`), in the `lastFolders` map the photo picker already used.
  `KitFolderMemory` is the seam: `ofEnvironment` at the composition root, `fixedAt` in tests. Every kit dialog starts
  there, the Kits folder until a kit is recorded. The picker records every kit it answers, and the start screen records
  every kit it opens by any route, the history included.
- **Ordering.** `.optics-kit` sorts after `.oc` where `.kit-json` sorted before it; four expected listings in
  `KitProxyTests` and `KitArchiveTests` were reordered.
- **Tests added.** `KitStartTests` (opening records the folder above; a non-kit records nothing; a root kit has no
  parent), `DataFilePickerFolderTests` (the pure memory, and the environment-file memory across a restart),
  `KitStartWindowTests` (Open kit asks `ChooseKitFile`, New kit `ChooseKitFolder`).
- **Results.** `OpticalConstructor.Tests` whole (it runs in under a minute and every kit test reads the renamed file):
  1916 of 1916. Ui classes `DataFilePickerFolderTests`, `KitStartWindowTests`, `KitWindowTests`, `LauncherTests`,
  `AppContextTests`, `WireUiFinalCompositionTests`, `ScrollExtentTests`: 131 of 131.

## Phase 1 — the page rules

- **The page state.** `PageMode` (`BrowsingPage | EditingPageEntry | CreatingPageEntry`), `PageLeave`, `DraftState`, and
  one draft per page (`ItemEditDraft`, `SetupCreation`, `ExperimentEditDraft`, `DataEditDraft`, `StudyEditDraft`,
  `RenameDraft`) live in the workbench model.
- **One guarded update.** The recursive core is `updateWorkbench`; `updateTableAndElementRotation` wraps it. With a
  changed draft, leaving the page, switching constructor, Cancel and Close kit set `leaveQuestionOpt` instead of acting;
  `ConfirmPageLeave` drops the draft and goes on, `KeepPageDraft` keeps it (D11). A status changed by a message is
  stamped with the page it lands on (`kitStatusStep`, 1.7).
- **Generic messages.** `BeginPageEdit`, `BeginPageCreate`, `CommitPageDraft`, `CancelPageDraft`, `ConfirmPageLeave`,
  `KeepPageDraft`, `CopyToClipboard`, `BeginRename` / `SetRenameText` / `CommitRename` / `CancelRename`. Save and Create
  send the existing §P write messages once, so AT-1 and AT-2 build exactly as before.
- **Renames.** `RenameSetup` (re-points every experiment over any revision: `Experiment.withSetupRenamed`),
  `RenameExperiment` (re-points every study: `Study.withExperimentRenamed`), `RenameStudy` (refused while a fit run names
  it, D13). New files are written before the old one is deleted; a change of case alone deletes first, as relabelling an
  item already did.
- **Copy (5.4).** `SetupName.copyNameAmong` gives `<base> # <N>`; `CopySetup` writes the copy as revision 1.
- **Shared controls** (`OpticalConstructor.Controls/PageControls.fs`): `ClipboardProxy` (`ofTopLevel`, `inert`),
  `ControlCaption.csvField` (the one RFC 4180 field writer), `EntryMenu` / `EntryRenaming`, `GridTable` with `csvText`,
  `PageHelp`, and the builders `copyableText`, `copyableLine`, `entryToggle` (tooltip, Copy name, Rename…),
  `gridTableView` (gridlines, Copy as CSV) and `pageHelpView` (an `Expander`).

## Phase 2 — the kit window's frame

- **The page row** (`KitWindowView.pageRowView`): Items · Setups · Experiments · Inverse|Main · (Inverse: Data · Unknowns ·
  Plan · Fit · Results) · Kit, a wrapping row at the top; the old left flow strip (`FlowStripView.fs`) is deleted.
  `ConstructorFlowStep` gains `KitFlowStep` and loses `PublishFlowStep`.
- **The Kit page** (D1, D2): the kit's caption, folder, created and last-saved stamps, then Send kit…, Show in Explorer,
  Copy experiments from another kit…, Reload changes, Close kit and Guide…, the copy source while one is open, the naming
  template (read-only with Edit), and Publish: the kit's fit runs as dynamic buttons, the chosen one's preview and
  promotions under it (`StudyPublishView.publishRunRows`, `ChoosePublishRun`).
- **Banners.** The reload offer shows on every page; a running send's progress on every page but Kit, which shows it
  among its verbs.
- **The table's buttons** (`setupTableView`, replacing `mainView`): the ribbon holds Add, Rotate, Move, Render and
  Details. The Select and Experiments bays, the Library… and Materials… buttons and the element menu's Choose… are gone
  from kit windows (D9); `BayNames.selector` and `BayNames.experiments` are deleted. A read-only table selects, pans and
  zooms the view only: no right-click menu, and the wheel never turns or zooms an element.

## Phase 4 — Items

- **The page** (`ItemsPanelView.itemsPanelView`): New item…, then the items as dynamic buttons (`kitItemRow`), the
  chosen one open under its row; a second press closes it (`SelectKitItem` toggles).
- **Read-only details**: Edit, model, dial, notes, photo with its thumbnail, the parameter grid (a `GridTable`, Copy as
  CSV), the revisions.
- **Edit in place**: label, notes, photo (Choose photo… / Remove photo stage `ItemPhotoDraft`; the file is attached at
  Save), the parameters (a pressed name opens the editor in a grid row right under it; Set changes the draft,
  `SetItemDraftParameter`). Save is `withItemDraftRecorded`: one `recordKitItemEdit`, so one write.
- **New item…** opens `ItemModelChooserWindow`, a window of its own as large as its screen allows, with every item
  model, light source, polarizer and sample the add form offered, a filter, and Choose / Cancel; the choice lands in the
  creation form (model, Choose another model…, label, mounting, Create / Cancel). The 220-px offer list is deleted.
  - **Deviation from `007` 4.3:** the plan named the Library window's Select mode. The Library window lists library
    entries and no item models, and the add form's offers are item models, sources, polarizers and samples together, so
    a chooser over exactly those offers (`ItemModelChooserView`, pure update `updateItemModelChooser`) serves the request
    without adding item models to the Library's catalogue tree.

## Phase 5 — Setups

- **The page** (`SetupsPageView`): New setup, then the setups as dynamic buttons; pressing the chosen one unchooses it
  (`DeselectSetup`). Under the chosen one: its slot line (`Setup.slotLine`, from `RoledSlot.chipCaption`), its revision,
  Edit and Copy…, and its table, read-only.
- **Edit**: the table editable with the ribbon and the slot chips; Save is `SaveSetup`, Cancel asks when
  `tableWrittenOver active <> active`.
- **New setup (D14)**: a name and the table seeded as before, a light source and a detector (`mainSeedElements`); Create
  is `CreateSetup`. **Copy…**: `<base> # <N>` prefilled; Create is `CopySetup`.
- **The slot bar** puts an item into its slot of the edited table (`AssignSlot`); `SwapSlot`, its domain
  `Setup.swapTarget` / `SetupSwap`, Record experiment's button and Setup grid… are gone from it. Selecting a setup no
  longer sets a "Loaded setup" status: the chosen button shows it.

## Phase 6 — Experiments

- **The page** (`ExperimentsPageView`): the header names the chosen setup; with none, New experiment is not shown and
  the page says to choose a setup in Setups first. The experiments are dynamic buttons (`ChooseExperiment`); under the
  chosen one: View chart ↗ (withheld with its reason when the cached chart has no series), Edit, Remove (asks first), and
  its setup (name, revision, slot line, no drawn table, D16), scan, source wavelength, role, session, notes and data.
- **The editor** (`ExperimentControls.experimentEditorView`) is the bay's steps 1–4, Add variable, Add block and the
  draft's chart; its list, Add / Update / New and bookkeeping moved to the page, and their state fields and handlers were
  deleted.
- **Edit**: the editor over the experiment's own revision (`ExpEdit`), Export data, role chips, session, notes, and a
  re-point to another setup with its audit note (moved here from the Data page; `ExperimentEditDraft.repointOpt`, applied
  at Save through `CorrectSetup`). Save sends `ExpCommit`, then the bookkeeping once, then the re-point.
- **New experiment**: the editor over the chosen setup, Export data, the note for its name, Create. With experiments
  already over that setup name (D8), Create asks "… already use setup X. Create another?" — OK records it, Cancel returns,
  Show drops the draft and chooses the first (D12). Create sends `RecordExperiment` (it names with the note).
- **New experiments from a setup grid…**: the grid panel as the page's Creating; its Close ends it.
- **The chart cache** (`ExperimentChartCache`, `withChartCachedFor`, `chartAvailabilityOf`): choosing an experiment on
  the Experiments or Data page computes its chart once, through `experimentChartHook` (wired to `chartForExperiment`, the
  existing forward-hook pattern); View reads it.

## Phase 7 — Data

- **The matrix is a table** (`DataStepView.experimentMatrixBlock`): a `GridTable` with gridlines and Copy as CSV, one
  column per Prep slot ("Prep 1"), one per object slot ("Obj 1": the item at its angle), then one per analyzer
  combination ("Analyzer: LP2"); a line above it says what rows and columns are. Each cell holds its experiments as
  dynamic buttons.
- **The matrix is the list (D4).** Pressing an experiment chooses it and opens its data in a spanning row right under its
  matrix row (`ChooseDataExperiment`); pressing it again closes it. The separate Data files list is gone; experiments
  the matrix cannot place get the same rows below it.
- **The panel**: its name, badge and file, View chart ↗ (withheld with its reason) and Edit, and its points chart. In
  Edit: Attach data file…, the reason, a row per point with Exclude / Include over `DataEditDraft`
  (`DraftExcludePoint`, `DraftIncludePoint`), Save writing the exclusions once (`withExperimentDataRecorded`). An attach
  during the edit starts its draft over, as a new mapping numbers its points anew.
- **No setup on this page** (`006-5a`): the review's re-point list moved to the experiment's Edit; the status line is
  the page's own.
- **No data** (`006-5e`): the new `AttentionCommand` fill and a "⚠ " prefix.
- **Split a file by key…**: a held split is the page's Creating; Apply split commits, Cancel drops.

## Phase 8 — Unknowns, Plan, Fit, Results

- **Unknowns** (`StudyUnknownsView`): New study, then the studies as dynamic buttons (Rename withheld while a fit run
  names the study, D13). Under the chosen one: Edit, its experiments, its parameters (a `GridTable`; Scope and Link are
  text) and its sessions (a `GridTable`). In Edit, every study verb changes `StudyEditDraft` instead of writing; the
  offers (Add experiment, Open material), a pressed parameter's editor in a spanning row right under it, with its scope
  and link choices there alone (`006-6d`, `006-6e`); the gains are choices; Save writes the changed items, then the
  study, once each. New study: name and "Experiments to fit:", each with its role and badge.
- **Plan, Fit, Results** head with the page help and `studyContextRows` ("Study: …", or "Choose a study in Unknowns
  first"); `studyBarView` is gone.
- **Fit** (`StudyFitView`): New fit run…, the running fit's progress, then the study's runs as dynamic buttons, newest
  first, the chosen one's summary and Open results under it. The form (`FitRunDraft`) holds the checks, the mode and the
  calibration use; `StartFitRun` writes them into the study only when they changed, then runs the stage (003 D4 keeps
  them in the study).
- **Results**: every table is a `GridTable` with Copy as CSV (`fitValuesGrid`, `fitCorrelationGrid <n>`).

## Phase 1, continued — tooltips, Options, help and guides

- **Tooltips (`006-11`).** `CommandHelpTexts.fs` (`ConstructorUiId.helpOpt`) holds the help of every static command,
  keyed by its id up to its first `_`; the command builders set it as `ToolTip.Tip` (shown on a withheld command too)
  and `AutomationProperties.HelpText`. A dynamic button's tooltip is its caption.
  - **Deviation from `007` 1.5:** the plan made the help a required builder argument. The text lives in one catalogue
    instead, enforced by `AutomationContractTests` rather than the compiler: 170 call sites stay as they are, and the
    texts can be read and reviewed together.
- **The delay.** `Preferences.toolTipDelay` (`ToolTipDelay`, 0–5000 ms, 1000 by default) in `environment.json`, whose
  schema gains the field and goes to version 2.0 (an older file falls back to the defaults, the existing no-migration
  rule). `ToolTipDelayStyle` sets `ToolTip.ShowDelay` on every control from the App's styles, and replaces itself when
  the option changes.
- **Options** (`OptionsView`, `OptionsWindow`, opened by the Launcher's footer Options…): the delay, Save offered only
  when it changed and reads, Cancel asking first; `OptionsProxy.ofEnvironment` writes the file and applies the style.
- **Help (`006-1l`).** `PageHelpTexts.fs` (`ConstructorFlowStep.help`) and `PageControls.pageHelpView`, an `Expander`
  at the top of every kit page and of the Options window.
- **Guides (`006-9`, `006-14`).** `GuideView` / `GuideWindow`: "Build a direct problem" and "Build an inverse problem",
  each step naming its page and the command it presses; opened by Guide… on the Launcher and on the Kit page (the
  constructor's own guide first).
- **Closing a window with changes (D11).** The App's kit window keeps its last model; closing with a changed draft is
  cancelled and sends Close kit, which the page rules turn into "Discard your changes and close the kit?".

## Phase 6, continued — D6: a Mueller polarimeter's chart

- **Why `absorbing-biaxial-noisy` had no chart** (confirmed in code): its detectors are items of the "Mueller polarimeter"
  model; `detectorKindAt` reads a kind only from a bound library detector entry and fell back to Intensity, so every run
  was charted as unpolarized intensity with no polarizer on the path, refused as "No analyzer present".
- **Now** `detectorChartingAt` charts a Mueller-polarimeter item's capture as the path's Mueller matrix
  (`ExperimentRun.RunStep.productOf`, the matrix the study reads as `PolarimeterDetectorReadout`): 15 series of its
  elements over m11 ("m12/m11" …), y-label "Mueller elements / m₁₁", and the no-polarizer refusal does not apply.
- **Measured** (`measuredMuellerSeries`): the kept Mueller observations as the same 15 elements over m11, one set per
  value of the angle the chart does not vary, so points of different configurations are never joined; they share the
  model's axis.

## Phase 9 — the two new samples

- `SampleIds.multilayerGlassPairsId` and `SampleIds.langasitePlateSiliconId`; the built-in catalogue gains
  "Quarter-wave glass n = 1.75 / 1.50 multilayer (100 pairs) on glass" (100 × [glass n = 1.75, 600/1.75/4 nm; glass
  n = 1.50, 600/1.50/4 nm] on n = 1.50 glass) and "Langasite plate on silicon (10 µm, dispersive)" (a 10 µm langasite
  plate on silicon). They were written through `CatalogueProxy.writeCatalogue` by a one-off console over the built
  assemblies (not committed): only the manifest changed and the two entry files were added; the other 72 files were
  rewritten byte for byte.
- Tests brought to them: `PropagationTests` (their legacy systems, the 200-film count, the plate), `MaterialProxyTests`
  (glass n = 1.75 is referenced by the stack too), `LibraryFacetsTests` and `BuiltInCatalogueIntegrityTests` (13
  samples).

## Tests so far

(The final figures are under "Results" at the end, and in `009`.)

- `OpticalConstructor.Tests`, whole: **1916 of 1916** after the page rules, the renames, the catalogue samples and the
  retired swap. Retired with their behaviour: the five `SwapSlot` facts and `Setup.swapTarget`'s fact; rewritten: the
  page-row facts of `KitWindowModelTests`, the chip fact of `SetupModelTests`, `ActivateSetup`'s status. Added: the copy
  name, an item's draft (Save withheld, leave asks, Yes / No, one write), a status on its page, Copy and Rename with
  re-pointed experiments and a refused clash, and an FsCheck property: no sequence of page messages without Save or
  Create writes the kit, and a question waits only while a page edits.
- `OpticalConstructor.Ui.Tests`: being brought to the new pages. `UiDriver.editableTableView` mounts the editable setup
  table where tests mounted `mainView`.

## Phase 3, continued — what D9 retired with the Select bay

Superseded code is deleted in the change that replaces it, so the whole Choose… chain went, not only the bay:

- **Table side** (`TableAndElementRotationView.fs`): the Select bay's state and handlers (`libraryState`,
  `selectorOffer` / `SelectorOffer`, `libraryHandlers`, `selectRows`, `allowedEntryIds`, `selectableEntryIds`), the
  confirm-gated bind (`RequestBindValueId`, `ConfirmBindValueId`, `CancelBindValueId`, `pendingEntry`), the Choose…
  session (`SelectSession`, `activeSelect`, `selectStatus` and its status row, `withActiveSelectCancelled`,
  `SelectSessionStarted` / `Ended`), the launcher's `openLibrarySelectWindow`, the strip buttons' `OpenMaterialsWindow`
  / `OpenLibraryWindow` messages with the model's `launchers` field, and the model's `quickPickThreshold` and
  `selectWindowModality`. `BindValueId` and `BindValueIdTo` stay: the direct and targeted binds the tests and the
  scene builders send; a vanished target is now a plain no-op.
- **`LibraryControls.fs`** is deleted (only the Select bay drew it).
- **The Library window's Select mode** (`LibraryWindowView`'s `mode`, `ConfirmLibrarySelect`, `CancelLibrarySelect`,
  `RetargetLibrarySelect`, `LibrarySelectDismissed`, the Select / Close pair and banner, `LibraryWindow.Retarget`, the
  context's `requestClose`): nothing opened it any more. The Materials window keeps its Select mode (the sample editor's
  material picks), so `WindowMode` and `WindowLauncher`'s Select opens stay.
- **The quick-pick threshold setting** (`WorkbenchSettings.QuickPickThreshold`, its error case, `AppConfig`'s key).
- **35 ids** no control used any more were deleted with their help entries; four help keys that named no id prefix
  were corrected (`ExperimentCandidate`, `DataSplitGroupSetup`, `StudyFitMode`, `StudySessionGain`).
- **Restored:** an experiment's setup-correction trail (spec 0050 S9.3) had left the screen with the Data page's
  re-point; it now shows under the experiment's details and in its Edit (`dataCorrectionTrail`).

## Tests brought to the pages

- `UiDriver` mounts the editable table (`editableTableView`), the Setups and Experiments pages, and
  `creatingExperimentOver`; `ScrollExtentTests.constructorWindow` is now the kit window itself, with `onBenchSetup`,
  `filledExperiments`, `creatingExperiment` and `editingSetup` as its states, so the scroll law covers the pages.
- Facts of retired behaviour were deleted with it: the Select bay's and the Library window's Select mode's (about 40),
  the pending bind's, the Choose… session's, the strip buttons' and the threshold's. Facts of surviving behaviour were
  moved to its new place: the shared-scope and single-instance window facts open through the Launcher's seam; the
  re-point fact drives the experiment's Edit; the direct-problem acceptance copies the setup and edits the copy
  (`Bench # 1`) instead of the swap's `Bench (2)`; Record experiment's facts create on the Experiments page and answer
  the same-setup question; the flow strip's facts are the page row's; the kit bar's are the Kit page's.
- `ItemsPanelTests` checks the model chooser (`ItemModelChooserView`: groups, filter, Choose withheld until chosen) and
  the creation form; `KitWindowTests`' add-item acceptance chooses in the chooser's own window.
- The desktop facts (`DesktopTests`, `DesktopSurvey`) drive the pages by id: the Kit page's verbs, the setup's Edit for
  the ribbon, the Data page's Edit for Attach, Remove under the chosen experiment.
- `AutomationContract` gains the rules of 1.4 and 1.5: every `CommandButton` and `ChoiceToggle` has a tooltip and help
  text, and a text carrying an id is a `SelectableTextBlock`; a new fact checks every page of both constructors against
  the contract and for its Help panel.

## What the tests found in the product

Bringing the UI tests to the pages found four defects, each fixed where it lives and covered by a fact:

- **Edit on data-less data.** The Data page's Edit opened with no draft when the chosen experiment had no data yet, so
  Attach data file… was not offered; it now opens with the experiment's exclusions, or none (`withPageEditBegun`).
- **Apply split left the page in Creating.** `MatchByTemplate` wrote the split and dropped it but kept `CreatingPageEntry`,
  which withholds every matrix entry; it now returns the Data page to Browsing, as Cancel does (1.2, 7.5).
  `DataSplitModelTests` gains the fact.
- **Copy… withheld Create.** A copy's prefilled `<base> # <N>` counted as an unchanged draft, so Create stayed withheld
  until the name was edited; a copy is a new setup, so Create is offered at once (5.4). `SetupModelTests` says so.
- **The re-point needed its setup in the kit.** Not a product defect: the fact's fixture listed `Bench B` in memory
  only, and `CorrectSetup` resolves the setup and its items in the kit, as it must; the fixture now writes them.

And one test-host matter: Avalonia's tooltips are popups, and with 1.5's tooltip on every command a popup could sit over
the next headless click. `TestApp.quietToolTips` turns the tooltip service off in every headless session
(`HeadlessSession.run`); the contract facts read `ToolTip.Tip` and `HelpText` directly, so nothing is left unchecked.

## Phase 10 — the desktop suite and the re-survey

- **`DesktopSession`** answers the kit's file dialog: `openKitSeeing` types the kit's `.optics-kit` into the real file
  dialog (a file dialog reads its name box only from typing). It also gains, moved from `DesktopSurvey` rather than
  copied: `Screen` (GDI capture), `kitContent`, `changedFiles` and `textsUnder`. New: `addCommittedKit` /
  `addCommittedKitUnder` (the copy loop the copy fact had inlined), `clipboardText` (the Win32 clipboard) and
  `workAreaOf` (the window's monitor).
- **The `005` survey's facts brought to the pages.** The kit's verbs are on the Kit page. Copy experiments asks for the
  source's `.optics-kit`. View chart ↗ comes under a chosen experiment. A Data entry opens its data under its matrix
  row. The photo is chosen in the item's Edit and attached at Save. And the calibration use is the New fit run…
  form's: choosing it writes nothing, and a started run records it.
- **The dead id `dataReviewButton`** went: no control carried it once the matrix became the list. Its id fact now
  checks `experimentMatrixEntry`.
- **`DesktopPagesSurvey.fs`**: one fact per comment of `006`, each named by its comment and driven by id on the real
  app. The report is `009`, which also lists what the survey found and fixed: Edit and Copy… moved onto the setup's
  line, the guide's steps exposed to UI Automation, the Options, Guide and library chooser windows put under
  `ScrollExtentTests`, and the chooser's offers made to wrap.
- **The Library window's backup fact and the headless mouse.** In a full `OpticalConstructor.Ui.Tests` run, and in a
  five-class batch that reproduced it every time, every other headless mouse press in that window reached no pointer
  handler of the pressed button. The hit test landed on the button, Restore's Invoke pattern opened its panel, and the
  next press arrived with a click count of 2. Alone, the class passed 72 of 72. Three driver changes were tried and
  reverted, since none changed the outcome: re-reading the press's centre after every render tick, a release before
  each press, and a second press. The fact now drives its buttons through their automation patterns, which
  `AGENTS.md` ranks above a click at the centre; the batch then passes 145 of 145. The root cause in the headless
  input is not established.

## The ELEV gate

The operator asked for the gate once, at the end. It took three runs, because the first found what the change had
added and each fix had to be proven.

- **First run: 23 findings**, all in code this change added. ELEV016, ELEV017 and the wire guard found nothing.
  - **ELEV013 / ELEV013P (15), one-parameter helpers.** The page heading had been written five times
    (`pageHeading` in the Experiments and Setups pages, `fitHeading`, `studyHeading`, `kitHeading`) and the page line
    twice (`pageLine`).
    They became `ControlCaption.pageHeading` and `pageLine` in `KitPageFrame`, one look for every page; the Kit
    page's headings lose 4 px of top margin.
  - The rest of the 15 became members on their types. On `Experiment`: `correctionTrailRows`. On `Item`:
    `revisionsView`. On the model: `prefitRows`, `studyContextRows` and `kitLibraryContext`, the last formerly
    `libraryOf`.
  - The page rules' helpers in the update's recursive group became members of the model, defined before the group:
    `droppedPageDraftWith`, `committedNewExperimentWith` and `committedPageDraftWith`. Each is handed the update,
    since each sends messages through it. They are `internal`, as a private member of a type extension in the type's
    own file is private to the type.
  - **ELEV011 (3).** `RenameSetup`, `RenameStudy` and `CopySetup` carried a tuple of twin names. They carry the
    records `SetupRename`, `StudyRename` and `SetupCopy` now.
  - **ELEV001 (2).** The item notes' re-wraps became `ItemNotes.caption`, beside `ExperimentNotes.caption`, and the
    private `ControlCaption.toItemNotes`, beside `toExperimentNotes`.
  - **ELEV010 (1).** `ToolTipDelay.tryParse : string → …` became `ControlCaption.toolTipDelayOpt`, on the source type.
  - **ELEV003P / ELEV010P (2).** The free `refusedSince` answered a bare `bool`. It became a member, which still
    answered one: see the second run.
- **Second run: 1 finding**: the `refusedSince` member still answered a bare `bool` beside the model (ELEV010). It is
  `writeOutcomeSince`, answering `PageWriteRefused | PageWriteAccepted`.
- **Third run: OK**, 0 new findings in 13 projects (`-ExcludeRules ELEV004`, 249 s). The known-findings list,
  `.elevation-analyzers.json` and the analyzers were not touched.
- **Not ours to change here:** a fresh compile of the vendored MathNet reports two `SYSLIB0051` warnings
  (`MathNetNumerics/Numerics/Exceptions.cs`, lines 24 and 77: the obsolete serialization constructors). They predate this
  change, which does not touch MathNet. They show only when MathNet recompiles, so both full builds of this change
  listed none. Clearing them means editing the vendored library: a question for the operator.

## Phase 9, continued — AT-3 and AT-4

- **`ScriptCurves.fs`** (`OpticalConstructor.Ui.Tests`) holds `FsxAcceptanceTests`' comparison, moved, not copied:
  `scriptData`, `assertSameCurve`, the multilayer system, the incidence and wavelength ranges, the langasite thickness.
- **AT-3 and AT-4** (`KitAcceptanceTests`) build their kits through the pages' messages: Items (Xenon, LP, Detector T,
  Detector R and the sample), the naming template `{obj1}-{note}` through the Kit page's Edit, each setup through the
  Setups page's New setup (D14's source and detector, then the polarizer and the sample, each assigned by its chip), each
  experiment through the Experiments page's New experiment with its note (`incidence`, `wavelength`), capturing R and T.
  Point counts follow D10: 90 (0–89°), 201 (500–700 nm) and 351 (250–600 nm).
- **The facts per kit:** the build and its curves, the built kit equals the committed one file by file, and the committed
  kit opens read-only against an empty library, its snapshots resolving, draws its curves and writes nothing.
  `SetupSaveLoadTests` activates every committed kit's setups, the two new ones included.
- **The committed kits** `data/multilayer-thin-film/` and `data/langasite-on-silicon/` are those builds' output, `.oc/`
  aside. The builds leave an empty `data/` folder, which git cannot hold; the read-only facts run against the copies
  without one, as a checkout has them.
- **AT-4 and the langasite gyration (deviation, recorded).** The plan checks the built-in "Langasite film on silicon"
  against `LangasiteOnSilicon.fsx`. The built-in langasite keeps a constant ρ (g11 = g33 = 1e-5) where the script's
  `langasiteOpticalProperties` uses the published gyration law: a standing decision (`MaterialComplexityTests`, spec 0040
  step 004, 0050 steps 10 and 51), so the catalogue was not changed. The constructor's curves disagreed with the
  script's by 2.7e-5 at 250 nm, past the 1e-6 of the comparison. AT-4 now checks two things: the constructor draws the
  script's own systems over the catalogue's materials within 1e-6 (`ScriptCurves.overCatalogueMaterials`), and those
  curves lie within `langasiteGyrationGap` = 5e-4 of the script's (measured: film R 3.00e-4, T 3.03e-4; plate R 8.7e-6,
  T 9.0e-7). AT-3's materials are the script's, so it is compared with the script directly.

## Deviations from `007`, in one place

Each is recorded where it was made, with its reason:

- **4.3, the library chooser** (Phase 4): a window of its own over the add form's offers, not the Library window's
  Select mode, which lists no item models.
- **1.5, the help texts** (Phase 1, continued): one catalogue (`CommandHelpTexts.fs`), held by `AutomationContractTests`,
  not a required builder argument.
- **D3, `.optics-kit`'s fields**: the file is renamed and nothing inside it, so `modified` keeps its name and meaning. It
  is stamped when the kit file itself is saved, not by every kit write as `007` 3.2 proposed with the rename to
  `lastSaved`.
- **9.1, AT-4's comparison** (Phase 9, continued): exact over the catalogue's materials, and within
  `langasiteGyrationGap` of the script, since the built-in langasite keeps a constant ρ.
- **The backup fact's presses** (Phase 10): through the buttons' automation patterns, the headless mouse's lost presses
  not root-caused.

## Results

Of the working tree after the ELEV gate's fixes; `009` has the same table with the desktop survey:

- `dotnet build Berreman.slnx -c Release`: succeeded, 0 errors, no warning but the exempt NU1701 / NU1902 advisories.
- ELEV gate (`-ExcludeRules ELEV004`): OK, 0 findings in 13 projects.
- `OpticalConstructor.Tests`: 1911 of 1911.
- `OpticalConstructor.Ui.Tests`, whole: 1028 of 1029, 1 skipped (AT-2's `Explicit` five-seed ensemble).
- `ui-smoke`: 273 of 273.
- `BerremanTests.MuellerReconstructionTests`: 25 of 25; the whole solver suite was not run, as no solver project changed.
- `OpticalConstructor.Desktop.Tests`, real desktop: DESKTOP_RESULT.
- Every changed file is LF. Nothing is committed by the agent: the operator commits.
