# Step 0163 — implementation log

## Progress

- Read the system prompts, the project prompt, the slice, manual 013 and the gate snapshot.
  Checkpoint 161 (step 0162) in `.checkpoints-json` is `constructor_unit_tests: 1137`, and the
  latest `ui_smoke_tests` checkpoint is 224 (checkpoint 155).
- Census before the shape change. `git grep` / ripgrep of `selectWindowModality =`,
  `thicknessBucketCap =` and `WorkbenchSettings.defaults` under `Berreman/` finds exactly three
  `WorkbenchSettings` record literals:
  - `defaults`;
  - the two facts in `WorkbenchSettingsTests.fs`.

  Other literals share field names but belong to different records, so they stay unchanged:
  - `TableAndElementRotationView.fs:508-509`, the workbench model;
  - `LibraryWindow.fs` / `MaterialsWindow.fs`, the window contexts;
  - the `LibraryWindowContext` fixtures in `LibraryWindowTests.fs`, `LightSourceEditorTests.fs`
    and `WorkbenchInfoAndCaptureTests.fs`.

  Every other consumer, the slice's census files and `TestAppScope.fs` included, passes
  `WorkbenchSettings.defaults` or reads a field.
- Name uniqueness: `tryPositiveCount` appears nowhere under `Berreman/` or in STL's sources
  (read-only grep). `ConfigKey.kitHistoryLimit` is new.
- Accessibility: a `member private` on an optional extension is private to the enclosing module,
  so other `type … with` blocks of `WorkbenchSettingsDoors` can call it. Precedent:
  `KitFolderStore.fs` declares `member private this.readKitText` in `type FileName with`, and
  `type KitProxy with` uses it.
- Edited the Domain, App and test files (details below).
- Line endings: `git diff --numstat` equals `--ignore-cr-at-eol` for all four files (20/22, 3/1,
  8/0, 23/0). `file` reports no CRLF.

### Attempt 02 (elevation retry)

- The gate engine's elevation run failed with one finding:
  `ELEV010P AppConfig.fs:91:29 - [private surface] tryPositiveCount mixes bare primitive(s) int, string with elevated types`.
  Every other gate in the roster passed, and review was green.
- Diagnosis from STL's analyzer source, read-only (`Analyzers/NoPrimitiveBesideElevated.fs`,
  `ElevationAnalyzer.fs`):
  - ELEV010 inspects private members too (`trySurfaceOf` and `EnforcePrivateSignatures`, under
    the `P` code). The slice's premise that a private member "adds no public signature" is true,
    but the rule still checks private signatures.
  - The rule reads only the curried parameters and the return (`Tree.signatureTypes`), not the
    receiver. Only F# unions and records outside FSharp.Core count as elevated, and a generic
    parameter has no type definition, so it does not count. The one elevated type in
    `(create : int -> Result<'Setting, WorkbenchSettingsError>) : Result<'Setting, string>` was
    `WorkbenchSettingsError`.
  - ELEV003P exempts members (`m.IsMember`), and ELEV009 reads only the happy path `'Setting`.
- Repair: the shared body never reads the intrinsic's refusal (`| Error _ ->`), so its type is
  generic. The new signature is
  `member private this.tryPositiveCount (create : int -> Result<'Setting, 'Refusal>) : Result<'Setting, string>`.
  - It now names none of our types, so nothing remains for ELEV010P to report.
  - The four doors, their texts, the tests and every acceptance obligation are unchanged.
  - A one-line doc beside the member says why the refusal is generic.
- Line endings: `git diff --numstat` equals `--ignore-cr-at-eol` (AppConfig.fs 21/22), and there
  is no CRLF.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/WorkbenchSettings.fs`
  - `WorkbenchSettings` gains `kitHistoryLimit : KitHistoryLimit` as its last field.
  - `defaults` gains `kitHistoryLimit = KitHistoryLimit.defaultValue`.
  - The record doc now reads "5 / 100 / 8 / modeless and the kit history limit's 100 appear
    exactly once, here".
- `Berreman/OpticalConstructor/OpticalConstructor.App/AppConfig.fs`
  - `ConfigKey.kitHistoryLimit = "KitHistoryLimit"` (`[<Literal>]`) follows
    `thicknessBucketCap`. Its one-line doc reads "appSettings int (001 §F): the most kits the
    history keeps. Default 100 — `KitHistoryLimit.defaultValue`."
  - `WorkbenchSettingsDoors` gains `type AppSettings.ConfigValue with` and
    `member private this.tryPositiveCount (create : int -> Result<'Setting, 'Refusal>) : Result<'Setting, string>`.
    The refusal is generic as of attempt 02; it was `WorkbenchSettingsError` in attempt 01, which
    tripped ELEV010P.
    This is the single count-door body:
    - an invariant-culture `Int32.TryParse`, then `create`;
    - `not a positive count: '<text>'` and `not a whole number: '<text>'`, the texts unchanged.
  - The `tryCreate` doors of `QuickPickThreshold`, `TreeAutoBuildThreshold` and
    `ThicknessBucketCap` are now one line each, `v.tryPositiveCount <Type>.tryCreate`. The three
    copies of the body are deleted, and the `configValue` renders are unchanged.
  - `type KitHistoryLimit with` adds two members:
    - `static member tryCreate (v : AppSettings.ConfigValue) : Result<KitHistoryLimit, string>`,
      through `tryPositiveCount`;
    - `member this.configValue : AppSettings.ConfigValue`, rendered as the other doors render.
  - `loadWorkbenchSettings` adds
    `kitHistoryLimit = readSetting provider (AppSettings.ConfigKey ConfigKey.kitHistoryLimit) defaults.kitHistoryLimit KitHistoryLimit.tryCreate _.configValue`
    beside the other keys. A missing or refused key is logged and written back as 100. The single
    `provider.trySave ()` saves it with the rest. No doc in the file counted the keys, so no doc
    changed for that.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/WorkbenchSettingsTests.fs`
  - `defaults equals the record constructed directly from the per-type defaults`: the record
    carries `kitHistoryLimit = KitHistoryLimit.defaultValue`. (change_expectation)
  - `a settings record assembled from validated values carries the validated thresholds`: the
    record carries `kitHistoryLimit = KitHistoryLimit.tryCreate 250 |> okOr KitHistoryLimit.defaultValue`,
    and the fact asserts `250`. (change_expectation)
  - New `[<Fact>]` `the default settings carry a kit history limit of 100`. It checks that
    `WorkbenchSettings.defaults.kitHistoryLimit` equals `KitHistoryLimit.defaultValue` and that
    its `.value` is 100. (create)
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/AppConfigTests.fs`
  - New `[<Fact>]`
    `the count doors read a positive whole number and refuse anything else, the kit history limit's included`.
    (create) It checks:
    - `KitHistoryLimit.tryCreate (ConfigValue "250")` answers a limit of 250 whose
      `configValue` is `ConfigValue "250"`;
    - `"0"` answers `Error "not a positive count: '0'"` and `"ten"` answers
      `Error "not a whole number: 'ten'"`, for the same four doors: `KitHistoryLimit`,
      `QuickPickThreshold`, `TreeAutoBuildThreshold` and `ThicknessBucketCap`.
- Census only, unchanged: `OpticalConstructor.Ui/AppContext.fs`, `AppContextTests.fs`,
  `LauncherTests.fs`, `TableAndElementRotationTests.fs`, `WindowLauncherTests.fs`,
  `WireUi0040CompositionTests.fs`, `WireUiCompositionTests.fs` and
  `WireUiFinalCompositionTests.fs`. Each reads the record or its `defaults` and builds no literal.

## Testing state

As Invariant 6 requires, this IMPLEMENT worker ran no gates. That means no `dotnet build`, no
`dotnet test`, no analyzer and no invariant. The gate engine runs build, elevation,
constructor-unit-tests, ui-smoke, ui-tests and the thirteen s19 invariants after exit. The only
commands run were read-only: `git grep` / ripgrep, `git diff --numstat`, `file`, and reads of
`.checkpoints-json`.

Attempt 02 likewise ran no gate. The supervisor's measured elevation diagnostic (one ELEV010P)
was repaired from the analyzer source, and the gate engine re-runs the roster.

Expected test effects:

- constructor-unit-tests: one new Fact and two changed facts, so +1 case, from checkpoint 161's
  1,137 to 1,138.
- ui-tests over the selected classes: one new Fact in `AppConfigTests`, so +1.
- ui-smoke: unchanged.

Every slice requirement is covered:

- (1) the record field, the `defaults` entry and the record doc;
- (2a) the `ConfigKey` literal and its doc;
- (2b) the shared private `tryPositiveCount`, the three doors rewired with the same texts, and the
  `KitHistoryLimit` door with its `configValue`;
- (2c) `loadWorkbenchSettings` reads the key;
- (3) the census, verified unchanged;
- (4) both changed facts and both new facts.

## Artifacts

None. The step produced no run artifact.

## Gotchas

- The slice cites `AppConfig.fs:64-109` for `WorkbenchSettingsDoors`, `:117` for `readSetting`
  and `:136` for `loadWorkbenchSettings`. At HEAD `3832e4a` these sit at `:70-116`, `:123` and
  `:188`, so the edits were placed by content. It also cites `WorkbenchSettings.fs:87-93` and
  `:95-101`, which are `:102-108` and `:110-116` after step 162's additions.
- Each door now passes `<Type>.tryCreate` as a first-class value. That method group is
  overloaded: the intrinsic takes an `int`, the door takes a `ConfigValue`. The parameter type
  `int -> Result<'Setting, 'Refusal>` picks the intrinsic, because the `int` domain is what
  disambiguates. `loadWorkbenchSettings`
  already relies on the same type-directed resolution in the other direction (`ConfigValue ->`).
- `appsettings.json` (App and Ui.Tests) is not in this step's edits, so the key is not added
  there. On first start `readSetting` writes the default 100 back, as it does for any missing
  key.
- `specs/0050/.manifest.state.json` was already dirty. It is supervisor-owned, so it was left
  alone. No git mutation was made.
- Deviation from the slice text (attempt 02): the slice prescribes
  `create : int -> Result<'Setting, WorkbenchSettingsError>`. That shape breaks ELEV010P, and
  AGENTS.md says the analyzers win. The refusal is now the generic `'Refusal`, which the body
  never reads. This stays inside the declared effects, because only `AppConfig.fs` changed.
