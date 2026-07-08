# State of the world — slice 004 (IMPLEMENT — CategoryEditor)

## Where we are

Slice 004 is the fourth step of arc 0035 (Part A — the open, editable, Guid-keyed category set).
Step 1 turned the material category into DATA (a seeded, `CategoryId`-keyed catalogue); step 2
DECLARED the mutating write-seam (`CategoryProxy` + `CategoryError` + `validateCategory`); step 3
IMPLEMENTED the real in-memory store behind that seam. This slice adds the pure, Avalonia-free
`CategoryEditor` Domain edit model that a future Category-manager window (spec A.2's
`<UICOMP:CategoryControls>`) projects: the state and message discipline for inline add / rename /
remove, plus the commit projection into the intents the host dispatches to `CategoryProxy`. It is
Domain-and-Tests only — no UI is wired this round.

## What's working

- Add the pure `OpticalConstructor.Domain.CategoryEditor` module mirroring `MaterialComplexityEditor`'s message-DU discipline (state record + one message DU + a `Result`-returning apply).
- Model the state as the opened-over catalogue snapshot, the working `MaterialCategory` rows, and the in-progress inline-edit marker (`editingRow`).
- Handle `BeginAddCategory` (mints `CategoryId.create ()` as a blank `UserCategory` / `SelectableOnCreate` row and marks it in-progress), `SetCategoryName`, and `RemoveCategoryRow`, reusing the typed `UnknownCategoryId` for a row the working set does not hold.
- Project the working rows against the snapshot with `commit` into add / rename / remove `CategoryCommit` intents, validating each add/rename through the shared `validateCategory` so a blank name surfaces the typed `InvalidCategory`.
- Add 11 pure `CategoryEditorTests` covering every message arm, the Begin-then-name acceptance, the blank-name rejection, and the commit projection — no window required.
- Build the whole solution clean (0 errors, no new warnings from the added files).

## Tests

Local runs are advisory diligence; the arc-runner gate engine is the sole gate authority (Invariant 6).

- `build`: verified locally — full `dotnet build Berreman.slnx -c Release` returns **0 Error(s)** and
  introduces no new `FS####`/`MSB####` warning from either added file. The 93 warnings are all
  pre-existing (MSB3277 WebView2/WindowsBase in the untouched Ui/Ui.Tests projects, MathNet
  `SYSLIB0051`, `Dispersion.fs` FS3873, `SeriesDataTests.fs` FS1125, `ChartWindow.fs` FS0044).
- `constructor-unit-tests` (baseline `constructor_unit_tests`): the full
  `OpticalConstructor.Tests` project ran **440 passed / 0 failed** locally (the 11 new
  `CategoryEditorTests` included); the `CategoryEditorTests`-only filter ran **11 passed / 0
  failed**. The slice only ADDS tests, so the `count_at_least` baseline cannot regress.
- `unit-tests`, `ui-smoke`, `ui-tests`: untouched by this slice (one new Domain module + its Tests
  file; BerremanTests and the Ui.Tests project are neither referenced nor changed). Deferred to the
  gate engine; their counts cannot regress.

## Architecture

- **A pure edit model, mirroring `MaterialComplexityEditor`.** State + one message DU + a
  `Result`-returning `applyCategoryMsg`, so the behaviour is unit-testable without a window and the
  future manager surface (Avalonia or WinForms) is a thin projection — the spec §0.4 "testable
  without a window" mandate.
- **The commit projection is a diff, not an accumulator.** The state keeps both the opened-over
  `original` snapshot and the working `rows`; `commit` diffs them into `CategoryCommit` intents
  (AddCategory → `addCategory`, RenameCategory → `updateCategory`, RemoveCategory → `removeCategory`).
  This keeps the message arms simple (they just mutate `rows`) and localizes the add/rename/remove
  classification to one pure function.
- **Editor errors vs store guards are separated.** The editor reuses only `InvalidCategory` (blank
  name, at commit) and `UnknownCategoryId` (an edit aimed at an absent row, at apply). The store-only
  guards — `DuplicateCategoryId`, `BuiltInNotRemovable`, `CategoryStillReferenced` — stay in
  `CategoryProxy` (step 3), which the host dispatches the projected intents to. The editor therefore
  lets a built-in be renamed (built-ins ARE renamable) and always projects a `RemoveCategory` intent;
  the proxy, not the editor, applies the removal block.
- **Blank-name validation at the write boundary, not per keystroke.** `SetCategoryName` accepts a
  transiently blank name; `commit` runs `validateCategory` — the same helper the store's
  `addCategory` / `updateCategory` use — so the rule lives in exactly one place.

## Deferred

- Wiring `<UICOMP:CategoryControls>` / `<UICOMP:CategoryEditorWindow>` and the Materials-bay
  `Categories…` verb onto this edit model — the UI surface is a later slice (spec A.2); this slice
  ships the pure Domain model and its tests only.
- The MSB3277 WebView2/WindowsBase conflict (spec §0.6) lives in the Ui/Ui.Tests reference chain,
  outside this slice's `touches` — its reference-level resolution belongs to a Part B / Ui slice.

## Gotchas

- **`BeginAddCategory` is the one non-deterministic arm** — it mints a fresh `CategoryId`
  (`Guid.NewGuid`), as the slice requires. Tests read the minted id back from `state.editingRow`
  rather than asserting a literal; every other arm is a pure transform.
- **A blank name surfaces `InvalidCategory` at `commit`, not at `SetCategoryName`.** The acceptance's
  negative half is proven by committing a Begin-added (blank) row and a name explicitly cleared to
  whitespace — both return `Error (InvalidCategory _)`.
- **`commit` requires the `original` snapshot.** Removing the snapshot from the state would make an
  add indistinguishable from a rename; a Begin-then-Remove of the same minted row correctly nets to
  no intent because it is absent from both `original` and `rows`.

## Changelog

- 2026-07-07 — Added the pure `OpticalConstructor.Domain.CategoryEditor` module (`CategoryMsg` /
  `CategoryCommit` / `CategoryEditState`, `init`, the `Result`-returning `applyCategoryMsg`, and the
  `commit` diff projection) over the seeded `MaterialCategory` catalogue, mirroring
  `MaterialComplexityEditor`'s message-DU discipline and reusing the step-2 `CategoryError`
  (`InvalidCategory` at commit, `UnknownCategoryId` at apply). Registered it in the Domain fsproj
  after `MaterialComplexityEditor.fs`. Added 11 pure `CategoryEditorTests` (every message arm, the
  Begin-then-name acceptance, the blank-name rejection, the add/rename/remove commit projection) and
  registered them after `CategoryProxyTests.fs`. Full solution builds clean (0 errors); the
  constructor test project runs 440 passed / 0 failed.

```yaml
gates:
  berreman_unit_tests: 119
  constructor_unit_tests: 440
  ui_smoke_tests: 83
  ui_tests: 310
```
