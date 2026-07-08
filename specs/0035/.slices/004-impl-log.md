# Impl-log — slice 004 (IMPLEMENT — CategoryEditor)

## Progress

- [x] Author `CategoryEditor.fs` (state + msg DU + apply + commit projection).
- [x] Register `CategoryEditor.fs` in the Domain fsproj (after `MaterialComplexityEditor.fs`).
- [x] Author `CategoryEditorTests.fs` (every message arm + acceptance + commit projection).
- [x] Register `CategoryEditorTests.fs` in the Tests fsproj (after `CategoryProxyTests.fs`).
- [x] Verify LF endings, no new warnings.

## Files modified

- ADD `Berreman/OpticalConstructor/OpticalConstructor.Domain/CategoryEditor.fs` — the pure edit
  model: `CategoryMsg` (BeginAddCategory / SetCategoryName / RemoveCategoryRow), `CategoryCommit`
  (AddCategory / RenameCategory / RemoveCategory — one per proxy verb), `CategoryEditState`
  (`original` snapshot + working `rows` + `editingRow` in-progress marker), `init`,
  `applyCategoryMsg` (Result-returning), and the `commit` diff projection.
- EDIT `Berreman/OpticalConstructor/OpticalConstructor.Domain/OpticalConstructor.Domain.fsproj` —
  registered `CategoryEditor.fs` after `MaterialComplexityEditor.fs`.
- ADD `Berreman/OpticalConstructor/OpticalConstructor.Tests/CategoryEditorTests.fs` — 11 pure
  unit tests (every message arm, the Begin-then-name acceptance, the blank-name `InvalidCategory`,
  the unknown-row `UnknownCategoryId`, and the add/rename/remove commit projection).
- EDIT `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj` —
  registered `CategoryEditorTests.fs` after `CategoryProxyTests.fs`.

## Testing state

Local diligence runs (advisory — the arc-runner gate engine is the sole gate authority, Invariant 6):

- **build** (full `dotnet build Berreman.slnx -c Release`): **0 Error(s)**, 93 warnings — every
  warning is pre-existing (MSB3277 WebView2/WindowsBase in the untouched Ui/Ui.Tests projects,
  MathNet `SYSLIB0051`, `Dispersion.fs` FS3873, `SeriesDataTests.fs` FS1125, `ChartWindow.fs`
  FS0044). Zero warnings originate in the two files this slice added.
- **constructor-unit-tests** (`dotnet test OpticalConstructor.Tests`): **440 passed / 0 failed**
  (the 11 new `CategoryEditorTests` included); the `CategoryEditorTests`-only filter ran
  **11 passed / 0 failed**.
- **unit-tests / ui-smoke / ui-tests**: untouched by this slice (it adds one Domain module and its
  Tests file; BerremanTests and the Ui.Tests project are neither referenced nor changed). Their
  counts cannot regress; deferred to the gate engine.

## Artifacts

No captured artifacts this round (a pure Domain + Tests slice, no logs/screenshots/traces).

## Gotchas

- **BeginAddCategory is the one non-deterministic arm** — it mints `CategoryId.create ()`
  (`Guid.NewGuid`), exactly as the slice requires. Tests therefore discover the minted id via
  `state.editingRow`, never a literal Guid; every other arm is a pure transform of its input.
- **Blank-name rejection lives at `commit`, not at each keystroke.** `SetCategoryName` accepts a
  transiently blank/whitespace name (so a name box may be cleared mid-edit); `commit` runs the
  shared `validateCategory` over each add/rename and surfaces the typed `InvalidCategory`. This
  matches how step 2/3's store applies `validateCategory` at the write boundary, and satisfies the
  acceptance ("a blank name MUST surface the typed `InvalidCategory`") through the commit path.
- **Ambiguity resolved — where the editor's own errors come from.** The slice says "Rejections
  (blank name) reuse the typed `CategoryError` from step 2." I reuse two `CategoryError` cases:
  `InvalidCategory` (blank name, at `commit`) and `UnknownCategoryId` (a `SetCategoryName` /
  `RemoveCategoryRow` aimed at an id the working rows do not hold, at `applyCategoryMsg` — the
  `MaterialComplexityEditor.NoSuchSegment` precedent). The store-only guards
  (`DuplicateCategoryId`, `BuiltInNotRemovable`, `CategoryStillReferenced`) are deliberately NOT
  re-enforced in the editor: they belong to `CategoryProxy`, which the host dispatches the projected
  intents to. In particular the editor lets a built-in row be renamed (built-ins ARE renamable per
  step 3) and lets a `RemoveCategory` intent be projected for any row — the proxy, not the editor,
  applies the removal block.
- **`commit` needs the `original` snapshot to classify a row.** The state keeps both `original`
  (the opened-over catalogue) and `rows` (the working set) so the diff can tell an add (new id) from
  a rename (changed record) from a remove (dropped id). A Begin-then-Remove of the same minted row
  nets to no intent.

## Changelog

- 2026-07-07 — Added the pure `OpticalConstructor.Domain.CategoryEditor` module (state + one message
  DU + a Result-returning `applyCategoryMsg` + the `commit` diff projection into add/rename/remove
  `CategoryCommit` intents), mirroring `MaterialComplexityEditor`'s message-DU discipline, over the
  seeded `MaterialCategory` catalogue. Reuses the step-2 `CategoryError` (`InvalidCategory` for a
  blank name at commit, `UnknownCategoryId` for an unknown row at apply). Added 11 pure
  `CategoryEditorTests` covering every message arm, the Begin-then-name acceptance, the blank-name
  rejection, and the commit projection. Full solution builds clean (0 errors, no new warnings); the
  constructor test project runs 440 passed / 0 failed.
