# Impl-log — slice 005 (ADD_COMPONENT — UICOMP_XDUO_0005 CategoryControls)

## Progress

- [x] Author `CategoryControls.fs` (domain-free Row / State / Handlers / UiIds / view).
- [x] Author `CategoryControlsTests.fs` (pure contract + headless structure proof).
- [x] Register both files in their `.fsproj`s.
- [x] Write state-of-the-world.

## Files modified

- ADD `Berreman/OpticalConstructor/OpticalConstructor.Controls/CategoryControls.fs` — the domain-free
  `CategoryControls` FuncUI component: `Row` (`categoryId` / `name` / `isBuiltIn`), `State`
  (`rows` + `blockMessage`), `empty`, `Handlers` (`addCategory`, `setCategoryName`, `saveCategory`,
  `removeCategory`, `cancelCategory`), a `[<RequireQualifiedAccess>] module UiIds` of `[<Literal>]`
  intent-named ids + `row*` derive helpers, and `view` (Add verb, the named `CategoriesList` of
  per-row panels, the inline block-message slot).
- ADD `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/CategoryControlsTests.fs` — a pure
  contract test (empty state + the stable UiIds literals/derived ids) and a
  `[<Trait("Category","ui-smoke")>]` headless structure proof.
- EDIT `OpticalConstructor.Controls.fsproj` — registered `CategoryControls.fs` after
  `SampleLibraryControls.fs`.
- EDIT `OpticalConstructor.Ui.Tests.fsproj` — registered `CategoryControlsTests.fs` after
  `WireUiCompositionTests.fs`.

## Testing state

Per the ADD_COMPONENT family Invariant 6 (**act only; run no checks**), this worker authored the
component + its headless test and registered them, but did NOT run the build/test gates locally — the
arc-runner's deterministic gate engine runs the step's gates (`build`, `unit-tests`, and the
constructor/ui-smoke/ui-tests roster) after the worker exits and is the sole gate authority.

Authoring diligence to keep the gates green:
- The component is a line-for-line shape copy of the known-good, compiling `MaterialsControls` /
  `LibraryControls` domain-free precedents (same imports, the same `AttrBuilder<_>.CreateProperty`
  AutomationId idiom, the same `Border.onPointerPressed` + `SubPatchOptions.OnChangeOf` click seam).
- Every FuncUI attr used has a direct in-repo precedent (`StackPanel.margin`, `ScrollViewer.maxHeight`,
  `TextBox.width` / `.onTextChanged`, `Border.horizontalAlignment`, `AttrBuilder<_>.CreateProperty`
  over `AutomationProperties.AutomationIdProperty`).
- The test is a shape copy of `MaterialsControlsTests` (the same `matchesId` / `isPresent` / `clickOn`
  headless helpers and the shared `HeadlessSession.run` driver); it adds only tests, so the
  `count_at_least` baselines for `constructor_unit_tests` / `ui_smoke_tests` / `ui_tests` cannot regress.
- Both new files verified LF (no CRLF churn); the component imports no domain type (domain-free).
- `commit_ready: true` — the single slice requirement (the component + its headless acceptance test,
  registered) is fully addressed this round; nothing deferred to a "round 2".

## Artifacts

None — this slice ships a component + its headless test only (no captured logs, screenshots, or traces).

## Gotchas

- **Per-row ids vs the seven literals (the one ambiguity).** The slice lists `CategoryNameBox`,
  `RemoveCategoryButton`, `CategorySaveButton`, `CategoryCancelButton` as fixed intent-named ids but
  ALSO mandates "an inline name box per row" and per-row Add/Rename/Remove verbs — a per-row control
  cannot carry a single fixed id. Resolved (the interpretation most consistent with the surrounding
  code) by making each of those four a `[<Literal>]` **base intent name** that the `row*` helpers
  prefix with the row's category id (`CategoryNameBox_<id>`, …) — exactly the `MaterialsControls.row`
  / `LibraryControls.entry` collision-safe precedent. `CategoriesList` / `AddCategoryButton` /
  `CategoryBlockMessage` are true singletons and carry their literal id directly. The structure test
  therefore "finds every UiIds control" by asserting the three singletons directly and the four
  per-row families for each known row.
- **Removed, not greyed.** A built-in row OMITS the Remove verb from its child list entirely (the
  `MaterialsControls` view-only-Edit precedent) rather than disabling it; the test asserts the
  built-in row's `rowRemoveButton` id is ABSENT from the tree, which a greyed/hidden button would fail.
- **AutomationId (not Name) on every per-row control.** Row membership changes when the host adds or
  removes a category, so a reused control can shift onto a different row's slot; Avalonia forbids
  re-setting `Name` on a reused styled control, so the per-row name box and verb buttons carry
  `AutomationProperties.AutomationId` (mutable, reuse-safe) — the same reason `MaterialsControls`
  gives. `matchesId` in the test matches by `Name` OR `AutomationId`.
- **The block-message slot renders only while a message is present** (the `LibraryControls.confirmPanel`
  precedent): `blockMessage = ""` means the `CategoryBlockMessage` control is absent, not an empty box.
  The known test State seeds a non-empty message so the slot is found.
- **Domain-free by construction.** The control imports no `OpticalConstructor.Domain` type; the host
  flattens each `MaterialCategory` (slice-004 `CategoryEditor` working row) into a `Row` and maps the
  `Handlers` onto the editor messages / `commit`. Wiring is the later WIRE_UI step, not this one.
