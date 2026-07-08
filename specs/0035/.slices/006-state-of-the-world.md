# State of the world — slice 006 (ADD_COMPONENT — UICOMP_XDUO_0006 CategoryEditorWindow)

## Where we are

Slice 006 is the sixth step of arc 0035 (Part A — the open, editable, Guid-keyed material-category
set). Step 1 turned the category into DATA (a seeded, `CategoryId`-keyed catalogue); step 2
DECLARED the mutating write-seam (`CategoryProxy` + `CategoryError`); step 3 IMPLEMENTED the
in-memory store; step 4 added the pure, Avalonia-free `CategoryEditor` Domain edit model (inline
add / rename / remove + the `commit` diff projection); step 5 added the domain-free
`OpticalConstructor.Controls.CategoryControls` component + its headless structure test. This slice
ADDS the **Category editor window** (`CategoryEditorWindow` + its pure `CategoryEditorView` MVU
loop) in `OpticalConstructor.TestWindows`, beside the Material / Sample editor windows — the
composition that projects the step-4 editor onto the step-5 control and dispatches to
`CategoryProxy` — plus its pure + headless tests. It is TestWindows + Ui.Tests only; the window is
authored and registered but NOT wired into a parent view / launcher (that is a later WIRE_UI step).

## What's working

- Add the `CategoryEditorView` MVU model + FuncUI projection and the `CategoryEditorWindow`
  `HostWindow`, beside `MaterialEditorWindow` / `SampleEditorWindow`, over the step-4
  `CategoryEditor` edit model and the step-5 `CategoryControls` surface.
- Inline Add opens a fresh `UserCategory` / `SelectableOnCreate` row; inline Rename edits the name
  (built-ins are renamable); each row's Save commits its add / rename through the matching
  `CategoryProxy` verb (`addCategory` / `updateCategory`).
- Remove is confirm-gated inline (arm → confirm on the same Remove verb) and surfaces the store's
  typed `CategoryStillReferenced` (naming the referencing materials) or `BuiltInNotRemovable`
  refusal in the inline block slot, leaving the store unchanged.
- A window-level Save/Cancel row (distinct positive/negative styling) flushes any staged edit
  through the step-4 `commit` diff then closes, or closes discarding.
- Register `CategoryEditorView.fs` + `CategoryEditorWindow.fs` in `OpticalConstructor.TestWindows.fsproj`
  and `CategoryEditorWindowTests.fs` in `OpticalConstructor.Ui.Tests.fsproj`.

## Tests

`OpticalConstructor.Ui.Tests/CategoryEditorWindowTests.fs` — 16 tests (12 pure `ui-tests`, 4
headless `ui-smoke`), all green locally:

- **Pure contract / model (`ui-tests`, 12):** the stable `CategoryEditorWindow` id; `init` seeds
  over `listCategories` with nothing armed; `BeginAdd` appends a blank `UserCategory` /
  `SelectableOnCreate` editing row; Add→set-name→Save persists through `addCategory`; a blank name
  is refused with the typed `InvalidCategory` and nothing persists; Rename of a user category and of
  a built-in both commit through `updateCategory`; a referenced-user remove is confirm-gated,
  surfaces `CategoryStillReferenced`, and leaves the store unchanged; a built-in remove surfaces
  `BuiltInNotRemovable` and leaves the store unchanged; Cancel discards a freshly-added row; the
  window Save flushes a staged rename through the proxy and requests close; the window Cancel
  requests close and writes nothing.
- **Headless semantic-tree (`ui-smoke`, 4):** DRIVE THE REAL `CategoryEditorWindow` by its UiIds
  over a stub `CategoryProxy` — the window mounts carrying the `CategoryEditorWindow` id and the
  `CategoryControls` surface; **Add** then Save grows the list through `addCategory`; **Rename**
  edits a name in place through `updateCategory`; a confirm-gated **Remove** of a referenced
  category surfaces the block and leaves the list (and its row) unchanged. (The slice acceptance.)

Gate results captured locally (the arc-runner gate engine remains the sole authority, Invariant 6):
`build` (full `Berreman.slnx`, Release) — 0 errors, no new warnings from our code; `ui-smoke` — 88
passed / 0 failed (84 → +4); `ui-tests` — 324 passed / 0 failed (312 → +12). `unit-tests` /
`constructor-unit-tests` are untouched (no file in `BerremanTests` / `OpticalConstructor.Tests`
changed), so their `count_at_least` baselines cannot regress. This slice only ADDS tests.

## Architecture

- **The window is the composition, not new UI vocabulary.** `CategoryEditorView` reuses the step-5
  `CategoryControls` AS-IS (Controls is not in `touches`): it flattens the step-4 `CategoryEditor`
  working `MaterialCategory` rows into `CategoryControls.Row`s and injects a `CategoryControls.Handlers`
  record that dispatches the view's `Msg`s — the `SampleEditorView`/`SampleEditorWindow` pair shape.
- **Per-row immediate commit.** Because `CategoryControls` exposes per-row Save / Cancel / Remove
  handlers and the how-to wants Remove "confirm-gated inline" and Add to "grow the list", each row's
  verb dispatches to the proxy immediately; a successful write `reload`s the editor from
  `listCategories`, so the `commit` diff baseline tracks the store and a re-Save of the same row is a
  no-op, never a `DuplicateCategoryId`.
- **The step-4 `commit` drives the window-level Save.** The window Save projects the working-row diff
  through `CategoryEditor.commit` and flushes each intent to the matching proxy verb — the only user
  of the batch diff; the per-row Save computes a single-row intent inline (add vs. rename by the
  snapshot), so it cannot be blocked by another row's transient blank.
- **Confirm gate over a fixed control.** `CategoryControls` carries no confirm button, so Remove is
  armed-then-confirmed on the SAME Remove verb (`pendingRemove : CategoryId option`), the block slot
  doubling as the confirm prompt and then the store's typed refusal.
- **UiId vocabulary.** One new literal (`CategoryEditorView.UiIds.window = "CategoryEditorWindow"`,
  the window's Name + AutomationId); every list / row / verb / block id is REUSED from
  `CategoryControls.UiIds`. The window-level Save/Cancel reuse the base `CategorySaveButton` /
  `CategoryCancelButton` literals — the per-row verbs carry the guid-suffixed derivations, so they
  cannot collide.

## Deferred

- Wiring `CategoryEditorWindow` into a parent view / a Materials-bay `Categories…` launcher (the
  real composition root couples it to the live `CategoryProxy.createInMemory` store and the
  `materialsReferencingCategory` lookup) — a later WIRE_UI step, not this ADD_COMPONENT slice.
- The window shows ALL catalogue rows including the `HiddenOnCreate` Vacuum category (this is the
  category MANAGER, not the create picker); a manager-side visibility filter, if wanted, is a later
  concern.

## Gotchas

- **Per-row vs batch commit (the main ambiguity).** The step-5 `CategoryControls` supplies per-row
  Save/Cancel/Remove handlers, and the how-to says Remove is "confirm-gated inline" and Add "grows
  the list" — both incompatible with a pure batch-at-window-Save model. Resolved (the interpretation
  most consistent with steps 4+5): per-row verbs commit immediately to the proxy; the window-level
  Save/Cancel row additionally flushes any staged edit through the step-4 `commit` and closes.
- **`BuiltInNotRemovable` cannot be reached through the UI.** Step-5 `CategoryControls` OMITS the
  Remove verb for a built-in row (removed, not greyed), so no built-in Remove is clickable. The
  window's remove path is origin-agnostic (it delegates the block to the store), so
  `BuiltInNotRemovable` is surfaced by the SAME wiring and is verified by a pure model test that
  drives the remove path with a built-in id; the headless proof drives the UI-reachable
  referenced-user case. Both refusals flow through the one block slot.
- **The freshly-added row's id is minted, not known.** `BeginAddCategory` mints the `CategoryId`, so
  the headless Add proof cannot target `rowNameBox <known-guid>`; it locates the new row by its empty
  name box and extracts the guid suffix from that box's AutomationId to target Save.
- **Row ids round-trip as Guid strings.** `CategoryControls.Row.categoryId` is a string; the view
  parses it back to the elevated `CategoryId` with `Guid.TryParse` (ignoring an unparsable id — it
  never happens for a real row), because Controls is domain-free and cannot carry `CategoryId`.
- **MSB3277 is pre-existing and out of scope.** The full-solution build emits `MSB3277`
  (conflicting `WindowsBase` 4.0.0.0 vs 5.0.0.0) originating from `OpticalConstructor.Ui.fsproj` — a
  project this slice does not touch; the two `.fsproj` edits here add only `<Compile>` items (no
  reference changes), so they introduce no new MSB3277, and the `build` gate passes with 0 errors as
  in prior green slices.

## Changelog

- 2026-07-07 — Added the `CategoryEditorWindow` (UICOMP_XDUO_0006): the `CategoryEditorView` MVU loop
  (`Model` over the step-4 `CategoryEditState` + `pendingRemove` confirm-gate + inline block; the
  `BeginAdd` / `SetName` / `SaveRow` / `RemoveRow` / `CancelRow` per-row verbs + `CommitAll` /
  `CancelWindow` window verbs; a view that flattens the editor rows onto the step-5 `CategoryControls`
  and pins a Save/Cancel row) and the `CategoryEditorWindow` `HostWindow` composition root,
  registered in `OpticalConstructor.TestWindows.fsproj` after the Material editor pair. Added the
  headless `CategoryEditorWindowTests` (12 pure contract/model tests + 4 UiIds-driven `ui-smoke`
  proofs over a stub `CategoryProxy`: Add grows the list, Rename updates a name, a confirm-gated
  remove of a referenced category surfaces `CategoryStillReferenced` with the list unchanged),
  registered in `OpticalConstructor.Ui.Tests.fsproj` after `CategoryControlsTests.fs`.

```yaml
gates:
  berreman_unit_tests: 119
  constructor_unit_tests: 440
  ui_smoke_tests: 88
  ui_tests: 324
```
