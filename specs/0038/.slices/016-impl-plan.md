# Step 016 — impl plan (IMPLEMENT, AC-G1)

## Goal

Give BOTH windows (Materials, step 013; Library, step 015) the Browse/Select
mode DU — one shared Domain shape, never a per-window copy — plus the launcher
re-target, the workbench staleness rules, and the targeted `onSelected`
dispatch with the vanished-target no-op + status line.

## Approach

1. **Domain** — new `WindowMode.fs` (after `SampleStackEditor.fs`):
   `KindConstraint` (single-case DU over `CatalogueKind`),
   `SelectionTarget = TableElementTarget of ElementId | SampleLayerTarget of
   LayerPosition`, `[<ReferenceEquality>] SelectionContext<'entry>` (constraint,
   target, `onSelected : 'entry -> unit`, `onCancelled : unit -> unit`), and
   `LibraryWindowMode<'entry> = Browse | Select of SelectionContext<'entry>`.
   Generic `'entry` is what makes ONE mode DU serve both windows
   (`LibraryEntry` / `MaterialEntry`). NOTE: the spec's field name `constraint`
   is an F# reserved word — the field is `kindConstraint` (recorded in the log).
   Also: `SampleStackEditor.isValidPosition` goes public (the vanished-layer
   check reuses it — never re-derived).

2. **WindowLauncher** — `SelectOpen` gains the baked re-target closure
   (`requestingWindow * retargetWindow : Window -> unit`); a Select-state open
   of a LIVE key re-targets + activates and returns the already-declared
   `RetargetedWindow`. Modal/modeless per the step-005 switch is already the
   step-008 behaviour and stays.

3. **Both window views** — model gains `mode`; context gains `requestClose`;
   messages `ConfirmSelect` (onSelected(highlighted) then close),
   `CancelSelect` (onCancelled then close), `RetargetSelect` (cancel the
   superseded session, re-point constraint/target, clear the highlight),
   `SelectDismissed` (the host's `Closed` hook — cancels a still-pending
   session exactly once; resolved modes flip to Browse first so nothing
   double-fires). Select state renders exactly TWO buttons in one row
   (positive/negative styling — the Save/Cancel precedent) plus the
   NON-REMOVABLE constraint banner (no breadcrumb chip). Library window:
   the constraint pre-applies at the corpus seam (`forKinds`); Materials
   window: every material serves a layer pick, so the constraint is
   structurally satisfied and shows as the fixed banner. Everything else IS
   the ordinary window (verbs/add-on-the-fly untouched).

4. **Window hosts** — `LibraryWindow` / `MaterialsWindow` ctors gain `?mode`;
   switch `mkSimple` → `mkProgram` with `Cmd.ofEffect` capturing dispatch;
   `Closed` hook dispatches `SelectDismissed`; public `Retarget` member is the
   launcher's re-target seam.

5. **Workbench** (TableAndElementRotationView) — `SelectSession` handle
   (`target : ElementId`, `cancelAndClose`), model fields `activeSelect` /
   `selectStatus`; a changed table selection (PointerUp), an element ADD
   (selects the new element) and the removal of the selected element all
   cancel+close the session (the pending-bind-clears precedent); new targeted
   message `BindValueIdTo of ElementId * string` — binds by element ID, a
   vanished element is a no-op plus the `WorkbenchSelectStatus` line, never a
   throw. Step 017's Choose… populates the session; tests seed it directly.

6. **Sample editor** — targeted message `BindMaterialToLayer of LayerPosition *
   MaterialId` (the Materials window's Select return; step 019 wires the verb):
   valid position → set that layer's material preserving the user selection
   (the SetLayerOrientation dance); vanished row → no-op + the existing status
   line.

## Files

- `OpticalConstructor.Domain/WindowMode.fs` (new) + `.fsproj`
- `OpticalConstructor.Domain/SampleStackEditor.fs` (isValidPosition public)
- `OpticalConstructor.Ui/WindowLauncher.fs`
- `OpticalConstructor.Ui/{LibraryWindowView,LibraryWindow,MaterialsWindowView,MaterialsWindow}.fs`
- `OpticalConstructor.Ui/TableAndElementRotationView.fs`
- `OpticalConstructor.Ui/SampleEditorView.fs`
- `OpticalConstructor.Ui.Tests/{LibraryWindowTests,MaterialsWindowTests,WindowLauncherTests,MainSceneMsgTests,SampleEditorWindowTests}.fs`

## Tests (headless + pure; both windows)

Select/Close pair + non-removable constraint banner + no chip; ConfirmSelect
returns the highlighted entry through the targeted dispatch and closes (full
loop through the REAL launcher into the workbench readout / the sample-editor
row); canvas selection change cancels+closes the open Select window for BOTH
windows and fires onCancelled once; vanished target (element removed / layer
row deleted) no-ops with the status line; launcher re-target returns
`RetargetedWindow` on the live single instance; retarget re-points
constraint/target and cancels the superseded session.

## Risks

- Elmish dispatch ordering around `requestClose` inside `update` (Closed →
  `SelectDismissed` must not double-fire after Confirm/Cancel) — handled by
  flipping mode→Browse in the same update that fires the callback; ring-buffer
  dispatch processes the hook message after the commit.
- The app-global `WindowRegistry` in tests — every opened window must be
  closed (step-008/013/015 gotcha).
- FuncUI dynamic rows: the new Select row / status line use AutomationId +
  keys, never `Name` (Part A discipline).
