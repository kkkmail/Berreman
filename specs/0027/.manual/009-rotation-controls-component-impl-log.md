# 0027-009 — Rotation-controls component (implementation log)

Implements `008-task.txt`: lift the bottom rotation buttons (R1−/R1+/R2−/R2+/R3−/R3+ …) out of the
diagnostic test windows into one reusable named component, give it the behaviour you asked for, use
it on all three test windows to prove it works, then wire the **Main** screen onto the same control.

Build is green; all tests pass — `OpticalConstructor.Ui.Tests` **167/167**, `OpticalConstructor.Tests`
**245/245** (unchanged — no Domain source was touched).

## The name and the home for it

Collective name: **the rotation bar** — module `RotationControls`, the function `RotationControls.view`.
It lives in a **new dedicated project**, `OpticalConstructor.Controls` (net10.0, references only
Avalonia + Avalonia.FuncUI, *not* Domain), so both the test windows and the Ui can reference one copy:

```
Domain ← Controls ← { Ui, TestWindows }
```

## What the bar is

`RotationControls.view (state : State) (handlers : Handlers) : IView` — a **stateless** view (a plain
FuncUI `IView`, *not* a `Component`; the one transient piece of state — the reset confirmation — is
held by the host model, see below). The host passes in a snapshot and a record of callbacks:

- `State = { r1; r2; r3 : float; r3Locked; enabled : bool; confirm : ResetConfirm }`
- `Handlers = { rotate; setAngle : Axis -> float -> unit; toggleR3Lock; requestReset; requestResetAll;
  confirm; cancel : unit -> unit }`

so the bar holds no logic of its own — every window/screen maps its own model to `State` and its own
messages to `Handlers`. Stable `RotationControls.UiIds` are exposed for headless tests.

### Behaviours (exactly as the task asked)

- **`Unlock R3` / `Lock R3` toggles its own label** from `state.r3Locked`. Locking captures the
  *current* R3 (it does not reset it); the R3 −/+ buttons and the R3 field are disabled while locked.
  Tables seed R3 *unlocked* (button reads `Lock R3`); other elements seed it *locked* (`Unlock R3`).
- **`Reset` resets only the current selection** and is **confirmation-gated** — clicking it arms an
  inline `Reset the current rotations? [Yes] [No]` prompt; nothing changes until `Yes`.
- **`Reset All`** (new button) resets the rotations of **every** element *including the table*, also
  confirmation-gated, with its own prompt.
- **R1 / R2 / R3 are editable** — each angle is an editable field next to its −/+ buttons. It is an
  uncontrolled `TextBox` (text follows the model only when not being edited) that commits on `Enter`
  / blur via `Double.TryParse(InvariantCulture)`, so **non-integers are allowed** (e.g. `42.5`).
- **No selection ⇒ the whole bar is disabled** (`isEnabled state.enabled`, dimmed) — there is nothing
  to rotate. `−/+` buttons step 15° (5° with `Shift`).

The confirmation is modelled as `ResetConfirm = NoConfirm | ConfirmReset | ConfirmResetAll` carried in
each host model; `requestReset`/`requestResetAll` arm it, `confirm`/`cancel` resolve it. This keeps
the view stateless and the confirm flow unit-testable as pure MVU.

## Using it on the three test windows (to actually test it)

All three diagnostic windows now render their bottom controls via `RotationControls.view`, each
supplying a `rotationState`/`rotationHandlers` mapping. This is what exercises the component end to end:

- **`TableRotationView`** — R3 seeded unlocked; `enabled` only when the table is selected.
- **`ElementRotationView`** — element R3 seeded locked; `Reset` zeros only the selected element's
  rotations (no longer touches zoom/lock), `Reset All` zeros all.
- **`TableAndElementRotationView`** — `toggleR3Lock` / reset / set-angle all target *whatever is
  selected* (the table's own R3 lock when `TableSelected`, else the element's); `Reset All` resets the
  view **and** every element.

Their tests were updated to the shared `RotationControls.UiIds` button ids and to the new behaviours
(label-toggle lock that captures the current value, single-selection reset, reset-all, editable
set-angle, confirm/cancel). A static-harness wrinkle: `withMouseHarness` builds the view once and does
not re-render after a dispatch, so a button's enabled-state must be right at build time — added
`withMouseHarnessFrom initial inject` and started the enabled-button tests from a pre-selected model.

## Wiring the Main screen onto the same control

`ConstructorView` (the **Main** screen) now drops its old per-element rotation menu items in favour of
the bar:

- The page is now a `DockPanel` — `canvasSurface` (the former `view`, the interactive canvas) fills the
  centre and the `RotationControls.view` sits docked at the bottom.
- `rotationBarState model` maps the **active element**'s `r1/r2/r3/r3Locked` into `State`, with
  `enabled = (active element is Some)` — so the bar is dead while only the table is selected, live on an
  element.
- `rotationBarHandlers dispatch` maps the callbacks onto new messages: `RotateActiveBy (axis, deg)` and
  `SetActiveAxis (axis, value)` edit the active placement via the lock-respecting
  `Placement.withR1/withR2/withR3`; `toggleR3Lock` reuses the existing `MenuToggleLock AxisR3`;
  `RotRequestReset` / `RotRequestResetAll` arm `model.rotationConfirm`; `RotConfirm` runs
  `resetActiveRotation` (active element only) or `resetAllRotations` (all placements, committed) then
  clears; `RotCancel` clears.

Reset on the Main screen zeros **rotations only** (it leaves position/zoom/lock alone) and goes through
the normal `commitIfChanged` path so undo/redo and the dirty flag behave.

Three new tests (in `CommandRegistryTests`) cover the Main-screen integration directly: delta-rotate
and exact set-angle of the active element; the bar enabled only when an element is active; and the
confirmation-gated `Reset` (active only) vs `Reset All` (every element). The existing real-injection
constructor tests (dedup / round-trip on the 760×480 surface) still pass under the new DockPanel layout.

## Files

- **New:** `OpticalConstructor.Controls/{OpticalConstructor.Controls.fsproj, RotationControls.fs}`.
- **Changed (test windows):** `TableRotationView.fs`, `ElementRotationView.fs`,
  `TableAndElementRotationView.fs` (each renders the shared bar; model gains `rotationConfirm`, the
  lock/reset messages are reworked).
- **Changed (Main):** `OpticalConstructor.Ui/ConstructorView.fs` (DockPanel + bar; `rotationConfirm`;
  `RotateActiveBy`/`SetActiveAxis`/`RotRequest*`/`RotConfirm`/`RotCancel`; `rotationBarState`/`Handlers`).
- **Changed (wiring/tests):** `Berreman.slnx`, the `OpticalConstructor.Ui` and `OpticalConstructor.TestWindows`
  `.fsproj`s (reference `Controls`), and the four rotation test files plus `CommandRegistryTests.fs`.

## Follow-up fix — `Test Table + Element Rotations` could not be unselected

Reported after the above landed: on `Test Optical Table` clicking off the plate correctly unselects the
table, but on `Test Table + Element Rotations` it did not — the table stayed selected forever.

Cause: `TableAndElementRotationView.selectionAt` had only two outcomes (`ElementSelected` / `TableSelected`)
and fell back to `TableSelected` for *any* click that missed an element — there was no unselected state, so
a click far outside the plate still "selected the table." `TableRotationView` already does the right thing
via `TableView.tableHit`.

Fix: added a third selection case `NothingSelected` and made `selectionAt` mirror the table test —
nearest element within the select radius → `ElementSelected`; else on the plate (`TableView.tableHit`) →
`TableSelected`; else → `NothingSelected`. All the selection matches (rotate / set-angle / reset / R3-lock /
element-zoom / readout) handle the new case as a no-op, and the rotation bar's `enabled` is now `false`
when nothing is selected (consistent with task 008's "nothing selected ⇒ controls disabled"). Tests: the
existing click test now also asserts an on-plate empty click selects the table and an off-plate click
yields `NothingSelected`, plus a new test that rotation gestures are inert while unselected
(`OpticalConstructor.Ui.Tests` 168/168).

## Task 009 — an axis lights up while its rotate modifier is held

Requested next: make the R1/R2/R3 +/- buttons stand out (bold + a changed background) when their
relevant modifier combo is held — and tweak only the control, not the screens.

**The map.** An axis is "armed" by exactly the combo that the wheel uses to rotate it:
`Shift → R1`, `Ctrl+Shift → R2`, `Alt → R3` (anything else — including the `Ctrl+Alt(+Shift)` zoom
combos — arms nothing). This is the pure function `RotationControls.activeAxisForModifiers`.

**The colour.** An armed button uses the standard Windows "selected/active" treatment — the **light
accent fill `#CCE8FF`** outlined by the Fluent / WinUI **system accent `#0078D4`** (sensible since the
app may move to WinForms/Windows, where `#0078D4` is the native accent). Per the request the text **only
goes bold** — it is not recoloured — and stays legible on the light fill; the idle button keeps the
neutral light grey. Only the `+/-` buttons light up (not the degree field), and only while their group
is actually live (a locked R3 won't light).

**Self-contained, no screen changes.** The bar was a pure `view : State -> Handlers -> IView`. To know
the live modifier state without each screen feeding it in, `view` now returns a small stateful FuncUI
`Component` that, on mount, finds its `TopLevel` and adds **tunnel-phase** `KeyDown`/`KeyUp` handlers
(`handledEventsToo`) — tunnel always starts at the TopLevel, so the bar sees every modifier press/release
regardless of which control holds focus. It stores the armed axis in `ctx.useState`, re-renders the
buttons, and tears the subscription down with the component (`useEffect AfterInit` returning an
`IDisposable`; re-subscribes across `AttachedToVisualTree`/`DetachedFromVisualTree`). The visible tree is
still the pure `renderBar state handlers activeAxis`. Because it is one shared control, **all three test
windows and the Main screen get the highlight for free** — no host code changed.

Tests (`OpticalConstructor.Ui.Tests` **170/170**): a pure test pinning `activeAxisForModifiers` to the
wheel map (including the zoom combos arming nothing), and a **headless** proof that a real `Shift`
key-press routed through the live input pipeline turns the R1 buttons' `Background` to the accent (R3
stays idle) and releasing reverts it — i.e. the key → TopLevel → state → re-render path works end to end.

Files: only `OpticalConstructor.Controls/RotationControls.fs` (the control) and the new
`OpticalConstructor.Ui.Tests/RotationControlsTests.fs` (+ its `.fsproj` entry). No screen was touched.

## Package update — Avalonia 11.3.4 → 12.0.5, FuncUI 1.6.0 → 2.0.0-preview1

The packages were bumped to Avalonia 12. Stable FuncUI 1.6.0 is locked to Avalonia 11 (its DSL
references a type removed in v12), so FuncUI + FuncUI.Elmish moved to **2.0.0-preview1** (the only FuncUI
that targets Avalonia 12.0.0) across Controls / Ui / App / TestWindows. That surfaced two real Avalonia 12
breaking API changes, both migrated:

- **Drag-and-drop** (`MaterialsView.fs`): the old `IDataObject` model became the typed `DataTransfer`
  API — `DataObject().Set` + `DragDrop.DoDragDrop` → `DataTransfer().Add(DataTransferItem.Create(fmt,id))`
  + `DragDrop.DoDragDropAsync`; the `"oc-material-id"` key is now a `DataFormat<string>`
  (`DataFormat.CreateStringApplicationFormat`); reading uses `e.DataTransfer` + the `Contains` /
  `TryGetValue` extensions.
- **Clipboard** (`App/Program.fs`): `IClipboard.SetTextAsync` moved to the `ClipboardExtensions` extension.

## Bug fixes (post-migration)

**(a) Yellow roll-normal pointed down.** In the two test windows that draw the N1/N2 axes
(`ElementRotationView`, `TableAndElementRotationView`) the secondary (roll) normal N2 — the table normal,
"up out of the table" — projects downward once the view/element is tilted. Per the request ("just the
drawing thing — make it point up") the N2 marker is now drawn toward **−N2** (`along n2 (-(1.3*half))`),
so the yellow arrow points up on screen. Drawing only; the element's actual orientation is unchanged.

**(b) The bar froze on the first-selected element, and lost per-window isolation.** Task 009 wrapped the
bar in a FuncUI `Component` for the keyboard highlight. A FuncUI component **re-renders only on its own
state** — the `state` captured in its render closure *freezes at the first render*, so a different
element's `r3Locked`/angles never reached it (the highlight still worked, being the component's own
`useState`). The model was always per-element; only the display was stale. A first fix pushed the state
through a **module-level** store, which fixed the staleness but (i) was shared across windows — breaking
the "only the active window changes" requirement — and (ii) re-entered the renderer mid-render (the
`Set` during build) and threw on the Reset swap.

**Final fix — make the bar PURE again.** `RotationControls.view` is once more a plain
`State -> Handlers -> IView`, so it re-renders on every host update and always shows the host's current
selection — per-window by construction (each window renders its own model), no frozen props, no
re-entrancy. The keyboard highlight is re-implemented WITHOUT component state: a module-level `armedAxis`
(the held modifier's axis — global, since modifier keys are global) that the bar reads at **build** time
(so a host re-render while a key is held keeps the highlight), plus a **per-window** TopLevel key hook
(`ensureKeyHook`, guarded by a `ConditionalWeakTable` so each window is hooked once) that recomputes
`armedAxis` and imperatively restyles that window's live buttons on key down/up. Key events only reach the
focused window, so the live restyle is naturally per-window; only the display state is read globally.

**(c) Reset threw "Cannot set Name : styled element already styled".** The confirm gate replaced
`[Reset, Reset All]` with `[prompt, Yes, No]`. At the shared position, FuncUI **reused** the existing
`Reset All` Border and tried to **rename** it to the confirm button — Avalonia forbids re-setting `Name`
on an already-styled element. Fix: the confirmation now happens **in place** — the same two named buttons
(`reset` / `resetAll`) keep their ids and only change label/action ("Reset"/"Reset All" ⇄ "Yes"/"No"),
with a prompt `TextBlock` that is simply shown/hidden. No control is renamed, so the swap renders cleanly.

**(d) "Yes"/"No" did nothing and Reset/Reset All crossed over.** The in-place confirm from (c) reuses the
same Border for both modes, but FuncUI's event subscriptions default to `SubPatchOptions.Never` — the
FIRST handler is kept forever, even though the button's action changed. So the armed "Yes" button still
ran `requestReset` (re-arming → the prompt never cleared) and "No" ran `requestResetAll` (so Reset's
cancel armed Reset All — the "mixing"). Fix: `clickBox`'s `onPointerPressed` now uses
`SubPatchOptions.OnChangeOf (box label)`, so a button re-subscribes its handler exactly when its label
flips (`Reset`↔`Yes`, `Reset All`↔`No`); the stable-label +/- and angle-step buttons keep their cheap
one-time subscription. Now "Yes" confirms and "No" cancels, and the two are independent.

Tests: `OpticalConstructor.Ui.Tests` **173/173** — headless reproductions that (i) a parent flipping
`r3Locked` updates the lock label (props are live again), (ii) arming Reset turns the buttons into Yes/No
in place without the rename exception, and (iii) **real clicks** through the armed buttons run
confirm/cancel (one reset, no cancel; then one cancel, no extra reset) rather than the original Reset
actions; the existing highlight test still passes against the pure bar. Full solution builds clean on
Avalonia 12 / FuncUI 2.0.0-preview1. Files: `RotationControls.fs` (pure bar + imperative highlight +
in-place confirm + per-label subscription), the two test windows (N2 flip), `MaterialsView.fs` /
`Program.fs` (Avalonia 12 APIs), the four `.fsproj`s (versions), `RotationControlsTests.fs` (new tests).
