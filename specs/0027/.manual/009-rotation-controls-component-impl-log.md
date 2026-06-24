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
