# 0027-005 — Optical-table rotation test window (implementation log)

Implements task `002-rotate-table-task.txt` with the four defaults you approved in `003`:
(A) a true 3-D tumble of the table projected to the screen; the launcher is the app's
startup window; a separate `OpticalConstructor.TestWindows` project; Avalonia FuncUI, with
all rotation/projection/selection logic kept in the pure (rendering-agnostic) domain.

Build is green (`dotnet build Berreman.slnx -c Release`) and all tests pass — see §Tests.

## Root cause that made table rotation "not work" (recap of 003)

`Table.TableViewState` already carried `r1/r2/r3`, but the projection only ever applied a
flat **in-plane spin by `r1`** (`ConstructorView.projectToCanvas` / `rotateInPlane`); `r2`
and `r3` were never read, and there was no 3-D→screen projection. The only rotate control in
the app (Trace-ribbon "rotate left/right" → `RotateViewR1`) drove that flat spin. So the
table could not be tumbled in 3-D — exactly the "3D rotation / projection" gap you flagged.

## What I built

**Single source of truth (pure, Avalonia-free, headless-testable):**
`OpticalConstructor.Domain/TableView.fs` — the real 3-D rotation + orthographic projection:
- Table frame: `+X` = length (2.0 m), `+Y` = width (1.2 m), `+Z` = thickness (0.10 m), origin
  at centre. Top-down default looks down `+Z` with `+X` right and `+Y` up.
- View rotations are measured **relative to the fixed screen axes** (Spec 0026 §C.2.4):
  `R1` spins about the viewing axis (`+Z`, the in-plane spin), `R2` pitches about the screen
  horizontal axis (`+X`), `R3` yaws about the screen vertical axis (`+Y`); composed
  `Ry(r3) ∘ Rx(r2) ∘ Rz(r1)`, then orthographic (drop depth). At `(0,0,0)` it is the identity,
  so the default is exactly the old top-down layout.
- Also: the plate as a 3-D box (`plateCorners3D`/`plateEdges`/`plateTopFace`), `tableHit`
  (click-selects-the-plate), and `unprojectToTablePlane` (the inverse, used to prove the
  projection round-trips).

**The test window project (separate, per the task):** `OpticalConstructor.TestWindows`
- `TableRotationView.fs` — pure MVU (`Model`/`Msg`/`update`, Avalonia-free) + the thin FuncUI
  `view` that draws the tumbling plate (filled top face + 12 box edges), the central-ray
  reference line with source/detector markers, the selection highlight, the `R1±/R2±/R3±` +
  `Reset` buttons, and a live `R1/R2/R3` + selection readout. Reuses **the same**
  `Table.defaultTable` and `TableViewState` as the main app — no parallel table.
- `TableRotationWindow.fs` — the FuncUI `HostWindow` that mounts that MVU loop.

**The launcher (the "very simple main form"):** `OpticalConstructor.App/Program.fs`
- New `LauncherWindow` with two buttons — **Main** (opens the existing Optical Constructor
  window, unchanged) and **Test Optical Table Rotations** (opens the test window). It is now
  the app's startup window; `ShutdownMode = OnLastWindowClose` so closing it after opening
  Main/Test doesn't quit the app.

## Tests (headless first, as you asked)

- `OpticalConstructor.Tests/TableViewTests.fs` — **11** pure projection tests: top-down
  identity; each of R1/R2/R3 actually moves the projection; "R2 is no longer inert"; the
  project→unproject round-trip; edge-on ⇒ `None`; the plate is a 1.2×2.0×0.10 m box (8
  corners/12 edges); click hits the plate / misses outside; still hittable after a 3-D tumble.
- `OpticalConstructor.Ui.Tests/TableRotationTests.fs` — **7** tests: pure MVU (rotate steps,
  reset, select/unselect, selectable-after-tumble) **plus 2 `ui-smoke`** headless render
  proofs (the FuncUI view and the window host each render a frame without throwing).
- Results: `OpticalConstructor.Tests` 245/245; `OpticalConstructor.Ui.Tests` 109/109
  (including the existing `ui-smoke` `SmokeTests`, which boots the app with the new launcher —
  no regression). Full solution builds clean.

## How to try it on screen (your manual step)

1. Run `OpticalConstructor.App` (e.g. `dotnet run --project Berreman/OpticalConstructor/OpticalConstructor.App -c Release`, or launch the built exe).
2. In the launcher click **Test Optical Table Rotations**.
3. **Drag the table with the mouse to tumble it**; **mouse-wheel to spin (R1)**; **click to
   select / deselect**. Or use the `R1±`/`R2±`/`R3±` buttons (15° each, **Shift = 5°**) and
   `Reset` (straight top-down). The readout shows the live angles + selection.
4. **Main** opens the existing Optical Constructor window, unchanged.

## Update — round 2 (your feedback: "rotation by mouse doesn't work; test it"; 5°/15°; mod 360)

You were right — the first cut only had buttons, so mouse rotation was never wired. This round:

- **Rotation by mouse now works and is tested for real.** Drag tumbles the table (horizontal =
  yaw R3, vertical = pitch R2); the wheel spins R1; a click (no drag) selects. The pointer
  handlers are dumb event→message translators and all logic is in the pure `update`, so I test
  the mouse path **end to end with real `Avalonia.Headless` pointer injection**
  (`MouseDown`/`MouseMove`/`MouseUp`/`MouseWheel` through the live input pipeline) and assert
  the model actually rotated — not a simulated message. See the four `ui-smoke` injection tests
  in `TableRotationTests.fs`.
- **Shift+button = 5°, normal = 15°** (`buttonStepDegrees`), proved by a real Shift+click
  injection test. (Rotate controls are button-styled `Border`s, because a real `Button.Click`
  carries no key modifiers.)
- **Angles wrap mod 360** (`normalizeDegrees`) — 350° + 15° = 5°, −15° = 345°.

- **Root cause of the "fishy" rotation — a genuine finding.** FuncUI subscribes every pointer
  handler with the event's full `Tunnel|Bubble` routing, so a handler fires **twice** for the
  clicked element (once tunneling down, once bubbling up). Idempotent handlers (select, drag-
  begin) hide it, but an **accumulating** handler doubles: a single click rotated **10°, not 5°**,
  until I fixed it. The fix is `e.Handled <- true` in each handler (drops the duplicate pass).
  **This very likely affects the main app too** — e.g. element rotation by `Shift`/`Ctrl+Shift`/
  `Alt`+wheel in `ConstructorView` goes through `Border.onPointerWheelChanged` and would apply
  **two** steps per notch. That is a strong candidate for the rotation looking off, and is worth
  a follow-up sweep (add `e.Handled <- true` to the accumulating mouse handlers there).

Tests after round 2: `OpticalConstructor.Tests` 245/245; `OpticalConstructor.Ui.Tests` 116/116
(the table-rotation module is 14 tests — 9 pure MVU + 5 real-input/headless). Full build green.

## Update — round 3 (your feedback: constrain the rotations; plain drag must do nothing)

You were right that free drag-to-orbit is wrong for a scientific app: it mixed two axes at once
and triggered on a too-easy gesture. I replaced it with the **documented, constrained gesture
scheme** (Spec 0026 §E.3/§E.5, mirroring `OpticalConstructor.Ui/Commands.fs` — replicated in the
test window with the citation, to keep this diagnostic project isolated on the Domain only):

- **One axis per gesture, 5° per notch** — `Shift`+wheel → R1, `Ctrl+Shift`+wheel → R2,
  `Alt`+wheel → R3; plain / `Ctrl`+wheel → zoom; **any other modifier combination does NOTHING**
  (a gesture does exactly one thing, or nothing — never several at once).
- **A plain press→drag→release does NOTHING** — it neither rotates nor selects (too easy a
  gesture to manipulate a scientific scene; deliberately blocked). Only a clean click (a press
  that never moved past the threshold) selects / deselects the table.
- Buttons unchanged (one axis each, 15°, Shift = 5°); angles still wrap mod 360.

- **On your `e.Handled` doubt:** it is exactly relevant here, so I kept it. FuncUI fires each
  pointer handler twice (Tunnel|Bubble), so without it a single wheel notch would rotate **10°,
  not 5°**. `e.Handled <- true` makes **one notch = one 5° step** — a headless test asserts
  exactly 5°, which would read 10° if the dedup were removed. (Same fix still wanted in the main
  app's wheel rotation — a follow-up.)

These are proven by **real `Avalonia.Headless` pointer injection**, not simulated messages:
`Shift`/`Ctrl+Shift`/`Alt`+wheel each rotate exactly one axis by exactly 5°, a real plain drag
changes nothing, and a real click selects. Table-rotation module is now 17 tests (11 pure + 6
real-input/headless). After round 3: `OpticalConstructor.Tests` 245/245;
`OpticalConstructor.Ui.Tests` 119/119; full build green.

Still open / not in the test window yet: `Ctrl+0` reset is a Reset button here rather than the
keyboard chord (the spec's keyboard reset); say the word if you want the chord too.

## Deliberately NOT done this round (needs your go-ahead)

- **The main constructor page still uses the old in-plane-only projection.** I did **not**
  rewire `ConstructorView`/`ConstructorTable` to `TableView` yet — the test window is the
  proving ground ("start very simple"). Once you're happy with the rotation model here, the
  follow-up is to point the real page's projection at `TableView` (the single source of
  truth) so the constructor canvas tumbles too. It's a focused change but touches the live
  page and its many tests, so I left it until you've validated the model on screen.
- Test-window content is intentionally minimal (plate + central ray + source/detector
  markers): the goal was table rotation + selection, not full element rendering.

## Files

- New: `OpticalConstructor.Domain/TableView.fs`; the `OpticalConstructor.TestWindows` project
  (`TableRotationView.fs`, `TableRotationWindow.fs`, `.fsproj`);
  `OpticalConstructor.Tests/TableViewTests.fs`; `OpticalConstructor.Ui.Tests/TableRotationTests.fs`;
  this log + `003-…md`.
- Changed: `OpticalConstructor.Domain.fsproj`, `OpticalConstructor.App/Program.fs`,
  `OpticalConstructor.App.fsproj`, `Berreman.slnx`, `OpticalConstructor.Tests.fsproj`,
  `OpticalConstructor.Ui.Tests.fsproj`.
