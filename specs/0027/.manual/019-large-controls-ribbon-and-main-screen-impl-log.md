# 0027-019 — "Bays" + the main-screen ribbon, a shared renderer, and the wired Main screen (impl log)

Implements `018-more-standardization-and-main-screen.txt`. Full solution builds clean;
`OpticalConstructor.Ui.Tests` **219/219** (was 213), `OpticalConstructor.Tests` **245/245**.

## The "large control" concept — a **Bay** + the **Ribbon**

We needed a short name for the "large controls" (the rotation bar, the move bar, the element palette, the
renderer bar — a whole control unit, not a single button). Coined **Bay** (short, distinct from FuncUI's
`Component` and the Ui project's `Panel`s; the docking-bay metaphor fits "add / remove large controls
dynamically"). New **`OpticalConstructor.Controls/Ribbon.fs`**: a `Bay = { name; content }` and a
`Ribbon.view` that lays the bays out **MS-Word-ribbon style** — a top strip of bay NAMES (the "menu"); the
selected name reveals that bay's controls below. The ribbon is fully generic: each Bay carries its
`content` as an already-bound `IView`, so the ribbon knows nothing of any control's state / message types.
Adding or removing a large control from a screen is now just adding / removing a Bay.

## Functional proxies

Surveyed Softellect's convention (`*Proxy` = a record of camelCase functions returning `Result` /
`Async<Result>`, built by a `create` that bakes in the IO; tests swap in stub records of the same shape;
bundled into a `*Context`). Our UI controls already embody the same **functional-proxy seam**: each control
takes a `Handlers` record of injected functions (`RotationControls.Handlers`, `RayPositionControls.Handlers`,
the new `RendererControls.Handlers`, the ribbon's `onSelect`), and the headless tests already substitute
stub handlers — no live IO needed. Kept the `Handlers` name (UI callbacks return `unit`, not `Result`); the
`*Proxy` + `Result` form is for the genuine IO boundary (persistence), to be applied when the Main screen
gains save / load. Documented the equivalence on the controls.

## A single shared renderer (function → DU case)

The renderer was inlined in the renderer-test window. Split it into two reusable pieces so the renderer
test AND the Main screen draw through ONE source:

- **`OpticalConstructor.Controls/RendererControls.fs`** — the renderer is identified by a serializable
  **DU** (`RendererKind = Wireframe | Shapes`; the "function-valued" choice mapped to a DU case so it can be
  passed / serialized), plus the cap / rail / transparency config `State`, the `Handlers` proxy, and the
  slider UI `view`. Pure, no domain types.
- **`OpticalConstructor.TestWindows/ElementRenderer.fs`** — the draw functions (cylinders, spherical caps,
  the red roll seam, the normals), with `rendererOf : RendererControls.State -> ElementRenderer` recovering
  the actual draw function from the DU case. Operates on a `Drawable` (placement + zoom + optical sign).
- `RendererTestView` is now **thin** — it holds a `RendererControls.State`, draws via `ElementRenderer`, and
  shows `RendererControls.view`. Its old ~250 lines of config + geometry are gone (no duplication).
- **Face opacity affects the table too**: the Main plate fill now uses `render.faceOpacity`.

`kindName` / `kindCode` (and a schematic `opticalSign`) moved to an early **`Catalogue.fs`** so both the
shared renderer and the scenes use them without a compile-order cycle (`TableAndElementRotationView`
re-exports them for the existing references).

## The Main screen — a Lego constructor of Bays

The Main screen (the window behind the launcher's **Main** button) is the SAME scene as
`TableAndElementRotationView` (model / update / table / elements / selection), but its new **`mainView`**
lays the controls out as a **Ribbon of Bays**: **Rotation** (`RotationControls`), **Move**
(`RayPositionControls` — newly wired: slide the selected element along the beam, clamped to the plate),
**Add** (the combined `ElementPaletteControls`), **Render** (`RendererControls`). The elements are drawn
through the shared `ElementRenderer`, so the Render bay actually changes the look, and face opacity dims the
table. The ribbon shows the bay names MS-Word-style; selecting a name shows that bay. The App's
`MainConstructorWindow` now mounts `mainView` (taller, for the ribbon). The static test-window `view` is
unchanged, so every existing test still holds.

Because every screen is now built from the same Bays drawing through the same renderer, a change to any
control shows up on all screens that use it.

## Tests

`OpticalConstructor.Ui.Tests` **219/219**:
- `RendererControlsTests` (new, 6): the default state, swap, discrete rail / radial presets + index
  round-trip, cap-circle clamp, transparency clamps.
- `TableAndElementRotationTests` (+4, Main screen): the Move bay slides the selected element along the beam
  (clamped, inert otherwise); the Render bay swaps + tunes the serializable config; the ribbon offers every
  large control as a Bay and `SelectBay` is pure; a headless proof that the ribbon shows the bay tabs and
  reveals exactly the selected bay's controls.
- `RendererTestTests` rewritten to the thin scene (swap via `render.kind`, gestures, the shared-control page
  proof, the code-label render).

## Follow-up fix — ribbon crash on bay switch

Reported: on the Main screen, choosing **Render**, swapping the renderer, then clicking back to **Rotation**
threw `System.InvalidOperationException: Cannot set Name : styled element already styled` (in the FuncUI
`Patcher`). Cause: the ribbon swapped its SINGLE content node between two different controls (the renderer
sliders ⇄ the rotation buttons); since both are `StackPanel`s at the same position, FuncUI recycled the
live, styled, named control and tried to set a different `Name`. Fix (`Ribbon.fs`): render EVERY bay's
content pane always, with only the selected one `IsVisible` — each bay now has a stable position, so FuncUI
only ever patches a pane against its own previous self (no cross-bay recycling). The bay names do not
collide (each control has its own id namespace), so all panes coexisting is safe.

Tests: the ribbon test now asserts on EFFECTIVE visibility (only the selected bay's controls are shown,
others present-but-hidden), and a new headless regression drives the exact sequence (Render → swap →
Rotation) through the real `MainConstructorWindow` Elmish loop — it threw before the fix, passes after.
`OpticalConstructor.Ui.Tests` **220/220**.

## Files

- New: `OpticalConstructor.Controls/{Ribbon,RendererControls}.fs`;
  `OpticalConstructor.TestWindows/{Catalogue,ElementRenderer}.fs`;
  `OpticalConstructor.Ui.Tests/RendererControlsTests.fs`.
- Changed: `TableAndElementRotationView.fs` (render + ribbon Model/Msg/update, `mainView`, `mainBays`,
  move/render bays, face-opacity table, `kindName`/`kindCode` dedup); `RendererTestView.fs` (rewritten thin
  onto the shared renderer); `OpticalConstructor.App/Program.fs` (Main → `mainView`, taller window); the
  `Controls` / `TestWindows` / `Ui.Tests` `.fsproj`s; `RendererTestTests.fs` / `TableAndElementRotationTests.fs`.
