# 0027-003 — Optical table rotations: diagnosis, plan, and questions

This is the round-1 implementation log for task `002-rotate-table-task.txt`. I have
**not** changed any code yet: the task says to start *very simple* and to **stop and ask**
when I have questions, and there are a few consequential, hard-to-reverse decisions
(most importantly *what "rotate the table" should actually mean*) that I want to get
right before building. My findings and a concrete plan are below; the four questions at
the end each carry a recommended default, so you can answer fast (or just say "go with
your defaults").

---

## 1. What I read

- The three specs: `specs/0022/.spec-md`, `specs/0024/.spec-md`, `specs/0026/.spec-md`
  (0026 Part A "the three rotations", Part C "the top-down table view", Part E
  "commands / mouse / keyboard").
- The live code: `OpticalConstructor.Domain/Table.fs`, `…/Placement.fs`,
  `OpticalConstructor.Ui/ConstructorTable.fs`, `…/ConstructorView.fs`, `…/Ribbon.fs`,
  `…/SystemView3D.fs`, and the composition root `OpticalConstructor.App/Program.fs`.
- `CLAUDE.md` (just authored): Avalonia FuncUI + Elmish; **state separable from UI**;
  push logic into the pure domain and test it headless; automate by stable ID; reuse
  before invention; elevate primitives.

## 2. Diagnosis — why table rotation "does not work" today

There are **two distinct rotation concepts** in the code, and the confusion between them
is most of the problem:

- **Element rotations** (`Placement.r1/r2/r3`) — each *optical element's* own mount
  orientation. These are implemented carefully in `Placement.orientedBasis`
  (`Placement.fs:259`) using Rodrigues rotations, with per-axis locks. This part looks
  correct and is well tested.
- **The table *view* orientation** (`Table.TableViewState.r1/r2/r3`,`Table.fs:87`) — how
  the whole table is turned *relative to the screen*. **This is the one that does not
  work**, and here is exactly why:

  1. The pure top-down projection `ConstructorTable.project` (`ConstructorTable.fs:182`)
     applies **only** scale + Y-flip + pan. It never reads `r1/r2/r3`.
  2. The UI projection `ConstructorView.projectToCanvas` (`ConstructorView.fs:1082`) adds
     **only** a flat *in-plane* spin by `view.r1` (`rotateInPlane`,
     `ConstructorView.fs:1074`) about the canvas centre. **`view.r2` and `view.r3` are
     never read anywhere** — a repo-wide search finds `view.r1` in the projection and
     `view.r2`/`view.r3` nowhere.
  3. The only UI control that rotates the table is the Trace-ribbon "rotate left / rotate
     right" pair (`Ribbon.fs:268-269`), wired to `RotateViewR1` (`ConstructorView.fs:820`,
     `952`). There is **no** `RotateViewR2`/`RotateViewR3` message and no control for them.
  4. The code comments are explicit that this is deliberate-but-incomplete: R2/R3 "keep
     the top-down approximation" (`ConstructorView.fs:1068-1073`,
     `ConstructorTable.fs:165-171`) — i.e. they do nothing.

  **Net:** the table can only be spun *flat in its own plane* (R1). There is no true
  3-D tumble of the table and no real 3-D→screen projection — which is exactly the "3D
  rotations and 3D projections to the screen" you flagged. Spec 0026 §C.2.4 actually asks
  for more than what's built: *"the table's own R1/R2/R3 MUST be measured relative to the
  screen … all elements travel with it,"* and §C.2.5 adds a reset-view *"because the view
  is easy to tumble and hard to recover by hand"* — the word **tumble** implies a real
  3-D orbit, not a flat spin.

- **Table selection** (`SelectAt` → `hitTest` → `TableSelected | ElementSelected i`,
  `ConstructorView.fs:258,852`) updates state correctly, but **selecting the table draws
  no visible feedback** — only the active *element* gets a halo (`indicatorView`,
  `ConstructorView.fs:1273`). So "select / unselect the table" looks like nothing happens.

## 3. Default optical table (from the code)

`Table.defaultTable` = **1.2 m (width, +Y) × 2.0 m (length, +X) × 0.10 m (thickness, +Z)**
(`Table.fs:28-36`), display unit mm, default view = straight top-down `(0,0,0)`, zoom 1.0.
(Your "1 m × 2 m × 10 cm" guess — it's 1.2 × 2.0 × 0.10.) I will use these as-is.

## 4. Proposed plan (once the questions below are settled)

1. **A simple launcher window** with exactly two buttons — `Main` and
   `Test Optical Table Rotations`. `Main` opens the existing Optical Constructor window
   (unchanged); the test button opens the first test window.
2. **A separate project** `OpticalConstructor.TestWindows` (one `.fsproj` per folder)
   holding the launcher + all future test windows, referenced by `OpticalConstructor.App`.
3. **A pure, headless-testable projection core** that does the table rotation **once**, so
   the SAME logic drives both the main app and the test window (single source of truth — I
   will *fix the real table projection*, not build a parallel one): rotate the table (the
   1.2×2.0×0.10 m plate as a 3-D box) by the view's R1/R2/R3 *relative to the screen*, then
   orthographically project to the canvas; default `(0,0,0)` = exactly today's top-down.
4. **The first test window**: renders `Table.defaultTable` from the top, with controls to
   rotate R1/R2/R3, reset to top-down, and select / unselect the table (with a *visible*
   selection indicator), driving the core in (3).
5. **Headless tests** (xunit.v3 + FsCheck, matching the repo) that prove, with no real
   window: top-down projection at `(0,0,0)`; each axis rotation moves screen points the
   right way; the inverse (screen→table) round-trips; selection hit-test; reset. This is
   the "test from the headless carcass first" you asked for; then it's yours for the final
   on-screen UI test.

## 5. Questions (each with a recommended default)

**Q1 — What should "rotate the table" mean?** *(the important one)*
You said "we look at it from the top." Two readings, and they change the core math:
- **(A) RECOMMENDED — a true 3-D tumble.** The table is a 3-D plate; R1/R2/R3 (relative to
  the screen) orbit it in 3-D and it is orthographically projected to the canvas, so you
  can tilt it out of the flat top-down plane; reset returns to top-down. Matches spec
  §C.2.4 / §C.2.5 and your "3D projections to the screen" framing.
- **(B) Flat in-plane spin only.** Keep R2/R3 as the top-down approximation (do nothing);
  only R1 spins the table flat on screen. This is the (partial) thing built today.

**Q2 — Should the launcher become the app's startup window?** When you run
`OpticalConstructor.App`, it would now show the two-button launcher, and `Main` opens the
existing Optical Constructor window unchanged. **RECOMMENDED: yes.** Alternative: leave the
app starting straight into the main window and make the launcher a separate dev-only entry.

**Q3 — New project layout.** Create `OpticalConstructor.TestWindows` (+ a sibling
`OpticalConstructor.TestWindows.Tests` for the headless tests), referenced by `.App`.
**RECOMMENDED: yes**, per the one-`.fsproj`-per-folder rule. (Alternative: put the headless
tests in the existing `OpticalConstructor.Ui.Tests`.)

**Q4 — Toolkit for the launcher + test window.** **RECOMMENDED: Avalonia FuncUI**, so the
test window reuses the *exact same* optical-table domain and rendering as the main app
(your "exactly the same optical table, NOT a separate one"). You mentioned (task 001) you
may drop Avalonia for WinForms — so I'll keep all rotation/projection/selection logic in
the pure, rendering-agnostic domain, which makes a later WinForms port cheap. Confirm
Avalonia, or say WinForms if you'd rather I prototype the test shell there.

---

**If you're happy with all four defaults, just reply "go with your defaults" and I'll
build it.** Otherwise tell me which to change. I'll then implement and write the full
implementation log as `0027-004-…md`.
