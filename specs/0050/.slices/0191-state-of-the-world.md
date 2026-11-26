# Step 0191 — state of the world

# Where we are

Step 0191 (`WIRE_UI`) completes "one window per kit" in section S5.3 (001 §E, "Concurrency: one window per kit").
Step 0190 gave the host-layer `WindowRegistry` a key for a constructor window, its kit's canonical folder, and made
the start screen ask that registry first. This step registers the real constructor windows under that key, at the
kit hand-off. The start screen closes after bringing the window forward. Choosing a kit whose window is open, in
any spelling of its folder, now brings that window forward and opens nothing. Once the window is closed, the kit
opens again.

# What's working

- `KitConstructorWindows.showConstructorIn` opens each constructor window through `WindowLauncher.create`, under
  `opened.folder.windowKey`.
- When a window already holds the kit, or the open fails, the opened kit is closed again. A failed open logs its
  reason.
- Choosing a kit whose window is open brings that window forward, opens no second window and keeps the window's
  lock. A closed window's kit opens again.
- Add the `ui-smoke` facts `acceptance: choosing a kit whose window is open activates that window and opens no
  second one` and `a kit can be opened again once its window is closed`.

# Tests

No gate verdict is claimed here (Invariant 6). The arc-runner runs the frozen roster after exit:

- build and elevation;
- constructor-unit-tests (all discovered);
- ui-smoke (all discovered);
- ui-tests over `OpticalConstructor.Ui.Tests.LauncherTests`;
- the fourteen s21 invariants.

The worker's development runs are in the impl-log's `Testing state`, with logs under
`specs/0050/.artifacts/0191-*.log`:

- build: 0 errors, no owned-code warning;
- `LauncherTests`: 25 passed;
- the red check over the old unregistered `Show ()`: exactly the two new facts failed, each on the missing
  behaviour;
- ui-smoke: 232 passed, which is 230 + 2, the two declared facts;
- the whole non-smoke UI suite: 649 passed, unchanged;
- constructor-unit-tests: 1209 passed, unchanged;
- elevation from the arc origin: `ELEV-GATE: OK`, 0 new findings in 12 of 12 projects;
- the fourteen s21 invariant greps: none finds anything.

Expected counts:

- ui-smoke +2 (230 → 232);
- constructor-unit-tests unchanged at 1209.

No test is removed or renamed, and no assertion is weakened. The YAML carries the latest recorded checkpoint's floors
(checkpoint 189); the supervisor measures this step's.

```yaml
gates:
  constructor_unit_tests: 1209
  ui_smoke_tests: 230
```

# Architecture

- **One registry, one entry point.** A constructor window over a chosen kit is born only in `showConstructorIn`,
  and only through `WindowLauncher.create`. It meets every other managed window in the same `WindowRegistry`. The
  window's `Closed` hook releases the kit (step 154). The registry forgets the window when it closes
  (`WindowLauncher.fs:225`).
- **The window owns the kit.** When the launcher answers with an existing window (`ActivatedWindow` /
  `RetargetedWindow`), the second open's kit is closed, so exactly one window holds a kit. A failed factory or show
  also closes the kit, because no window will ever release it.
- **Direct construction stays unregistered.** Tests and composition sweeps that build `MainConstructorWindow` /
  `InverseConstructorWindow` directly never register, so they cannot collide on a key.

# Deferred

Nothing. Every requirement of the slice is in this round.

# Gotchas

- The start screen's own activation (step 190) handles the normal case, so `showConstructorIn` sees
  `ActivatedWindow` only in a race: activation fails, the open then meets this process's lock, and the user opens
  read-only. Closing that read-only kit releases nothing.
- The real launcher's `openOrActivate` never answers `WindowNotRegisteredErr`. The arm exists so the match is
  exhaustive without a wildcard.
- On Windows the test kit folder already uses backslashes, so "written with backslashes and in upper case" differs
  from the original by case only. The fact asserts the two spellings differ.
- `LauncherTests.withWindowClosingLauncher` closes every window the fact opened, even on a failed assertion, so no
  `KitWindowKey` outlives a fact. A new launcher fact that opens a constructor window should use it, or close its
  window in `finally`.

# Changelog

- 2026-11-08 — attempt 01:
  - `showConstructorIn` registers each constructor window under its kit's folder through `WindowLauncher.create`;
  - the opened kit is closed when a window already holds it or the open fails;
  - the two declared `ui-smoke` facts.
