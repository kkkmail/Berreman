# Step 0272 — state of the world

## Where we are

Spec 0050 section S8.3 (naming template). Step 268 gave the template grammar, 269 reading and writing names
at text level, 270 re-pointed the lab-label parser, and 271 `setupNameOf` / `setupNameAmong`. This step uses
them in the constructor window: every setup the window creates by a commit or a swap takes the kit's template
name. Step 273 adds the kit-bar field that saves a template, and 276 does the same naming for the grid.

## What's working

- A commit on a table that is no setup yet names the new setup by the kit's naming template, made free among the held setups
- A swap to content no setup holds names the created setup by the template of the swapped table
- A table the template cannot name keeps S8.2's name (`Setup <n>`, `<active> (2)`)
- SetupModelTests covers the template-named commit (`LP1-(QZ#45)-LPd`) and swap (`LP1-(QZ2#45)-LPd`)

## Tests

All run locally on the final tree. None exceeded its effective deadline.

- build: pass (73 s; deadline 600 s).
- constructor-unit-tests: pass, 1446 succeeded (baseline 1444, +2: `a commit on a table the kit's template
  names records it under that name`, `a swap to content no setup holds takes the template's name of the
  swapped table`). Took 68 s against a 180 s deadline. Both facts were confirmed red against S8.2 naming first.
- ui-smoke: pass, 245 succeeded (201 s, run concurrently with the constructor suite; deadline 600 s).
- ui-tests over `ExperimentControlsTests`, `FsxAcceptanceTests`, `InverseConstructorTests`,
  `SetupSaveLoadTests`, `SolverHandoffWindowTests`: pass, 76 succeeded (103 s; deadline 360 s).
- elevation: `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)` (202 s; deadline 600 s).
- invariant-s27-* (all fifteen): pass.

```yaml
gates:
  constructor_unit_tests: 1446
  ui_smoke_tests: 245
  ui_tests: 76
```

## Architecture

- The template is read from the kit file the window holds (`model.kit.kitFile.namingTemplate`, step 131) at
  the moment of the write, never cached, so a template saved in the window names the next setup (step 273).
- `ExpCommit` passes one `recordedSlots` list both to the template and to `Setup.create`, so the name is
  written from exactly the slots recorded.
- `SwapSlot` keeps `Setup.swapTarget`'s content-first rule: a held setup holding the swapped content is
  switched to. The template only names a setup that will be created, with S8.2's name as `setupNameAmong`'s
  `unnamed` fallback.
- `CreateSetup` keeps the typed name, and a copied setup keeps its own. No message, field or option was added.

## Deferred

Nothing. Every requirement of the slice landed this round.

## Gotchas

- Opening `OpticalConstructor.Domain.NamingTemplates` in `TableAndElementRotationView.fs` brings more record
  labels into scope. The build was clean, but a future `{ role = …; … }` there may need an annotation.
- `NamingTemplate.standard` requires Prep 1, Obj 1 (with its angle) and Analyzer 1, which is why bare test
  tables still come out as `Setup 1` / `Bench A (2)`. A fixture that fills those slots with items now gets a
  template name instead.
- Setup file names keep `(`, `)` and `#` (`FileStem.ofText` only replaces `:*?"<>|/\`). File-name lists sort
  ordinally, so `QZ#` comes before `QZ2`.

## Changelog

- 2026-11-10 — Step 0272: the constructor window names new setups (commit on a table that is no setup yet,
  swap to new content) by the kit's naming template, made free, keeping S8.2's name when the template cannot
  name the table; two SetupModelTests facts added (1444 → 1446).
