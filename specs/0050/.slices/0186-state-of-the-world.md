# Step 0186 — state of the world

# Where we are

Step 0186 is in section S5.3 (the kit window: title, Kit menu, lock, reload and mode). It follows step 0185's
read-only kit behaviour and checkpoint 184. It adds the Kit menu's verbs and the forward / inverse switch as
semantic Elmish messages over an injected `KitWindowShell`. The test scenes hold the inert shell. Step 189
composes the real shell (the system explorer and the hosting window's close) into the windows.

# What's working

- Add `KitWindowShell` (`explorer`, `requestClose`) and its `inert` shell, held by every `initWith` model as `kitShell`.
- Add `SwitchConstructorMode`: it changes only `constructorMode`, keeping the kit, table, experiments, draft and bay.
- Add `ShowKitInExplorer`: it asks the shell's explorer for the kit's folder and tells a refusal in `kitStatus`; a
  scene with no folder answers unchanged.
- Add `CloseKit`: it asks the shell to close the window and changes nothing else.
- Add the five declared `KitWindowModelTests` facts.

# Tests

No gate was run (Invariant 6: the `IMPLEMENT` role acts only). The arc-runner runs the frozen roster after exit:

- build and elevation;
- constructor-unit-tests (all discovered);
- ui-smoke (all discovered);
- ui-tests (`OpticalConstructor.Ui.Tests.KitWindowTests`);
- the fourteen s21 invariants.

Expected counts:

- constructor-unit-tests rises by exactly the five declared facts, from checkpoint 184's 1197 to 1202;
- ui-smoke stays at 227;
- the selected `KitWindowTests` class is unchanged.

The YAML carries checkpoint 184's measured floors; the supervisor measures this step's.

```yaml
gates:
  constructor_unit_tests: 1197
  ui_smoke_tests: 227
```

# Architecture

- **The shell is everything the Kit menu reaches outside the model.** `KitWindowShell` follows the
  functional-proxy convention: a `[<ReferenceEquality>]` record of the step-182 `ExplorerProxy` and a
  `requestClose`, as `LibraryWindowContext.requestClose` closes its window.
- **The model stays pure.** The arms call the shell's functions and return the model unchanged, apart from the
  explorer-refusal status.
- **The switch reuses `ConstructorMode`.** `WindowMode.fs`'s `LibraryWindowMode` is a catalogue window's Browse /
  Select state, not a constructor mode. The chart and Data-files gating already read `constructorMode`.
- **Close kit loses nothing** (.spec-md §10.5). Every kit write is already on disk, and the hosting window's
  `Closed` handler releases the kit (step 154), so the message adds no confirmation, cache or save.

# Deferred

Nothing. Every requirement of the slice is in this round. Step 189 owns composing the real shell into the windows;
that work was never part of this slice.

# Gotchas

- `KitWindowShell` compares by reference, so a model whose `kitShell` is replaced equals only models built from
  it. `KitWindowShell.inert` is a static property, and each access builds a fresh shell.
- `CloseKit` does not release the kit lock; the window's `Closed` handler does. A model-level test sees
  `.oc/lock` until it closes the kit itself.
- The slice's line references lag the file. Anchor on the symbols (`EditorLaunchers`, `ConstructorMode`,
  `experimentChartVisible`), not on the line numbers.

# Changelog

- 2026-11-08 — attempt 01: added `KitWindowShell` / `inert`, the model's `kitShell`, and the `SwitchConstructorMode`,
  `ShowKitInExplorer` and `CloseKit` messages and arms, with the five declared `KitWindowModelTests` facts.
