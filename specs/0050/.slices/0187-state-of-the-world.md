# Step 0187 — state of the world

# Where we are

Step 0187 is in section S5.3 (the kit window: title, Kit menu, lock, reload and mode), after step 0186's Kit menu
verbs and constructor switch. It adds the reload offer to the workbench model. The offer is model state and three
semantic messages over the step-130 stamps. Step 188 renders it in the kit bar, together with the Kit menu's Reload
changes. Step 189 runs the check on every window activation.

# What's working

- Add `KitCheckOrigin` and `kitChanges`, the kit files changed outside the app that the window offers to reload.
- Add `CheckKitChanges`: it offers exactly the files another writer changed, never the window's own writes, and says
  "nothing changed" only on request.
- Add `ReloadKitChanges`: it re-reads the kit file, experiments and scene names, keeping the kit's proxy, access and
  clock, and clears the offer.
- Add `KeepMyKitChanges`: it keeps the model and records the current stamps, so only a later change is offered.
- Move what `loadKit` sets from a kit's content into one private function, which a reload also uses.
- Add the five declared `KitWindowModelTests` facts, covering a read-only kit's reload.

# Tests

No gate was run (Invariant 6: the `IMPLEMENT` role acts only). The arc-runner runs the frozen roster after exit:

- build and elevation;
- constructor-unit-tests (all discovered);
- ui-smoke (all discovered);
- ui-tests (`InverseConstructorTests`, `SceneSaveLoadTests`);
- the fourteen s21 invariants.

Expected counts:

- constructor-unit-tests rises by exactly the five declared facts, from checkpoint 185's 1202 to 1207;
- ui-smoke stays at 227;
- the selected UI classes are unchanged, and their `loadKit` facts run over the moved body.

The YAML carries checkpoint 185's measured floors; the supervisor measures this step's.

```yaml
gates:
  constructor_unit_tests: 1202
  ui_smoke_tests: 227
```

# Architecture

- **The offer is model state.** `kitChanges : FileName list` holds what `changedKitFiles` answered. Step 188's kit bar
  renders it, and its Reload / Keep mine buttons send `ReloadKitChanges` / `KeepMyKitChanges`. No file watcher, timer,
  merge, per-file choice or diff is added.
- **An open and a reload set the same fields.** The private `withKitContent` (kit context, experiments, scene names)
  sets:
  - the kit;
  - the collection, with a fresh draft;
  - the scene names;
  - the rechecked attachment statuses;
  - no status and no offer.

  `loadKit` adds only `kitFolderOpt` and `experimentData`, and sets them before the shared function, so the recheck
  reads the opened kit's data proxy.
- **Nothing in the reload path writes.** The check, the reload and Keep mine only read and record stamps, so a
  read-only kit is offered reloads through `readOnlyView`.
- **A scratch kit is never checked.** `guardKitFolder` (the `guardKitWrite` shape) answers a model with no kit folder
  unchanged, because nothing outside the app can change a scratch kit.

# Deferred

Nothing. Every requirement of the slice is in this round. Step 188 renders the offer and the Kit menu's Reload
changes, and step 189 checks on window activation; neither was part of this slice.

# Gotchas

- `changedKitFiles` compares content hashes, so rewriting identical bytes is not offered. A test's further write has
  to change content.
- The offered names are leaf names in ordinal order: `.kit-json` sorts before `E2.experiment-json`.
- `withKitContent` also clears `kitStatus` and `kitChanges`, so opening a kit clears any offer the model held.
- A reload replaces only the kit's `kitFile`. The proxy, `access` and `clock` are the model's own, so a read-only kit
  stays read-only after a reload.

# Changelog

- 2026-11-08 — attempt 01:
  - added `KitCheckOrigin`, `kitChanges` and the `CheckKitChanges`, `ReloadKitChanges` and `KeepMyKitChanges`
    messages and arms;
  - moved `loadKit`'s content setting into `withKitContent`;
  - added the five declared `KitWindowModelTests` facts.
