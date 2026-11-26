# Step 0215 — state of the world (IMPLEMENT: the item, its dial and its revisions)

## Where we are

Spec 0050, section S6 (send and receive kits; copy experiments between kits), is landing the shared item
contracts. They must exist before S7, so that copying an experiment can map its items (001 §H, §S6; S7.1;
001 §I).

- Step 213 added an item's identity and the model version it instantiates.
- Step 214 added a parameter and its state.
- This step adds the `Item` record to the `Kit` module, after `Parameter`, together with its dial mounting and its
  revision history.
- Step 216 adds `ItemDraft` / `AddItem`, which builds an item's parameters from the model's axes and then
  `dial.parameterAxes`.
- Step 217 stores an item as `<label>.item-json`.
- Step 219 records an item through `snapshotReferences item.libraryReferences`.

## What's working

- Add `Kit.ItemDial` (`DialMounted of DialSign` | `FixedMounting`). Its `parameterAxes` publishes the one
  `dialZero` angle axis (default 0) for a dial-mounted item and nothing for a fixed one, so the zero is an ordinary
  parameter.
- Add `Kit.ItemRevisionRecord`, the parameter values an earlier revision held.
- Add `Kit.Item`: id, label, model, own parameters, dial, photo, notes, revision and history.
- `Item.libraryReferences` names the model's exact version, then its bound physics, each once.
- `Item.tryFindParameter` finds a parameter by name.
- Add two `KitItemTests` facts. Constructor unit tests: 1294 → 1296.

## Tests

These are development runs, recorded in `0215-impl-log.md`. The gate engine gives the verdict (Invariant 6).

- **build:** exit 0, 0 errors; the final build had 0 warnings.
- **constructor-unit-tests:** 1296 passed, 0 failed, 0 skipped (+2), in 64 s against a 180 s estimate. Both
  declared identities pass:
  - `an item references its model's library version once and finds its parameters by name`;
  - `a dial-mounted item publishes its dial zero and a fixed one nothing`.
- **The facts bite.** Four mutations each fail exactly one fact, and the source was then restored:
  - no `List.distinct`;
  - the binding listed before the model;
  - no dial-zero axis;
  - `tryFindParameter` answering the first parameter.
- **elevation** (from the arc origin `f395597`, ELEV004 excluded): `ELEV-GATE: OK - 0 new finding(s) in 12
  project(s)`, 192 s against a 600 s deadline.
- **The fourteen `invariant-s22-*` gates:** each holds.
- The UI suites are not in this step's roster. No UI file or shared fixture changed.

```yaml
gates:
  constructor_unit_tests: 1296
```

## Architecture

- **The item is pure Domain data in `Kit`, appended in the brief's order after `Parameter`.**
  - Its model is the `ItemModelInstance` from step 213, so every item carries an exact item-model version. A
    preset or sample appears only as the typed binding of that instance, never as the model.
  - Its photo is a `KitRelativePath` to a kit file.
- **The item holds its own `parameters` list.** Two items of one model never share parameters (001 §I).
- **`libraryReferences` is what a kit snapshots and pins when it records the item** (step 219). It is the model's
  `entryReferenceOpt`, then its `bindingReferenceOpt`, de-duplicated with the first occurrence kept.
- **The dial maps a raw reading to the physical angle as `sign · (reading − dialZero)`**, the rule of
  `MuellerReconstruction.physicalAngle`.
  - The sign is the `DialSign` of `DialMounted`.
  - The zero is the item's `dialZero` parameter, published through `ItemDial.parameterAxes`.

## Deferred

Nothing in this step's scope. By the spec's plan:

- `ItemDraft` / `AddItem` is step 216;
- the `.item-json` file and schema is step 217;
- the kit's item read and write is step 218;
- `recordItem` is step 219.

None of these is deferred by this step.

## Gotchas

- **`Item` repeats record labels from earlier types:** `id` and `notes` (`KitFile`), `parameters` and `revision`
  (`ItemRevisionRecord`). Annotate record values with their type.
- **The test file's shared built-in fixtures are now module-level private values:** `sourceModel`,
  `polarizerModel`, `stackModel`, `sampleVersion`, `sampleAtVersion` and `presetAtLatest`. Later `KitItemTests`
  facts should reuse them rather than redefine them.
- **The vendored MathNet `Numerics` project prints two `SYSLIB0051` warnings whenever it recompiles.** This is
  pre-existing and not from this step's files.
- **In this Git Bash, count CR bytes with `tr -cd '\r' | wc -c`.**

## Changelog

- 2026-11-09 — step 0215: added `Kit.ItemDial` (`parameterAxes`), `ItemRevisionRecord` and `Item`
  (`libraryReferences`, `tryFindParameter`); added two `KitItemTests` facts and moved their shared built-in
  fixtures to module level; constructor unit tests 1294 → 1296.
