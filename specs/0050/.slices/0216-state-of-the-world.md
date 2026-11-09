# Step 0216 — state of the world (IMPLEMENT: AddItem as `ItemDraft`)

## Where we are

Spec 0050, section S6 (send and receive kits; copy experiments between kits), is landing the shared item
contracts. They must exist before S7, so that copying an experiment can map its items (001 §H, §S6; S7.1;
001 §I, §P).

- Step 213 added an item's identity and its `ItemModelInstance`.
- Step 214 added a parameter and its state.
- Step 215 added the `Item` record with its dial and revisions.
- This step adds `AddItem`, as a pure update over the kit's items:
  - `ItemModelChoice`, a model as the library resolved it;
  - `ItemDraft`, what `AddItem` gives;
  - `ItemDraft.toItem`;
  - `ItemDraft.addTo`.
- Step 217 stores an item as `<label>.item-json`.
- Step 218 reads and writes a kit's items.
- Step 219 records an item through `recordItem`.

## What's working

- Add `Kit.ItemModelChoice`: a source preset, a polarizer preset, or a sample version over its item model, or a
  standalone library model.
- `ItemModelChoice.instance` always carries the model's exact version. `parameterAxes` answers the model's axes over
  its typed binding (steps 207, 208).
- Add `Kit.ItemDraft`. `toItem` creates every parameter the model and then the dial publish, known at its default,
  at revision 1, with no history, photo or notes.
- `ItemDraft.addTo` refuses three kinds of draft:
  - a held id (`DuplicateItemIdErr`);
  - a label whose file name is held, ignoring case (`DuplicateItemLabelErr`);
  - a model that does not take its binding, a non-GUID preset, a sample version of another sample, or an unbound
    binding model (`InvalidItemModelBindingErr`).
- Add four `KitItemTests` facts. Constructor unit tests: 1296 → 1300.

## Tests

These are development runs, recorded in `0216-impl-log.md`. The gate engine gives the verdict (Invariant 6).

- **build:** exit 0, 0 errors. The final build over the restored source had 0 warnings.
- **constructor-unit-tests:** 1300 passed, 0 failed, 0 skipped (+4), in 72 s against a 180 s estimate. All four
  declared identities pass:
  - `adding an item from a library model creates its parameters known at the model's defaults and its dial zero`;
  - `a source preset, an ideal polarizer and a sample give an item their own parameters`;
  - `two items of one model are two items with equal starting parameters`;
  - `adding an item whose id or label file name the kit already holds is refused`.
- **The facts bite.** Six mutations of `Kit.fs` were each caught by the fact they target, and the source was then
  restored byte for byte:
  - the dial's axes first;
  - a case-sensitive file-name comparison;
  - no id check;
  - no GUID check;
  - no sample-version check;
  - no unbound-library check.
- **elevation** (from the arc origin `f395597`, ELEV004 excluded): `ELEV-GATE: OK - 0 new finding(s) in 12
  project(s)`, 201 s against a 600 s deadline.
- **The fourteen `invariant-s22-*` gates:** each holds.
- The UI suites are not in this step's roster. No UI file or shared fixture changed.

```yaml
gates:
  constructor_unit_tests: 1300
```

## Architecture

- **`AddItem` is pure Domain data and logic in `Kit`, appended after `Item`.**
  - `addTo` takes the kit's items and answers the new `Item` or an `ItemError`. It writes nothing.
  - The caller records the item. Step 219's `recordItem` snapshots `item.libraryReferences` first, then writes.
- **Every item carries an exact item-model version.**
  - `ItemModelChoice.instance` maps each case to the `ItemModelInstance` holding `model.versionId`.
  - A preset or sample appears only as the typed binding of that instance, never as the model.
  - There is no direct-preset or direct-sample path.
- **Where the axes come from.**
  - A binding choice reuses `ItemModelForm.tryBoundParameterAxes` (step 208) over `SourcePresetBound` /
    `PolarizerPresetBound` / `StackSampleBound`. That calls step 207's preset and sample `parameterAxes`, so no axis
    computation is copied.
  - `addTo` decides "the model takes this binding" by the same call.
- **The item's parameters are the model's axes, then the dial's** (`dialZero` for a dial-mounted item), each through
  `Parameter.knownAtDefault`. That is the one way an item's parameters are made (step 214).
- **The label rule.** Every `<label>.item-json` shares the kit root on Windows. So a label clashes when its
  `FileStem.ofText` equals a held one's under `OrdinalIgnoreCase`. For example, `lp1` clashes with `LP1`, and
  `LP:1` with `LP_1`.

## Deferred

Nothing in this step's scope. By the spec's plan:

- the `.item-json` file and schema is step 217;
- the kit's item read and write is step 218;
- `recordItem` (snapshot, pin, then write) is step 219.

None of these is deferred by this step.

## Gotchas

- **`ItemDraft` shares the record labels `id`, `label`, `model` and `dial` with `Item`.** It is now the latest
  record with those labels, so an unannotated record expression with them infers `ItemDraft`.
  - `toItem` annotates its `Item`, and the tests annotate every record value.
  - Later code building an `Item` record must annotate it too.
- **A mismatched choice's `parameterAxes` is `[]`.** `addTo` refuses such a draft, so call `addTo`, not `toItem`
  directly, for anything the user chose. Tests and storage fixtures may call `toItem` on drafts known to be valid,
  as steps 217–219 do.
- **`addTo` checks in this order: id, then label file name, then binding.** A draft breaking several rules gets the
  first refusal.
- **The test module now holds the three built-in binding models as `ItemModelVersion`:** `sourceItemModel`,
  `polarizerItemModel` and `stackItemModel`. The existing `*Model` version ids are their `.versionId`. The helpers
  `polarizerPreset`, `knownAt` and `added` are private to `KitItemTests`.
- **The vendored MathNet `Numerics` project prints two `SYSLIB0051` warnings whenever it recompiles.** This is
  pre-existing and not from this step's files.

## Changelog

- 2026-11-09 — step 0216: added `Kit.ItemModelChoice` (`instance`, `parameterAxes`) and `ItemDraft` (`toItem`,
  `addTo`); added four `KitItemTests` facts and module-level built-in model fixtures; constructor unit tests
  1296 → 1300.
