# Step 0216 — impl log

## Progress

- [x] (1) `Kit.fs`: appended `ItemModelChoice` (`instance`, `parameterAxes`) and `ItemDraft` (`toItem`, `addTo`) after
  `Item`
- [x] (2) `KitItemTests.fs`: appended the four declared facts, and lifted the three built-in `ItemModelVersion` fixtures
  to module level
- [x] Development runs: build, `KitItemTests`, mutation check, constructor-unit-tests, elevation and the fourteen
  invariants

## Files modified

### `Berreman/OpticalConstructor/OpticalConstructor.Domain/Kit.fs`

Appended at the end of the `Kit` module, after `Item`.

- **`ItemModelChoice`**, a model as the library resolved it:
  - `SourcePresetChoice of model : ItemModelVersion * source : SourcePreset`;
  - `PolarizerPresetChoice of model : ItemModelVersion * polarizer : PolarizerPreset`;
  - `StackSampleChoice of model : ItemModelVersion * version : SampleVersionId * sample : Sample`;
  - `LibraryModelChoice of ItemModelVersion`.
- **`ItemModelChoice.instance : ItemModelInstance`** carries `model.versionId` in every case:
  - `PresetInstance (model.versionId, preset.id)`;
  - `SampleInstance (model.versionId, version)`;
  - `ItemModelVersionInstance model.versionId`.
- **`ItemModelChoice.parameterAxes : ItemParameterAxis list`:**
  - A binding choice answers `model.entry.model.tryBoundParameterAxes` over `SourcePresetBound` /
    `PolarizerPresetBound` / `StackSampleBound`. That is step 208's bound derivation, and it calls step 207's
    `SourcePreset` / `PolarizerPreset` / `Sample.parameterAxes`. It answers empty when the form takes no such
    binding.
  - `LibraryModelChoice` answers `entry.model.parameterAxes`.
- **`ItemDraft = { id : ItemId; label : ItemLabel; model : ItemModelChoice; dial : ItemDial }`.**
- **`ItemDraft.toItem : Item`**:
  - the draft's id, label, `model.instance` and dial;
  - `parameters` = `model.parameterAxes @ dial.parameterAxes |> List.map Parameter.knownAtDefault`;
  - `photoOpt = None`, `notes = ItemNotes ""`, `revision = ItemRevision.first`, `history = []`.
- **`ItemDraft.addTo (items : Item list) : Result<Item, ItemError>`**, in this order:
  1. `DuplicateItemIdErr` when a held item has the id. It names the draft's label, the id and the holder's label.
  2. `DuplicateItemLabelErr` when a held label's `fileStem` equals the draft's under `OrdinalIgnoreCase`. It names
     both labels and the file name they share.
  3. `InvalidItemModelBindingErr`, naming the model (its name and `GUID vN`) and the binding, in four cases:
     - a binding choice whose model's form takes another binding or none (`tryBoundParameterAxes` = `None`);
     - a preset id that is not a canonical GUID (`tryToLibraryEntryId` = `None`, step 137's rule);
     - a sample version naming another sample than the one carried;
     - a `LibraryModelChoice` whose form requires a binding (`bindingKindOpt` = `Some _`).
  4. Otherwise `Ok this.toItem`. It writes nothing.

### `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitItemTests.fs`

- The module-level fixtures now hold the three built-in binding models as `ItemModelVersion`:
  - `sourceItemModel`;
  - `polarizerItemModel`;
  - `stackItemModel`.

  The existing `sourceModel` / `polarizerModel` / `stackModel` are their `.versionId`, the same values as before.
  The drafts need the version records, and AGENTS.md says to reuse rather than copy.
- New private helpers:
  - `polarizerPreset` returns the Built-in polarizer preset by id;
  - `knownAt` makes a known parameter;
  - `added` answers the item that `addTo` gives, failing with the refusal's text.
- The four declared facts, appended:
  - `adding an item from a library model creates its parameters known at the model's defaults and its dial zero`;
  - `a source preset, an ideal polarizer and a sample give an item their own parameters`;
  - `two items of one model are two items with equal starting parameters`;
  - `adding an item whose id or label file name the kit already holds is refused`.

## Decisions

1. **What a mismatched choice's `parameterAxes` answers.** `parameterAxes` has a plain list as its contract. A
   binding choice whose model's form does not take that binding answers `[]` (`tryBoundParameterAxes` →
   `Option.defaultValue []`). `addTo` refuses such a draft with `InvalidItemModelBindingErr` before `toItem` runs.
   So no recorded item can carry the empty axes. The doc comment says so.
2. **"Absent / mismatched model" maps to one check.** `addTo` is given an already-resolved `ItemModelVersion`, so a
   model cannot be missing from the choice. A binding choice is therefore refused whenever its model's
   `bindingKindOpt` is not the choice's binding kind:
   - `None` is the absent case: a standalone model given a preset or a sample;
   - another kind is the mismatched case.

   One `tryBoundParameterAxes` call (step 208) decides both, so the kind check is not written twice.
3. **A sample version must name the sample it carries.** In `StackSampleChoice (model, version, sample)`, the
   `version` is the binding reference that `Item.libraryReferences` pins (step 215). The `sample` supplies the
   parameter axes. If `version.sampleId <> sample.id`, the item would pin one sample and take its thicknesses from
   another. That is a mismatched binding, so `addTo` answers `InvalidItemModelBindingErr`. This adds no member,
   option or default, only one rule inside `addTo`.
4. **Check order.** The order is id, then label file name, then binding, which is the brief's order. A draft that
   breaks several rules is refused by the first.
5. **Test ids are fixed GUID literals.** Steps 213–215 used `Guid.NewGuid()`. Fixed literals keep each fact
   deterministic, and the brief asks for "distinct test-local ids".
6. **The source/polarizer/sample fact builds its kit through `addTo`.** The laser goes into an empty kit, LP1 beside
   it, and the plate beside both. That fact then exercises eight refused choices against that kit:
   - a source preset under the ideal-polarizer model;
   - a source preset under the standalone circular-polarizer model;
   - a polarizer preset under the stack model;
   - a polarizer preset whose id is not a GUID;
   - the glass film's version carrying the glass plate;
   - the plate under the source model;
   - `LibraryModelChoice` of the source model;
   - `LibraryModelChoice` of the stack model.

   Each answers `InvalidItemModelBindingErr` naming the model, and the kit still holds exactly the three items.

## Testing state

These are development runs; the gate engine gives the verdict (Invariant 6).

- **build** (`dotnet build Berreman.slnx -c Release -nologo -v:m`, from `Berreman/`): exit 0, 0 errors.
  - The first build showed the two pre-existing `SYSLIB0051` warnings. They come from the vendored
    `MathNetNumerics/Numerics/Exceptions.cs` whenever it recompiles, and steps 214–215 recorded the same. They are
    not from this step's files.
  - The final build over the restored source had 0 warnings and 0 errors (`0216-dev-build-final.log`). Every copy
    of `Softellect.Berreman.OpticalConstructor.Domain.dll` is newer than the restored `Kit.fs`.
- **KitItemTests:** 12 / 12 passed: the eight facts from steps 213–215 and the four new ones.
- **constructor-unit-tests**, the whole suite, run with the gate's command shape (TRX included): 1300 passed,
  0 failed, 0 skipped, in 1 min 12 s against a 180 s estimate. The count rose from 1296 by 4, as the acceptance
  requires.
- **Mutation check** (`0216-dev-mutations/`): each mutation was applied to `Kit.fs`, the test project rebuilt and
  `KitItemTests` re-run. Every mutation was caught, and the source was then restored byte for byte (`diff -q`
  against `Kit.fs.orig`).
  - **`dial-first`** (the dial's axes before the model's): fails the library-model fact and the two-items fact.
  - **`ordinal-label`** (a case-sensitive file-name comparison): fails the duplicate fact.
  - **`no-id-check`**: fails the duplicate fact.
  - **`no-guid-check`**: fails the source/polarizer/sample fact.
  - **`no-sample-version-check`**: fails the source/polarizer/sample fact.
  - **`no-unbound-library-check`**: fails the source/polarizer/sample fact.
- **elevation** (`ASG_ARC_BASE_COMMIT` = the `.arc-base-json` origin `f395597`, ELEV004 excluded):
  `ELEV-GATE: OK - 0 new finding(s) in 12 project(s), 201s` against a 600 s deadline.
- **The fourteen `invariant-s22-*` gates**, each run through `section_invariants --find-nothing` exactly as its
  gate command is: each answers `INVARIANT HOLDS`.
- **LF endings:** neither edited file has a CR byte, and `git diff --numstat` equals
  `--ignore-cr-at-eol --numstat`.

## Artifacts

All in `specs/0050/.artifacts/`:

- `0216-dev-build.log`: the first build;
- `0216-dev-build-final.log`: the build over the restored source after the mutation check;
- `0216-dev-constructor-unit-tests.log`: the whole-suite run;
- `0216-dev-mutations/`:
  - `mutate.py`, the six mutations;
  - `Kit.fs.orig`, the restored source;
  - `results.txt`, the per-mutation failures;
  - `build-<mutation>.log`;
- `0216-dev-elevation.log`: the ELEV gate run;
- `0216-dev-invariants.log`: the fourteen invariant runs.

## Gotchas

- **`ItemDraft` shares the record labels `id`, `label`, `model` and `dial` with `Item`.** It also shares `id` with
  `KitFile`, and `model` with `ItemModelEntry`. `ItemDraft` is now the latest record with those labels, so an
  unannotated `{ id = …; label = …; … }` infers `ItemDraft`. `toItem` annotates its result (`let item : Item = …`),
  and the tests annotate every record value. The solution built clean, so no other site relied on the old
  inference.
- **No operator note was in flight for this attempt.**
