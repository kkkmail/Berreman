# Step 0221 — impl-log (IMPLEMENT, S72-COPY-MAPPING)

## Progress

### Survey

- Read the system prompts, the project prompt, the slice, manual 015 and step 0220's state of the world.
- Surveyed every `ElementDescriptor` record literal. It is built in:
  - `TableAndElementRotationView.fs`;
  - `ExperimentProxyTests.fs`, `ExperimentCollectionRoundTripTests.fs` and `KitContentFileJsonTests.fs`;
  - **`KitLibraryTests.fs`**, which is outside the edit list (see Gotchas).
- `ExperimentDataLoad.fs` and `SolverHandoffView.fs` only read descriptors (`detectorDescriptorOpt`, `binding`). They
  compile against the enlarged record unchanged, and their detector and validation behaviour is untouched.
- `ExperimentCollectionProxyTests.fs` is absent (deleted at step 152). It stays absent and nothing was recreated.

### Domain (`Kit.fs`, after the step 213–217 item declarations)

- **`ItemLabel.nextFreeAmong`** answers the label, then `<label> (2)`, `(3)`, … until no taken label shares its
  `fileStem`, ignoring case. It is the suffix form of `KitDataFolder.copyTargetFor`.
- **`ItemCopyChoice`** is `MapOntoKitItem of ItemId | AddCopiedItem`. It is never stored, so it has no `value` tag.
- **`Item.defaultCopyChoice held`** picks, in order:
  1. the held item of the same id;
  2. the first held item whose `model` (the `ItemModelInstance`, step 216's payload) and `label` are equal (DU
     equality, so the label is matched ordinally);
  3. otherwise `AddCopiedItem`.
- **`ItemCopyRow = { sourceItem; choice }`** has two intrinsic statics:
  - `copyRowsFor used held chosen` gives one row per used item, in order. It keeps a chosen row's choice by
    `sourceItem.id` and otherwise takes the default.
  - `itemsToAdd rows held` checks every row first, through `List.tryPick` over all rows:
    - a mapping onto an id the kit lacks is `UnknownKitItemErr`, naming the source label and the id;
    - an add of a held id is `DuplicateItemIdErr`, naming both labels.
    Only then does it answer the added items in order. Each is relabelled `nextFreeAmong` the held labels plus the
    labels already given, and every other field is kept.

### Domain (`Experiments.fs`)

- **`ElementDescriptor.itemIdOpt : ItemId option`** holds the explicitly assigned piece, separate from `binding`.
- **`Experiment.itemsUsedBy experiments items`** works in two steps:
  1. It gathers every `Some` id with its descriptor and experiment. The first id the items lack is
     `UnknownKitItemErr`, naming the id, the element's label and id, and the experiment's `E<n>`.
  2. Otherwise it filters the supplied items to the used ids, in the supplied order, once each.
  No binding, preset or model is consulted.

### Storage

- **`experiment.schema.json`:** `setup` gained an `items` schema. It requires `itemIdOpt` and accepts `null` or the
  shared `library-common` GUID (canonical lower case). A body without the field, or with a non-GUID, is refused as
  `SchemaValidationErr` before binding.
  - `required` and `properties` apply to objects only, so a non-object descriptor still reaches binding. The
    existing `MalformedJsonErr` facts are unchanged.
- **Serializer:** the field rides the one shared serializer (`WithUnwrapOption`, so a value or `null`). There is no
  second parser and no fallback reader.
- **`KitContentFiles.fs`:** `parseExperimentFile`'s doc names the refusal (S7.2); its code is unchanged.

### Ui (`TableAndElementRotationView.fs`)

- `experimentSetup` reads the edited experiment (`draft.editingId`) from the collection.
- Each recaptured descriptor keeps that experiment's `itemIdOpt` for the same `elementId` only when the recaptured
  binding equals the stored one. A new or rebound element carries `None`.

### Tests (all new identities exactly as the effects name them)

- **`KitItemTests`:** the four facts.
  - Fixtures are built through `ItemDraft.addTo` from `BuiltinCatalogue.itemModel` (step 210's shipped entries):
    real LP and circular polarizer as `LibraryModelChoice`, the plate as `StackSampleChoice`.
- **`ExperimentProxyTests`:** the two facts.
  - Laser: `SourcePresetChoice` over HeNe. QZ-A and QZ-B: `StackSampleChoice`, the same model and plate version,
    distinct ids, QZ-B's parameters changed.
  - CPL: a standalone `LibraryModelChoice`. LP1: `PolarizerPresetChoice` over `polarizerLp`.
  - The `desc` helper keeps `itemIdOpt = None` for the deliberately unassigned fixtures; `pieceAt` assigns an
    item's own id.
- **`KitContentFileJsonTests`:** one fact.
  - It round-trips two same-preset descriptors with distinct ids, asserting exact ids and order.
  - It checks the canonical JSON: the GUID string, or `null` for an unassigned descriptor.
  - It refuses as `SchemaValidationErr`: a removed field (an assigned or an unassigned descriptor), a non-GUID, an
    upper-case GUID and a number.
- **`ExperimentCollectionRoundTripTests`:** E1's two same-preset polarizers carry real ItemIds. The existing
  round-trip fact gained the itemIdOpt assertions; no assertion was removed.
- **`InverseConstructorTests`:** the recommit fact.
  1. It adds two `polarizerLp` polarizers to the inverse bench, commits, and writes the copy with three assigned
     pieces (both polarizers and the sample) to the kit.
  2. It reopens the folder and checks the loaded copy, then edits the range, rebinds the sample to a hint, adds a
     third `polarizerLp` polarizer and commits.
  3. It reopens again: both polarizer ids remain distinct, the rebound sample and the new polarizer carry `None`,
     and the range edit persisted.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/Kit.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/Experiments.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/schema/experiment.schema.json`
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/KitContentFiles.fs` (doc only)
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/TableAndElementRotationView.fs`
- Tests:
  - `OpticalConstructor.Tests/KitItemTests.fs`, `ExperimentProxyTests.fs`, `ExperimentCollectionRoundTripTests.fs`;
  - `OpticalConstructor.Tests/KitContentFileJsonTests.fs`;
  - `OpticalConstructor.Ui.Tests/InverseConstructorTests.fs`.
- **Out of the edit list, compile-forced:** `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitLibraryTests.fs`,
  one line, `itemIdOpt = None`.
- Not edited, by design: `ExperimentDataLoad.fs` and `SolverHandoffView.fs` compile unchanged.

## Testing state

These are development runs over the final source. The gate engine gives the verdict (Invariant 6). Logs are in
`.artifacts/0221/`.

- **build** (`dotnet build Berreman.slnx -c Release`):
  - first build: exit 0, 0 errors; the only warnings were the two existing vendored MathNet `SYSLIB0051`;
  - final rebuild after the mutation checks: 0 warnings, 0 errors.
- **constructor-unit-tests** (all discovered): **1319** passed, 0 failed, 0 skipped. That is 1312 + 7: four
  `KitItemTests`, two `ExperimentProxyTests` and one `KitContentFileJsonTests` fact.
  - The TRX shows each of the seven declared identities `Passed`, and the extended E1 round-trip fact too.
  - The final rerun after restoring the mutants gave the same result.
- **ui-tests** (selection: `InverseConstructorTests`, `SolverHandoffWindowTests`, `ExperimentControlsTests`):
  - **51** passed, 0 failed, 0 skipped (29 + 14 + 8);
  - `InverseConstructorTests` went from 13 to 14, and the new identity is `Passed` in the TRX;
  - the final rerun gave the same result.
- **ui-smoke**:
  - **237** passed, 0 failed, 0 skipped, so the count is unchanged;
  - 206 s against the 600 s `attempt_timeout_seconds` (manual 015), measured while the elevation run shared the
    machine.
- **elevation** (`ASG_ARC_BASE_COMMIT` = the arc origin `f395597`, ELEV004 excluded):
  - `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, 204 s;
  - suppressed findings went from 1309 to 1311. They are measured, not gated (manual 002 R7).
- **The fourteen `invariant-s22-*` must-find-nothing greps:** each finds nothing.
- **The new facts bite.** Each mutant was applied and then restored byte for byte (checked with `cmp`) before the
  final rebuild:
  - `experimentSetup` always answering `None` fails the recommit fact ("Expected Some(ItemId 3c9e…), Actual null");
  - `experimentSetup` ignoring the binding check fails it on the rebound sample ("Expected null, Actual
    Some(ItemId 5eb0…)");
  - the schema without `"required": ["itemIdOpt"]` fails the JSON fact. The serializer itself refuses the missing
    field, but as `MalformedJsonErr`, not the required `SchemaValidationErr`.
- Not in this step's roster: `unit-tests` (BerremanTests). No solver code changed.

## Artifacts

All under `specs/0050/.artifacts/0221/`:

- `build-1.log`, `build-final.log`;
- `constructor-unit-tests.log` and `.trx`, `constructor-unit-tests-final.log`;
- `ui-tests.log` and `.trx`, `ui-tests-final.log`;
- `ui-smoke.log` and `.trx`;
- `elevation.log`;
- `mutation/`: the mutant build and test logs, plus the pre-mutation copies used for the restore.

## Gotchas

- **Compile-forced edit outside the edit list.** `OpticalConstructor.Tests/KitLibraryTests.fs` builds a full
  `ElementDescriptor` record literal (`recordingExperiment`). The change_shape contract cannot compile without it,
  so it gained one line, `itemIdOpt = None`; that descriptor is deliberately unassigned.
  - No assertion or identity changed.
  - Step 0210 is the precedent for such edits. The inventory listed only the absent
    `ExperimentCollectionProxyTests.fs` as a further consumer.
- **The binding must be equal, not just the element.** `experimentSetup` keeps a piece only when the recaptured
  `ElementBinding` equals the stored one. Binding a hint to an inverse sample therefore drops its piece, which is
  the "rebound" case.
- **The scene is not in the kit.** The UI fact reopens the folder with the first session's elements, because
  `AddElement` mints ids with `freshId`.
- **The schema's descriptor rule applies to objects only.** A non-object descriptor still reaches binding and stays
  `MalformedJsonErr`, which the existing facts expect.
- **No item-model resolution happens in this step.** `defaultCopyChoice` compares `ItemModelInstance` values, which
  carry the exact `ItemModelVersionId` (step 216). `itemsUsedBy` resolves `Item`s by id. Nothing here resolves a
  model or reads `ItemModelProxy`, so none is manufactured. Fixtures take every version from
  `BuiltinCatalogue.itemModel` (step 210's shipped entries).
- **`itemsUsedBy` refuses on the first dangling id in setup order.** The reason names the id, the element's label and
  id, and the experiment's `E<n>`.
