# Step 0215 — impl plan

## Approach

Append three types to the `Kit` module in `OpticalConstructor.Domain/Kit.fs`, after `Parameter` (step 214), in the
brief's order. Each type gets a doc comment of one to three lines citing spec 0050 S7.1.

1. `ItemDial`, with two cases:
   - `DialMounted of DialSign`;
   - `FixedMounting`.

   Its `parameterAxes` member answers the one `dialZero` axis (`AngleParameter`, default 0) for a dial-mounted item
   and `[]` for a fixed one.
2. `ItemRevisionRecord = { revision : ItemRevision; parameters : Parameter list }`.
3. `Item = { id; label; model; parameters; dial; photoOpt; notes; revision; history }`, with two members:
   - `libraryReferences`: the model's `entryReferenceOpt`, then its `bindingReferenceOpt`, with `List.choose id`
     and `List.distinct`;
   - `tryFindParameter`: `List.tryFind` by name.

No other member, type or `open` is added. `ItemModels`, `LibraryEntryFiles` and the earlier `Kit` types are already
in scope.

Append the two declared facts to `OpticalConstructor.Tests/KitItemTests.fs`. They work over test-local items written
as records:

- the circular-polarizer v1 standalone item;
- the source, ideal-polarizer and stack items over the shipped built-in model versions, as in step 213's fact;
- the dial-axis fact.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/Kit.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitItemTests.fs` (already in the test project)

## Risks

- **New record labels on `Item`** (`id`, `label`, `model`, `parameters`, `notes`, `revision`) can shift F# record
  inference in later files that open `Kit`. The build shows it, and any fix belongs in the step's two files where
  possible.
- **Unique names (ELEV014 / 016).** `Item`, `ItemDial`, `ItemRevisionRecord`, `DialMounted` and `FixedMounting` are
  found nowhere in Berreman or STL, so no rename is needed.
