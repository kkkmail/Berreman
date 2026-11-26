# Step 0235 — impl plan

Obligation `S71-PUBLISH-REVISION`: publishing a revision, as a pure update of one item (001 §I "Revisions"; the S7.1
brief, which S11.5's Publish calibration uses).

## Approach

- **`Kit.fs`.** Add an intrinsic member on `Item`, after `withParameterState`:
  `member this.publishRevision (published : Parameter list) : Result<Item, ItemError>`.
  - Every published parameter is checked before anything is answered:
    - a name the item lacks is `UnknownItemParameterErr` (the same `'<name>' on '<label>'` payload as
      `withParameterState`);
    - a name given twice is `InvalidParameterStateErr` naming it;
    - a state that fails `Parameter.validate` (step 214) is its `InvalidParameterStateErr`, unchanged.
  - Otherwise it answers the item whose `history` is the old history followed by
    `{ revision = this.revision; parameters = this.parameters }`, whose `revision` is `this.revision.next`, and whose
    parameters take each published state in place, the others unchanged.
  - No limit, time stamp or other field is added. Doc comment: one to three lines citing spec 0050 S7.1.
- **`KitItemTests.fs`.** Two facts at the end of the module, over `heldCpl1` (the slice's `ItemDraft` over
  `LibraryModelChoice circularPolarizerModel` and `DialMounted DialPositive`, made by `modelItem`):
  1. `publishing a revision records the values it replaces and counts the revision up`;
  2. `a revision naming a parameter the item lacks, naming one twice or breaking a rule publishes nothing`.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/Kit.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitItemTests.fs`

## Risks

- **Reuse.** The per-parameter rule is `Parameter.validate`; the unknown-name payload must not be a second spelling of
  `withParameterState`'s. Keep one rule set.
- **ELEV013 / ELEV011.** The member lives on `Item`; no wide or repeated-type tuples.
- **Count.** constructor-unit-tests rises from 1356 (checkpoint 233) to 1358.
