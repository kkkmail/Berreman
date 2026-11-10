# Step 0234 — impl plan

Obligation `S71-SET-PARAMETER-STATE`: setting one parameter's state as a pure update of one item (001 §P, §I).

## Approach

- **`Kit.fs`.** Add an intrinsic member on `Item`, after `tryFindParameter`:
  `member this.withParameterState (name : ParameterName) (state : ParameterState) : Result<Item, ItemError>`.
  - No parameter of that name: `UnknownItemParameterErr`, naming the item's label and the name.
  - Otherwise, the result of `{ name = name; state = state }.validate` (step 214). A broken state answers its
    `InvalidParameterStateErr`. A valid state answers the item with that one parameter replaced in place
    (`List.map` over the list).
  - The order, every other parameter, the revision and the history stay as they are.
  - The doc comment is one to three lines citing spec 0050 S7.1. It says the change edits the working revision, and
    only a published revision moves it (step 235).
- **`KitItemTests.fs`.** Three facts, over CPL1 and CPL2 made by an `ItemDraft` over
  `LibraryModelChoice (BuiltinCatalogue.itemModel ItemModelIds.circularPolarizerModelId)` and
  `DialMounted DialPositive`:
  1. `setting a parameter's state replaces it in place and keeps the revision`;
  2. `setting the state of a parameter the item lacks, or one that breaks its rules, is refused`;
  3. `two items of one model keep independent parameters`.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/Kit.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitItemTests.fs`

## Risks

- **ELEV013.** The member lives on `Item`, so it is not a free function of one of our types.
- **ELEV001 / ELEV005.** Test fixtures take their values at origin, as typed `let`s. There is no `(T x.value)` round
  trip.
- **Count.** constructor-unit-tests rises from 1353 (checkpoint 232) to 1356.
