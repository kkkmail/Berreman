# Step 0277 — impl plan

## Approach

The slot bar's pure halves (spec 0050 S8.4; 001 §J "Quick swap"), all in `OpticalConstructor.Domain/Setups.fs`, after
step 261's members (`pinFor`, `slotsIn`, `itemsUsedBy`):

1. `PickLetter of char` with `.value` and `static member tryCreate (text : string) : PickLetter option` — the text's
   first character upper-cased in the invariant culture when `Char.IsLetterOrDigit` holds for it, else `None`.
2. `type SetupSlot with`:
   - `turnAngle : Angle` — a `DialTurn reading` slot's reading, else the placement's R1. The rule moves here from the
     local `angleIn` in `NamingTemplate.nameReadingOf`, whose `AnglePlaceholder` arm now calls `slot.turnAngle` and
     keeps its condition (Obj k holds an item).
   - `fittingItems items choiceOf` — the items, in order, whose resolved choice `withItem` accepts. Reusing
     `withItem` itself (rather than re-transcribing `slotKinds |> List.contains kind`) keeps the chip's list exactly
     what `SwapSlot` accepts; an item `choiceOf` cannot resolve is left out.
   - `pickedByLetter letter fitting` — the fitting items whose label's first character, upper-cased, is the letter
     (matched through `PickLetter.tryCreate` of the label, so the upper-casing rule lives once); the one after the held
     item (wrapping) when the slot holds one of them, else the first; `None` when none starts with it or the pick is
     the held item.

## Tests

Three facts at the end of `SetupTests`, over its `src`, `lpA`, `sampleA`, `det` slots. Existing file-private items
are reused where they are exactly what the slice names (`laser` = Laser, `fixedLp` = LP1, `plate` = QZ). New private
items: a FIXED `LP2` (the file's `dialLp` is LP2 on a dial, which the slice does not want here), `CPL1`
(`LibraryModelChoice` of `BuiltinCatalogue.itemModel ItemModelIds.circularPolarizerModelId`), `QZd` (the glass plate,
`DialMounted DialNegative`, dial zero 10°) and `Ghost`, with a test-local `choiceOf` answering `None` for Ghost.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/Setups.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/NamingTemplates.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/SetupTests.fs`

## Risks

- ELEV: `PickLetter` is a new type and case name — checked unique (no `PickLetter`, `turnAngle`, `fittingItems`,
  `pickedByLetter` anywhere in `Berreman/`). Members may speak their own primitives (`char`, `string`). No `.value`
  is needed in production code (the letter is compared as a `PickLetter`), and no bool-returning helper is added.
- `NamingTemplateTests` must pass unchanged: `turnAngle` is the same match `angleIn` was.
