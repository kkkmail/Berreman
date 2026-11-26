# Step 0277 — impl log

## Progress

- [x] Read the protocol, the project prompt, the slice, `Setups.fs`, `NamingTemplates.fs`, `SetupTests.fs` and
  `SetupGridTests.fs` (step 274's `choiceOf` / Ghost pattern).
- [x] Added `PickLetter` and the three `SetupSlot` members in `Setups.fs`, after step 261's members.
- [x] `NamingTemplate.nameReadingOf` now calls `slot.turnAngle`. Its local `angleIn` is deleted.
- [x] Added the three `SetupTests` facts.
- [x] Ran build, constructor-unit-tests (whole), elevation and the sixteen s28 invariants locally on the final tree.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/Setups.fs` (+53), at the end of the module:
  - `type PickLetter = PickLetter of char`, with `.value` and
    `static member tryCreate (text : string) : PickLetter option`. It takes the first character (`Seq.tryHead`). When
    `Char.IsLetterOrDigit` holds, it answers that character upper-cased in the invariant culture; otherwise `None`.
  - `type SetupSlot with`:
    - `turnAngle : Angle` — a `DialTurn reading` slot answers its reading. A `PlacementTurn` slot or an empty one
      answers `placement.r1`.
    - `fittingItems (items : Item list) (choiceOf : Item -> ItemModelChoice option) : Item list` — the items, in
      order, whose `choiceOf` answers a choice that `this.withItem item choice` accepts (`List.choose` with
      `Result.toOption`).
    - `pickedByLetter (letter : PickLetter) (fitting : Item list) : Item option` — see the Decisions section.
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/NamingTemplates.fs` (+3 / −9):
  - The local `angleIn` in `nameReadingOf` is deleted. The `AnglePlaceholder` arm calls `slot.turnAngle` under its
    unchanged condition (`tryFindHeld (ObjectSlotRole position)`, i.e. Obj k holds an item the kit has).
  - The doc now says "an object's `turnAngle` (S8.4; 001 §I)" and is re-wrapped to three lines.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/SetupTests.fs` (+90), a new "S8.4: the slot bar's chips"
  section at the end:
  - New private fixtures:
    - `circularPolarizerChoice`, the `LibraryModelChoice` of
      `BuiltinCatalogue.itemModel ItemModelIds.circularPolarizerModelId`;
    - `fixedLp2`, LP2 over the LP preset, `FixedMounting`;
    - `cpl1`, CPL1;
    - `plateOnDial`, QZd: the glass plate's `StackSampleChoice`, `DialMounted DialNegative`, dial zero 10°;
    - `ghost`, Ghost over the LP preset;
    - a test-local `choiceOf` that answers `None` for Ghost.
  - The three facts named in the slice.

## Decisions

- **Existing fixtures are reused, not copied.** The file already has private items that are exactly what the slice
  names: `laser` (Laser, `heNeChoice`), `fixedLp` (LP1, the `polarizerLp` preset) and `plate` (QZ,
  `StackSampleChoice` of the glass plate at its first version). The tests use them.
  - LP2 is new (`fixedLp2`). The file's existing `dialLp` is labelled LP2 but is dial-mounted, and the slice wants
    every item fixed unless named.
  - Every choice comes from step 216's payloads and step 210's shipped item models (`BuiltinCatalogue.itemModel`), as
    the file's step-258 fixtures already do.
- **`fittingItems` reuses `withItem`.** It does not repeat `slotKinds |> List.contains kind`. The slice describes it as
  "the rule `withItem` checks, applied before the choice, so a chip lists exactly what `SwapSlot` accepts". Calling
  `withItem` makes that true by construction, with no second copy of the rule. Today `withItem` refuses only on
  `slotKinds`, so the result is exactly the slice's definition. Using `List.choose` with `Result.toOption` also avoids
  a function that maps a `Result` to `bool` (the ELEV012 shape).
- **`pickedByLetter` matches labels through `PickLetter.tryCreate item.label.value = Some letter`.** The upper-casing
  rule therefore lives in one place. A label starting with a non-letter-or-digit gives `None`, and since a
  `PickLetter` is always a letter or a digit, that label can never match. This is the same as "the label's first
  character, upper-cased in the invariant culture, is the letter".
  - The match goes through `List.tryFindIndex` of the held id among the starting items, then `List.skip (at + 1)`.
    That gives the next starting item, or wraps to the first.
  - When the slot's held item is not among them, the pick is the first starting item.
  - A pick equal to the held id answers `None`.
- **`PickLetter` uses `System.Char`, fully qualified.** `Setups.fs` had no `open System`, and opening it for two calls
  could shadow names across the module.
- **Ghost is made over the LP preset.** Resolved, it would fit `lpA`. It sits between CPL1 and LP2 in the fact's item
  list, so the fact shows that an item `choiceOf` cannot resolve is left out, and not merely an incompatible one.
- **The chip-picking fact covers two cases beyond the slice's list.** `x` is checked while holding LP1 and while
  holding nothing. The angle fact also asserts that QZd's drawn R1 stays 30°, which shows `turnAngle` is the reading
  and not R1. Both are extensions inside the authorized test identities; nothing is removed.

## Testing state

The production code was written first. The three facts reference `PickLetter`, `fittingItems`, `pickedByLetter` and
`turnAngle`, so before this step they did not compile, which counts as red under the protocol's "new code" rule. They
pass now.

- build (`dotnet build Berreman.slnx -c Release -nologo -v:m`): pass, 74 s (deadline 600 s). There were 0 errors. The
  only warnings are the two pre-existing vendored MathNet `SYSLIB0051`.
- constructor-unit-tests (whole suite, the gate's own command): `Test run summary: Passed!`, 1460 of 1460, in 72 s
  (deadline 180 s). The baseline was 1457 (step 276), so the count rose by 3. In the TRX the three new facts are
  `Passed`, and `NamingTemplateTests` passed all 14 unchanged.
- elevation (`Elevation/Run-ElevationGate.ps1 -ExcludeRules ELEV004`, with no base, so all 12 projects):
  `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, 197 s (deadline 600 s); known findings 0.
- invariant-s28-* (all sixteen): each grep finds nothing
  (`specs/0050/.artifacts/0277-invariants.log`).

## Artifacts

- `specs/0050/.artifacts/0277-build.log`
- `specs/0050/.artifacts/0277-constructor-unit-tests.log`
- `specs/0050/.artifacts/0277-elevation.log`
- `specs/0050/.artifacts/0277-invariants.log`
