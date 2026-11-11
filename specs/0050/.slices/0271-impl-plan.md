# Step 0271 — impl plan

## Approach

A setup's name is written from its slots and read back into its items and angles (spec 0050 S8.3; 001 §J). Everything
lands in `NamingTemplates.fs` after step 269's declarations, and in `NamingTemplateTests`.

1. `OpticalConstructor.Domain/NamingTemplates.fs`, appended:
   - `type ItemLabel with member this.namePart : NamePart`, a label as a name part;
   - `type NameText with member this.toSetupName : Result<SetupName, SetupError>`, `SetupName.tryCreate` of its text.
     No member converts back (ELEV006);
   - `SlotItem = { role : SlotRole; item : ItemId }`;
   - `type NamingTemplate with`:
     - `nameReadingOf slots items noteOpt` lays the slots out by `SetupSlot.inLightOrder`. For each placeholder of
       `pieces` in order, it takes:
       - a slot's part: the first alias whose label matches the item's ignoring case, else `label.namePart`;
       - Obj k's angle: a `DialTurn` reading, else the placement's R1;
       - the note.

       A missing role, an empty slot, or an item that `items` lacks gives no value.
     - `itemsOf reading items` names, for each part, the item it stands for. An alias of that placeholder whose part
       matches (ordinal) names its label's item. Otherwise the item whose label equals the part, ignoring case. A miss
       answers `UnknownNameItemErr` naming the part and the placeholder's `text`.
     - `setupNameOf slots items` is `writeName` of `nameReadingOf … None`, then `toSetupName`. A blank name answers
       `MissingNameValueErr` naming the template.
     - `setupNameAmong slots items taken unnamed` is `setupNameOf`'s name made free by `SetupName.nextFreeAmong`, or
       `unnamed` when the template cannot name the slots.
2. `OpticalConstructor.Tests/NamingTemplateTests.fs` gets the slice's fixtures:
   - the slots `src`, `lpA`, `sampleA`, `sampleB`, `lpB` and `det`, made as `SetupTests` makes them;
   - the items `Laser`, `CPL1`, `CPL2`, `QZ`, `LR` and `QZd`, made through `ItemDraft.addTo` and put in their slots
     by `withItem` after R1 is set;
   - the template `lab`.

   It adds the four census facts.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/NamingTemplates.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/NamingTemplateTests.fs`

## Risks

- ELEV rules on the new members. Members may speak their own type's primitives, and `NamePart this.value` on the
  source type is the sanctioned cross-type form (cf. `ElementId.toTreeLabel`).
- Record inference for `{ role; item }` beside `SlotPart` / `RoledSlot`. F# takes the intersection of the field
  labels' record types, so it is unambiguous.
- Fact 3: "the slots' items by role" must be read against the roles the template names. The light slot holds `Laser`,
  but `lab` has no `{light}` placeholder, so the comparison is restricted to the template's slot placeholders.
