# Step 0274 — impl log

## Progress

- [x] Kit.fs: `SetupError.SlotItemTwiceErr of reason : string`, rendered `the item is in two slots: <reason>`
- [x] SetupGrids.fs (new) + one `<Compile>` line after `NamingTemplates.fs` in the Domain fsproj, no comment
- [x] SetupGridTests.fs (new) + one `<Compile>` line appended at the end of the Tests fsproj compile list, no comment
- [x] KitTests: `every setup error renders its condition and its payload` covers `SlotItemTwiceErr` (8 distinct)
- [x] Local build / constructor-unit-tests / elevation / the fifteen s27 invariants

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/Kit.fs` — new `SetupError` case and its `describe` arm.
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/SetupGrids.fs` — new: `SlotChoice`, `SlotOptions`, `SlotPick`,
  `GridCombination` (+ `slotsOver`), `SetupGrid` (+ `combinations`, `ticked`, `untick`, `tick`, `create`, `generate`).
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/OpticalConstructor.Domain.fsproj` — `SetupGrids.fs` compile line.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/SetupGridTests.fs` — new, `module SetupGridTests`, four facts.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj` — `SetupGridTests.fs` compile line.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitTests.fs` — the setup-error render fact (one case, one match arm, 7 → 8).

## Decisions

- `slotsOver` folds the picks in order over the table: a pick's slot must be in `table` (`UnknownSetupSlotErr '<id>'`),
  then it is turned (`{ placement with r1 = choice.r1 }`, the step's "set", not the lock-respecting `withR1`), then
  emptied (`withoutItem`) or filled: item from `items` (`UnknownSlotItemErr "the kit holds no item <id>"`), not already
  in another picked slot (`SlotItemTwiceErr "'<label>' in '<first>' and '<second>'"`), choice from `choiceOf`
  (`UnknownSlotItemErr "'<label>': its model is not in the library"`), then `withItem` (`IncompatibleSlotItemErr`).
  The texts are `resolveSlotItem`'s / `slotAssigned`'s, so AssignSlot and the grid refuse alike. Every table slot is
  then `recordedWith items`.
- `combinations` is a right-fold product (`[ [] ]` for no axis), so the first axis varies slowest. `ticked` filters
  `unticked` out; `untick` appends once; `tick` removes. `create` unticks, in order, every combination whose
  `slotsOver` is an `Error` (a `List.choose`, no bool-from-union helper).
- `generate` folds the ticked combinations with `Result.bind`, so the first failing combination refuses the whole grid
  and nothing half-made is answered. A combination whose `slotsOver` equals a setup's current `slots` in `held @ earlier`
  (held first) answers that setup; otherwise `Setup.create (template.setupNameAmong slots items taken
  (SetupName.firstFree taken)) slots known`, `taken` the names of held and earlier-made setups. Nothing is written.
- No cache, sort, size limit or extra member was added.

## Testing state

All on the final tree, from the repo's own commands (captures under `specs/0050/.artifacts/0274-local/`):

- build: `dotnet build Berreman.slnx -c Release -nologo -v:m` exit 0, no `error` in the output, no warning from our code
  (the first, fuller build showed the vendored MathNetNumerics' pre-existing `SYSLIB0051` warnings, as `b11-build.log`
  does; the incremental rebuilds showed none). 76 s first, 22 s final; deadline 600 s.
- constructor-unit-tests (whole suite): `Test run summary: Passed!`, total 1452, succeeded 1452, failed 0, skipped 0;
  68 s against a 180 s deadline. Baseline (checkpoint 272) 1448 → 1452, +4 as the step requires. The TRX shows the four
  new SetupGridTests facts and the changed KitTests fact `Passed`.
- elevation (`ASG_ARC_BASE_COMMIT=f395597…`, the arc origin): `ELEV-GATE: OK - 0 new finding(s) in 12 project(s), 202s`;
  deadline 600 s. Only the Tests file changed after that run, and test projects are outside the analyzers' scope.
- invariant-s27-* (all fifteen, through the ASG evaluator): `INVARIANT HOLDS` each.

Red: the new facts were written with the production symbols and could not compile without them. The first full run
had one red, `a ticked combination the grid cannot make refuses the whole grid`, on an assertion I had added beyond the
spec: the grid with the Laser's combination unticked has no `lpB` axis, so `lpB` is empty and `lab` (whose
`{analyzer}` is required) names no setup. The product was right by the spec (`Setup 1`); the expectation was
corrected to `Setup 1`, with a comment saying why.

## Artifacts

- `specs/0050/.artifacts/0274-local/build.log`, `build-2.log`, `build-3.log`
- `specs/0050/.artifacts/0274-local/constructor-unit-tests.log` (the one red), `-2.log`, `-3.log` (final)
- `specs/0050/.artifacts/0274-local/elevation.log`

## Gotchas

- `SlotItemTwiceErr` is checked among the PICKED slots only, as the step says ("one item in two picked slots"). The
  table's `AssignSlot` does not refuse an item held in a second slot either, so an unpicked slot already holding the
  picked item is not refused here; the grid keeps the table's rule rather than inventing a stricter one.
- A slot named by two axes takes the later pick (the later pick replaces the earlier in the fold). The step names no
  error for it and adds no case, so none was invented.
- The item resolution texts duplicate `TableAndElementRotationModel.resolveSlotItem` (Ui) by design of the step: the
  Ui file is outside this step's authorized effects, and the Domain generator resolves through the caller's
  `choiceOf`, which the host supplies as `model.tryItemModelChoice` when it wires `GenerateSetupGrid` (step 276).
- Two records share the field `slot` (`SlotOptions`, `SlotPick`); every construction is annotated (`let pick : SlotPick`)
  so inference never picks the wrong record.
- `grep -c $'\r'` under Git Bash reports line counts, not CRs; `tr -cd '\r' | wc -c` shows 0 CR bytes in every
  touched file (LF), and `git ls-files --eol` shows `i/lf w/lf`.
