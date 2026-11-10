# Step 0235 — impl log

## Progress

- **Reading.** Read: the system prompts (base and `IMPLEMENT`), the project prompt, AGENTS.md, the slice and its
  gates snapshot, and step 0234's impl-log, plan and state of the world. No operator note is in flight, and no
  `.manual/` ruling names this step. Nothing in `Berreman/` names `publishRevision` yet. Baseline:
  constructor-unit-tests 1356 (checkpoint 233).
- **Kit.fs.** Added an intrinsic member on `Item`, after `withParameterState`:
  `member this.publishRevision (published : Parameter list) : Result<Item, ItemError>`.
  - A private recursive `publishFrom` walks the published list in order over the working item, carrying the names
    already taken:
    - a name already taken is `InvalidParameterStateErr "<name>: published twice in one revision of '<label>'"`;
    - every other parameter goes through `withParameterState` (step 234), which is where the unknown-name refusal
      (`UnknownItemParameterErr "'<name>' on '<label>'"`) and `Parameter.validate` (step 214) already live. So there
      is one rule set and one payload spelling, not a copy.
  - Only when the whole list passes does it answer: the replaced item with `revision = this.revision.next` and
    `history = this.history @ [ { revision = this.revision; parameters = this.parameters } ]`.
  - A refusal anywhere in the list answers that `Error` and no item. Nothing is written; no limit, time stamp or
    other field is added. The two-line doc cites spec 0050 S7.1 and 001 §I.
- **KitItemTests.fs.** A new section, `spec 0050 S7.1: publishing a revision`, at the end of the module, reusing the
  S7.2 fixtures `modelItem`, `circularPolarizerModel`, `heldCpl1` (the slice's `ItemDraft` over
  `LibraryModelChoice circularPolarizerModel` and `DialMounted DialPositive`) and step 234's `retardance`. It adds two
  private fixtures, `fastAxisOffset` and `calibratedRetardance sigma` (value 1.55, fit run
  `QZ-LR@20261101T120000Z`), and the two declared facts:
  1. `publishing a revision records the values it replaces and counts the revision up`.
     - Revision 1's five default parameters are written out and equal `heldCpl1.parameters`.
     - Publishing the calibrated retardance (sigma 0.002) answers exactly `{ heldCpl1 with parameters = …;
       revision = 2; history = [ revision 1 record ] }`, the calibration in the retardance's place.
     - Publishing a known `c2.fastAxisOffset` 0.01 on that answer gives revision 3, whose history is the revision 1
       and revision 2 records in that order, with both published states held.
  2. `a revision naming a parameter the item lacks, naming one twice or breaking a rule publishes nothing`.
     - Each refused list leads with a valid offset, so the refusal is of the whole list.
     - `nothing` answers `UnknownItemParameterErr` naming `'nothing'` and `'CPL1'`, equal to `withParameterState`'s.
     - `c2.retardance` twice answers `InvalidParameterStateErr` starting `c2.retardance: `, naming `twice` and `'CPL1'`.
     - A calibration with sigma 0 answers `InvalidParameterStateErr "c2.retardance: the calibration sigma 0 is not
       above zero"`, equal to `Parameter.validate`'s.
     - CPL1 keeps revision 1, an empty history, and is still the item its draft makes.
- **Red before green.** Both facts call `Item.publishRevision`, which did not exist before this step; the red is the
  missing symbol (the base protocol's `NameError` / `undefined` case).

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/Kit.fs` (+26 lines)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitItemTests.fs` (+107 lines)

These are exactly the slice's two authorized edits. No project file changed, and the test file was already in
`OpticalConstructor.Tests.fsproj`.

## Testing state

These are development runs over the final source. The gate engine gives the verdict (Invariant 6).

- **build:** `dotnet build Berreman.slnx -c Release -nologo -v:m` exit 0, 0 errors, 71 s. The only warnings are the
  vendored MathNet's two `SYSLIB0051`, the same as step 0234. Nothing comes from our code.
- **constructor-unit-tests**, whole suite: 1358 passed, 0 failed, 0 skipped, in 68 s. That is checkpoint 233's 1356
  plus the two declared facts.
- **KitItemTests alone:** 21 passed, 19 + 2. The TRX names both declared ids exactly, each `Passed`.
- **elevation**, with `ASG_ARC_BASE_COMMIT` set to the arc origin `f395597`: `ELEV-GATE: OK - 0 new finding(s) in
  12 project(s)`, in 189 s. The run widened to all 12 projects, because the solution changed since the origin.
- **The fourteen s23 invariant greps:** none finds anything.
- **Line endings:** both edited files are LF, `i/lf w/lf`, with no CR. `git diff --numstat` equals the
  `--ignore-cr-at-eol` numstat.

No test is removed or renamed, and no assertion is removed or weakened.

## Artifacts

All under `specs/0050/.artifacts/`:

- `0235-dev-build.log`
- `0235-dev-constructor-unit-tests.log`, with its TRX under `0235-constructor-unit-tests/`
- `0235-dev-kititemtests.log`, with its TRX under `0235-kititemtests/`
- `0235-dev-elevation.log`
- `0235-dev-invariants.log`

## Gotchas

- **Reuse over a second rule set.** The slice lists three refusals. Two of them, the unknown name and the
  `validate` failure, are exactly what `withParameterState` (step 234) already does, so `publishRevision` folds
  each entry through it rather than re-checking. The new fact pins that the unknown-name error equals
  `withParameterState`'s and the sigma-0 error equals `Parameter.validate`'s.
- **The repeated-name payload.** The slice asks only that it name the parameter. I chose
  `<name>: published twice in one revision of '<label>'`, which follows `validate`'s `<name>: <rule>` form.
- **Check order is list order.** For each entry, a repeat is checked first, then the unknown name, then the state.
  So `[ nothing; nothing ]` answers `UnknownItemParameterErr`, because the first `nothing` is refused before any
  repeat is seen. The slice's cases each break one rule, so this order is not pinned beyond them.
- **An empty list publishes** the next revision with unchanged parameters. The slice names no refusal for it, so I
  added none.
- **"publishing `c2.fastAxisOffset`" names no state.** The fact publishes `KnownParameter 0.01`, which also pins the
  slice's "any state may be published" beside revision 2's `CalibratedParameter`.
