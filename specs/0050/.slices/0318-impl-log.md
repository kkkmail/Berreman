# Step 0318 — impl log

## Progress

- [x] Read the protocol, project prompt, slice, AGENTS.md and the code the slice names: step 255/258 (dial rule),
      257 (`inLightOrder`), 263 (`tryItemModelChoice` shape), 269–271 (`readName` / `writeName` / `itemsOf`), 274
      (`SlotChoice`, `SetupGrid.generate`), 286 (`toExperimentName`, `experimentNameAmong`, file-stem identity) and
      317 (`KeyedGroup`, `splitGroups`).
- [x] Domain (`DataSplits.fs`, after `splitGroups`): `DataRowKey.nameText`, `DataSplitError` (+ `describe`),
      `SplitSetupOffer`, `SplitGroupMatch`, `SplitGroup`, `[<ReferenceEquality>] DataSplitParam`, `DataSplit` with
      `matched` / `offered` / `unmatched` and `static member ofGroups`.
- [x] Tests (`DataSplitTests.fs`): test-local copy of step 271's bench, items, `lab` and a `choiceOf`; the four facts.
- [x] Build, constructor-unit-tests, elevation, the 16 s31 invariants: all green (see Testing state).
- [x] Mutation check (three mutations, then the dial one alone); the file was restored byte-identical and the final tree
      rebuilt and re-tested.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/DataSplits.fs` — +250 lines, nothing removed.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/DataSplitTests.fs` — the module doc and opens widened; four
  facts and their fixtures appended. Step 317's two facts are unchanged.

No other product file was touched. The `touches` (`OpticalConstructor.Domain`, `OpticalConstructor.Tests`) and the
authorized `edits` are exactly these two files.

## Decisions

- **`DataSplitParam` is declared before `DataSplit`.** That makes `ofGroups` an intrinsic static member of `DataSplit`,
  not an extension. The slice lists the param fourth; it lists no order constraint.
- **The fold's state is the groups made so far.** The twice-rule, the setups offered earlier (added to `held` for
  `generate`) and the experiment names offered earlier (added to `taken` for `experimentNameAmong`) are all read from
  `earlier`. So there is no second accumulator that could drift from the groups.
- **The twice-rule compares the matched `ExperimentName`.** Both (b) and (c) answer the kit experiment's own name, the
  first in list order, so one experiment always gives one name.
- **"A role the reading names" means the reading's slot parts (`SlotPart.role`).** The slice says "role", and an angle
  carries a position, not a role. In every template the angle's object part is a placeholder of the same optional
  group, so the missing slot is named by its part either way. Example: `Obj 2` for `CPL-(QZ#0-LR#0)-CPL` over a table
  without `sampleB`.
- **Error routing beyond what the slice names, with the same cases:**
  - a `writeName` error after `readName` succeeded is `SplitKeyNameErr`;
  - a blank written name is `SplitKeyTextErr`.

  Neither is reachable: a read name re-writes its required values, and a blank written name implies a blank key, which
  (a) already refuses. They keep the rule total without an extra case.
- **`generate` over one-choice axes answers exactly one setup.** The `Ok []` arm is an unreachable typed refusal
  (`SplitKeySetupErr (UnknownSetupSlotErr …)`), not a throw.
- **The grid is `{ axes; unticked = [] }`, not `SetupGrid.create`.** `create` would untick an impossible combination,
  and `generate` would then answer `Ok []` silently. With the plain grid the table's refusal (e.g. `SlotItemTwiceErr`,
  `IncompatibleSlotItemErr`) reaches the group as `SplitKeySetupErr`.
- **Tests: one extra assertion beyond the slice's list,** inside the authorized fact "an unmatched group the template
  reads is offered…". A third group `CPL-(QZ#90.0)-CPL-2` writes the same experiment name as the first. It is offered
  `CPL-(QZ#90)-CPL-2 (2)`. This pins that offered experiments are `taken`. No test identity was added or renamed.
- **The held-setup case uses another name (`QZ at 90`)** with the same slots. That proves the offer is the held setup
  itself, not a structurally equal new one.

## Testing state

Development runs of each gate's own command over the final tree (the gate engine is the authority):

| Gate | Result | Time | Deadline |
|---|---|---|---|
| build | exit 0, 0 warnings, 0 errors, no lowercase `error` in stdout | 45 s (17 s incremental after the mutation restore) | 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, known findings 0, ELEV004 excluded (suppressed 1434) | 206 s | 600 s |
| constructor-unit-tests | `Passed!`, 1542 of 1542, 0 failed, 0 skipped | 69 s | 180 s |
| invariant-s31-\* (16) | each decoded `git grep` finds nothing | under 1 s each | 300 s |

- The count is 1538 + 4 = 1542, as the slice requires. The TRX (`.artifacts/0318-dev-trx/constructor-unit-tests.trx`)
  lists the four new facts, `Passed`, under their census ids.
- Elevation ran with `ASG_ARC_BASE_COMMIT` set to the `.arc-base-json` origin (`f395597c`). The sources it analyzed are
  byte-identical to the final tree.
- **Red first.** Without the production code the four facts cannot compile: `DataSplit`, `DataSplitParam`,
  `SplitGroupMatch`, its cases and `DataSplitError` do not exist.
- **Mutations.**
  - Run 1 (`.artifacts/0318-dev-mutation.log`) applied three at once: the twice-rule disabled, a dial reading used as
    R1, and the offered names dropped from `taken`. Two of six facts failed, each naming its mutation: the offer fact
    expected `CPL-(QZ#90)-CPL-2 (2)`, and the refusal fact got two `ExperimentForGroup`.
  - Run 2 (`.artifacts/0318-dev-mutation-dial.log`) applied the dial mutation alone, because run 1 masked it behind the
    earlier assertion. The offer fact failed with expected R1 0.5236 rad (30°).
  - Both runs restored the file byte-identical (`cmp`), then rebuilt and re-ran the suite over the final tree.
- No run exceeded its effective deadline.

## Artifacts

- `specs/0050/.artifacts/0318-build.log`
- `specs/0050/.artifacts/0318-constructor-unit-tests.log`
- `specs/0050/.artifacts/0318-dev-trx/constructor-unit-tests.trx`
- `specs/0050/.artifacts/0318-elevation.log`
- `specs/0050/.artifacts/0318-dev-mutation.log`, `specs/0050/.artifacts/0318-dev-mutation-dial.log`

## Gotchas

- A recursive `grep -rn` over `OpticalConstructor.Tests/` without a file filter walks `bin/` / `obj/` and hangs past the
  tool timeout. Use the Grep tool or `--include=*.fs`.
- A quoted heredoc that contains many `'` characters failed to parse in the Bash tool's wrapper. Appending through the
  Edit tool avoided it.
- The record labels `key`, `points`, `setup`, `items` and `table` are shared with `KeyedGroup`, `Experiment`,
  `ExperimentTarget` and `GridSlotDraft`, so every construction in `DataSplits.fs` and the tests is annotated.
