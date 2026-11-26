# Step 0235 — state of the world

## Where we are

Spec 0050, section S7.1 ("Items, parameters and item models"), step 0235 (IMPLEMENT), obligation
`S71-PUBLISH-REVISION`.

- Steps 214–216 built an item's parameters and their validation, the `Item` record and `AddItem`'s `ItemDraft`.
- Step 234 added `Item.withParameterState`, which edits the working revision.
- This step publishes a revision as a pure update of one item (001 §I "Revisions"). S11.5's Publish calibration
  will call it with `CalibratedParameter` states.

## What's working

- Added `Item.publishRevision published`. It records the current revision's values in `history`, counts `revision`
  up and puts each published state in place of its parameter. The other parameters stay unchanged.
- A revision that names a parameter the item lacks, names one twice or breaks a rule is refused whole, with
  `UnknownItemParameterErr` or `InvalidParameterStateErr`. Nothing is published.
- Added two `KitItemTests` facts over CPL1 of the circular polarizer model:
  - revisions 1 → 2 → 3, with the history oldest first;
  - the three refusals.

## Tests

No gate verdict is claimed here (Invariant 6). After exit, the arc-runner runs the frozen roster: build, elevation,
constructor-unit-tests over everything discovered, and the fourteen s23 invariants.

The worker's development runs over the final tree are in the impl-log's `Testing state`, with logs under
`specs/0050/.artifacts/0235-*`:

- **build:** 0 errors. The only warnings are the vendored MathNet's two `SYSLIB0051`, the same as step 0234.
- **constructor-unit-tests:** 1358 passed, 0 failed, 0 skipped. That is checkpoint 233's 1356 plus the 2 declared
  facts.
- **KitItemTests:** 21, up from 19. The TRX holds the two declared ids, each `Passed`.
- **elevation** from the arc origin `f395597`: `ELEV-GATE: OK`, 0 new findings in 12 of 12 projects, in 189 s.
- **The fourteen s23 invariant greps:** none finds anything.

No run came near a deadline. No test is removed or renamed, and no assertion is removed or weakened.

```yaml
gates:
  constructor_unit_tests: 1358
```

## Architecture

- **A pure member on the record it updates.** `publishRevision` is an intrinsic member of `Item`, beside
  `withParameterState`, so it is no free function of one of our types (`ELEV013`). It writes nothing. Whatever
  records the item (S11.5's Publish calibration) persists the answer.
- **One rule set, reused, not copied.** Each published parameter goes through `withParameterState`, so the
  unknown-name refusal and `Parameter.validate` (step 214) are the same code, with the same payload, as a single edit.
  The only new rule is the repeated name.
- **All or nothing.** The list is walked in order over a working copy, and nothing is answered until every entry
  passes. A refusal anywhere answers that error, so a valid entry before it is not published either.
- **The history holds values only.** A record is `{ revision; parameters }` of the revision being replaced. No limit,
  time stamp or fit-run vector is added: each fit run keeps its own parameter vector (001 §I).

## Deferred

Nothing in this step's scope is deferred. `Item.publishRevision` and its two facts are complete.

## Gotchas

- **The repeated-name payload is `<name>: published twice in one revision of '<label>'`.** It follows
  `Parameter.validate`'s `<name>: <rule>` form, so every `InvalidParameterStateErr` starts with the parameter's
  name.
- **Refusals come in list order.** Each entry is checked for a repeat first, then by `withParameterState` (unknown
  name, then validation). So `[ nothing; nothing ]` is `UnknownItemParameterErr`, because the first `nothing` is
  refused before any repeat is seen.
- **An empty list publishes.** `publishRevision []` answers the next revision with the parameters unchanged. The
  slice refuses only unknown, repeated and invalid entries, so the step adds no "nothing to publish" rule.
- **The facts sit at the end of `KitItemTests`.** They reuse the S7.2 fixtures (`modelItem`,
  `circularPolarizerModel`, `heldCpl1`) and step 234's `retardance`, and module-level `let`s must precede their use.

## Changelog

- 2026-11-09 — Step 0235:
  - added `Item.publishRevision`, a pure, all-or-nothing publish of one item's next revision with its value history;
  - added two `KitItemTests` facts (constructor-unit-tests 1356 → 1358).
