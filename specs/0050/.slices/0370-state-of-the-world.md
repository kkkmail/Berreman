# Step 0370 — state of the world

## Where we are

Spec 0050 section S11.2 (the forward model of an experiment and the study residual). Step 369 moved an experiment's
pre-fit checks into `OpticalConstructor.Domain.PrefitChecks`; this step adds the pre-fit "data changed since attach /
since the last fit run" flag beside them (001 §E), over S4.2's attachment status (steps 132, 134) and the attachments a
fit run read (S11.3 records those).

## What's working

- Add `PrefitChecks.DataChangeFlag`: unchanged, changed since attach, changed since the last fit run, or missing, each with its `text`
- Add `MappedAttachment.changeWith`, flagging a missing file, a change since attach and a change since the last fit run, in that precedence
- Add the `PrefitCheckTests` fact covering every status, the `fitted` cases and the four texts

## Tests

```yaml
gates:
  constructor_unit_tests: 1620
```

- build: exit 0, no warning from our code, 75 s (deadline 600 s).
- constructor-unit-tests: Passed!, 1620 succeeded (1619 at checkpoint 369, +1), 71 s (deadline 180 s).
- elevation from arc base `f395597c`: OK, 0 new findings in 13 projects, 212 s (deadline 600 s).
- The twenty s37 invariants: each `section_invariants --find-nothing` exits 0.
- Nothing deferred. All durations are within their deadlines.

## Architecture

- The flag is derived, never stored, so `DataChangeFlag` has no wire tag, only a `text` render.
- `changeWith` is a pure member over the S4.2 status the caller already read (`KitProxy.attachmentStatus`) and the last fit
  run's `DataAttachment list`; it reads nothing itself, reusing S4.2's check rather than re-hashing.

## Deferred

Nothing.

## Gotchas

- `ChangedSinceFitFlag` carries the attachment's own hash (the current one, since the status says it is unchanged since
  attach), not the hash the fit run read.
- Only a same-path, different-hash entry in `fitted` raises the since-fit flag; an empty `fitted` (before the first fit
  run) is never flagged.

## Changelog

- 2026-11-13 — Step 0370: `DataChangeFlag` and `MappedAttachment.changeWith` in `PrefitChecks.fs`, with one
  `PrefitCheckTests` fact (constructor suite 1619 → 1620).
