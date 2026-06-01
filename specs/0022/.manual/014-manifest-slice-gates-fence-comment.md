# 014 — manifest slice_gates fence must start with `slice_gates:`

## The issue

The spec-loop ran to a clean `done-green` at round 5 (16 session-sized
slices, ACCEPTABLE verdict, 0 BLOCKER / 0 MAJOR, all 27 facts confirmed),
but the splitter still exited **code 4** in its POST-SPLIT step.

Log line:

```
POST-SPLIT: manifest slice_gates block malformed: expected
'slice_gates:' at top of slice_gates block; got
'# Gate source-of-truth (reconciler round-3 §g): this manifest `slice_gates:` block'
```

Root cause: the final splitter/reconciler wrote a 15-line `#` comment
block **inside** the ```` ```yaml ```` fence, *before* the `slice_gates:`
key. The POST-SPLIT validator (`splitter.py`) requires `slice_gates:` to
be the **first line inside the fence** — it does not tolerate leading
comment lines. So the slicing was good; only the manifest framing was
malformed.

(Two earlier exit-4s were unrelated: a round-3 split sub-agent
hallucinated an unrelated spec and never wrote its output — recovered by
committing the round-2 state.)

## How to fix it

Edit `C:\GitHub\Berreman\specs\0022\.manifest`: remove every comment
line between the opening ```` ```yaml ```` fence and `slice_gates:`, so
the fence opens directly onto the key:

```yaml
slice_gates:
  - slice: 001
    gates: [build, unit-tests, constructor-unit-tests]
  ...
```

Leave the gate entries and the `.slices/NNN.slice-md` path list
unchanged. Then re-run the splitter; it resumes from the round-5
reconcile decision and emits `DONE-GREEN` immediately (no worker
re-spawn).

## What I did

1. Stripped the leading comment block from the `slice_gates:` fence in
   `.manifest` (gate data and slice list untouched).
2. Re-launched the splitter with the same arguments. It resumed from
   `023-05-01-reconcile.md`, logged `DONE-GREEN — reconciler signalled
   completion`, and exited **0**. The slicing is ready for the
   arc-runner.

## Prevention

The splitter prompt/POST-SPLIT contract should either (a) instruct the
splitter never to emit comment lines ahead of `slice_gates:` inside the
fence, or (b) make the validator skip leading `#` comment lines before
the key. Until then, any explanatory prose belongs outside the fenced
block.
