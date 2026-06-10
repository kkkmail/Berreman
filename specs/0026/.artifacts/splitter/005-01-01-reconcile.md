# Reconciler decision -- round 01 iter 01

**Decision: `route-to-splitter` (next round: split round 2).**

## What I weighed

The verifier returned **ACCEPTABLE** with severity counts BLOCKER 0 / MAJOR 1
/ MINOR 3. The claim-checker fact-checked every falsifiable claim and returned
**30 CONFIRMED, 0 REFUTED, 11 NOT-CHECKED (semantic)**. With zero refutations,
no verifier finding gets discounted for inaccuracy -- the headline MAJOR survives
the check intact, and the remaining items rest on semantic judgment that lands on
my plate.

### The MAJOR (§g) -- gate-roster delivery mechanism -- is SUBSTANTIVE

The verifier flagged a contradiction between two contracts:

- Both cross-repo system prompts (`arc-runner.system-md` step 5;
  `verifier.system-md` Phase 1.3) say each slice's gate roster lives in a
  sibling `<slice-stem>.gates` snapshot file and that the manifest carries no
  `gates_file:` key.
- The project `arc-runner.user-md` (§Gates) says the arc-runner reads the gate
  set from the manifest's `slice_gates:` block.

The splitter authored only the manifest `slice_gates:` block (`.manifest:9-25`)
and emitted **no** `.slices/<NNN>.gates` files -- the `.slices/` folder holds only
`001.slice-md`..`007.slice-md` (confirmed on disk, claim-check #33/#34). The
verifier correctly identified this but left the "which mechanism is live?"
question open for the reconciler/operator.

I resolved that question against the live tool's source rather than guessing:

- `run_arc_runner.py:2085` -- the running arc-runner calls
  `load_slice_gates(slice_path)`.
- `load_slice_gates.py:43-62` -- that function reads the **`<slice-stem>.gates`
  sibling snapshot file**; when it is absent it returns `[]`.
- `run_arc_runner.py:2086-2091` -- on an empty roster the runtime logs
  `WARN ... no per-slice gate snapshot ... proceeding with zero gates (was the
  splitter skipped?)` and then runs `evaluate_all_gates` against that empty list.
- `load_manifest.py:201` only calls `strip_slice_gates_block` -- it **strips** the
  manifest `slice_gates:` block so the path parser doesn't choke; it never parses
  it for gate selection.
- `parse_slice_gates_block` is invoked **only** in `run_splitter.py:1379` (the
  splitter's own self-validation), never on the arc-runner path.

Conclusion: the live arc-runner reads gate rosters from `.slices/<NNN>.gates`
snapshot files and ignores the manifest block for gate selection. Because those
snapshot files do not exist, this arc would run **every slice with zero gates** --
build, unit-tests, constructor-unit-tests, ui-smoke, ui-tests, impl-log-structure
and state-of-world-structure all silently skipped, and every slice committed
without acceptance verification. That is not benign "intentional project
divergence"; it is a defect that defeats the gate system. **Kept and routed to the
splitter:** it must emit `.slices/001.gates`..`.slices/007.gates` from the spec's
`.locked` gates (mirroring the per-slice rosters already in the manifest's
`slice_gates:` block, which can stay as documentation). The splitter's
`render_slice_gates_block` self-validation can remain; the snapshot files are the
runtime contract.

### The MINORs -- dropped (none substantive)

- **§a (AC-I1 "all UI strings")** -- NOT-CHECKED (semantic). The verifier itself
  calls slice 003's new-constructor-surface-only reading "a defensible reading"
  and frames the item as "worth confirming," not a defect. The spec is about the
  new constructor; retrofitting legacy Shell/FitView/MaterialsView strings is not
  obviously in scope. I leave the splitter's reading standing and pass this only as
  an operator scope-note, not a required slice change. **Dropped** (non-substantive
  judgment the splitter already made defensibly).

- **§c (split Part K out of slice 007)** -- NOT-CHECKED (semantic). An optional
  granularity preference; the verifier concedes the undo/redo coupling is "partly
  unavoidable" because K retrofits a snapshot push onto every prior edit, and the
  slice already flags it High risk. This is operator taste, not a correctness
  defect. **Dropped** (optional; not required for a green slicing).

- **§i (`.user-md` self-citation path `./.user-md/Berreman/...` vs. the resolved
  `.user-md/...`)** -- CONFIRMED as a factual inconsistency, but the verifier
  itself scopes it out: it lives in the project-prompt files, not in the splitter's
  manifest or slice output. Nothing for the splitter to fix in its slicing.
  **Dropped** (out of splitter-output scope).

## Routing

One substantive finding remains (§g), so this is not `done-green`. The finding is
a concrete defect in the splitter's own output (missing snapshot files), not a
citation slip in the verifier's critique, so this is `route-to-splitter` rather
than `route-to-verifier`. Budget is healthy -- round 1 of `splitter_max_rounds: 5`
-- so no escalation. The splitter re-runs as round 2, emits the `.slices/<NNN>.gates`
snapshots, and the loop re-verifies.

---

```json
{"decision":"route-to-splitter","next_role":"split","next_round":2,"next_vn":null,"rounds_consumed":{"split":1,"verify":1,"claim-check":1,"reconcile":1},"verifier_findings_to_apply":["§g MAJOR gate-roster: splitter must emit .slices/001.gates..007.gates from .locked -- live arc-runner (run_arc_runner.py:2085 -> load_slice_gates) reads sibling .gates snapshots and runs zero gates when absent; the manifest slice_gates: block is only stripped, never read for gate selection"],"verifier_findings_to_drop":["§a MINOR AC-I1 'all UI strings' -- splitter's new-constructor-surface-only reading is defensible (verifier's own words); operator scope-note, not a slice defect","§c MINOR split Part K out of slice 007 -- optional granularity preference; verifier concedes the undo/redo coupling is partly unavoidable; not required for green","§i informational .user-md self-citation path mismatch -- CONFIRMED but lives in project-prompt files, outside splitter-output scope"],"next_task_file_path":null,"reasoning_summary":"All verifier facts confirmed (0 refuted); source trace proves the live arc-runner reads .slices/<NNN>.gates snapshots (none exist), so the MAJOR gate-roster finding is substantive -- routing to splitter to emit the missing snapshot files; 3 MINORs dropped as non-substantive."}
```
