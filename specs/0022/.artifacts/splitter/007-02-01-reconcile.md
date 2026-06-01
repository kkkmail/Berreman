# Reconciler decision -- round 2 iter 1

## What I weighed

The verifier returned **NEEDS-REVISION** with 0 BLOCKER, 4 MAJOR, 2 MINOR.
The claim-checker fact-checked the critique and came back with **40 CONFIRMED,
0 REFUTED, 6 NOT-CHECKED (semantic)**. Because nothing was refuted, every
finding's underlying file/line/identifier substrate is accurate -- I take the
verifier's factual base at face value and spend my judgment only on the six
semantic conclusions the claim-checker explicitly left on my plate (#3 verbatim
directive reproduction, #4 one-AC-one-slice coverage, #8 G.11 circularity, #20
under-decomposition, #53 FuncUI build-feasibility).

### The four MAJOR findings -- all substantive, all kept

- **§c -- under-decomposition (headline).** The slicing is a literal 1:1
  Part->slice mapping (claim-check #1, #2, #5), which `splitter.user-md`
  explicitly flags as a smell and asks the splitter to "lean toward more
  slices" / keep them "session-sized -- typically 3-8" (#18, confirmed
  verbatim). The confirmed slice sizes make the oversizing concrete: 001 is
  five `.fsproj` + a test project + slnx wiring + the beam-tree type family +
  the aggregate + JSON seam + schema scaffold (#14); 007 is nine Optimization
  modules + ALGLIB + a UI page + the Wolfram tests across 11 requirements
  (#15); 009 is eight Storage modules + schema + four test files + `.gitignore`
  (#16); 010 is ten UI modules + an environment schema + six test files +
  drag-drop across 12 requirements (#17). The arc-runner contract is confirmed:
  one worker round per slice, every enumerated R-N must land or the round goes
  `commit_ready:false` -> escalation -> failure-budget burn (#13). My semantic
  read agrees with the verifier: against a one-round-per-slice runner these
  slices will escalate on nearly every slice. **Substantive -> splitter.**

- **§i -- FuncUI build path unresolved across the UI-touching slices (002,
  003, 005, 007, 008, 010, confirmed #52).** Every slice inherits "MUST NOT add
  the clone as a dependency until the A.6/A.9 audit passes," and the clone is
  the only FuncUI source the spec names (#53, the build-will-fail conclusion is
  semantic). My read: F# `view` bodies that reference FuncUI DSL types cannot
  compile with no FuncUI reference at all, so as literally drafted the `build`
  gate -- which runs on every slice -- is unsound from the first UI slice. The
  verifier correctly notes a resolvable path exists (the public MIT
  `Avalonia.FuncUI` NuGet is *not* the gated clone), but no slice states it.
  **Substantive -> splitter** (state the buildable interpretation in slice 001).

- **§g -- slice 009's Part I tests live in a `StorageTests` project the gate
  never runs.** The `constructor-unit-tests` gate command runs only
  `OpticalConstructor.Tests.fsproj` (#42); slice 009 plans a separate
  `OpticalConstructor.StorageTests` project and even adds it to the slnx
  (#43), hedging that the gate "MUST cover whichever project hosts them" (#44)
  -- but the gate descriptor is fixed and locked. As drafted, AC-I1..AC-I11
  would build but never execute under the gate. **Substantive -> splitter**
  (host the Part I tests in the gated `OpticalConstructor.Tests` project).

- **§a -- G.11 / AC-G9 Wolfram reference data is not in the repo.** Slice 007
  R-11 / AC-G9 require encoding the converged parameters and final chi-squared
  from the ported Wolfram runs and asserting `LocalRefinement` reproduces them
  (#6, confirmed verbatim), yet spec G.0 states that workflow "lives outside
  the repository entirely" (#7) and no fixture carries those values. The
  circularity conclusion is semantic (#8); my read backs the verifier -- with
  no recorded values, the test either fabricates the expected numbers
  (circular) or is unauthorable. **Substantive -> splitter**: either carry the
  recorded Wolfram values as an explicit fixture input, or re-scope G.11 to a
  self-consistency/regression test with the limitation documented.

### The two MINOR findings

- **§g MINOR -- gate roster in the manifest `slice_gates:` block rather than
  per-slice `.slices/<NNN>.gates` snapshot files (#45-#47, all confirmed).**
  This is a real divergence from the cross-repo snapshot-file contract, but the
  project `arc-runner.user-md` explicitly sanctions reading the set "from the
  manifest's `slice_gates:` block" (#47), so it is functionally unambiguous,
  not a blocker. Since the splitter is regenerating the slicing anyway, I fold
  this in as a low-cost cleanup: emit the per-slice `.gates` snapshots to match
  the protocol while re-decomposing. **Kept (minor, fold into the splitter
  round).**

- **§g MINOR -- `worker_timeout_sec: 21600` (6 h) is high (#19, #48).** This is
  a direct symptom of the oversized slices in §c, not an independent defect.
  Right-sized slices will not need it. **Kept as a derivative of §c**: once
  slices are session-sized, right-size the timeout.

## Routing

Nothing was refuted, so no finding is dropped. All four MAJOR findings are
substantive and the two MINOR items are real (one a protocol cleanup, one a
derivative of the headline). The output needs reworked slicing, not a clean
re-verify of the existing slicing -- so this is **route-to-splitter**, not
route-to-verifier.

Budget check: `splitter_max_rounds` is 5; `split` has consumed 2 rounds, so the
next splitter pass is **round 3** -- comfortably within budget. No escalation
warranted.

---

```json
{"decision": "route-to-splitter", "next_role": "split", "next_round": 3, "next_vn": null, "rounds_consumed": {"split": 2, "verify": 1, "splitter-claim-check": 1, "reconcile": 1}, "verifier_findings_to_apply": ["§c MAJOR under-decomposition: 1:1 Part->slice mapping, re-decompose oversized Parts A/G/I/J into session-sized slices", "§i MAJOR FuncUI build path unresolved across UI slices (002,003,005,007,008,010): slice 001 must state the buildable interpretation (public Avalonia.FuncUI NuGet, only the clone is audit-gated, or stub views)", "§g MAJOR slice 009 Part I tests must live in the gated OpticalConstructor.Tests project, not a separate OpticalConstructor.StorageTests", "§a MAJOR G.11/AC-G9 Wolfram reference data absent: carry recorded fixtures as explicit input or re-scope to a self-consistency regression with the limitation documented", "§g MINOR emit per-slice .slices/<NNN>.gates snapshot files to match the protocol contract", "§g MINOR right-size worker_timeout_sec once slices are session-sized (derivative of §c)"], "verifier_findings_to_drop": [], "next_task_file_path": null, "reasoning_summary": "Claim-check confirmed 40 facts with 0 refuted; all 4 MAJOR + 2 MINOR findings substantive; routing to splitter round 3 to re-decompose oversized slices and fix FuncUI/gate/Wolfram issues."}
```
