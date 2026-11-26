# 003 — Handover: spec 0050 from authoring to the arc

Written 2026-11-05, after the handoff commit `bbe71da` (06:19). It records the state the arc starts from, what went
wrong while the spec was written and how ASG was fixed, and the one cost the arc should expect.

## State at handoff

- **Spec:** 47 sections (S1.1–S14.2 plus Close), 494 steps. It is locked (`.locked`), with `.manifest` and
  `.slices/0001`–`0494` written.
- **Slice width:** 4 digits. BRM's `.specrc` gained `"slice_width": 4` on 2026-11-03, and the `.manifest` records
  `slice_width: 4`. Older spec manifests that lack the field (0033–0042, among others) now read as 4 if they are re-run
  or checked by the doctor; adding `slice_width: 3` to their headers would pin them.
- **Claim check:** fully green on Claude, 178 of 178 claims confirmed.
- **Gates:** two required-only gates cycles (`a2c3ffe`, `796c351`). Every step's test selection is valid; the
  compiler's executability check reports no finding.
- **Agent:** `.specrc` `agent` is `openai-codex` (switched 2026-11-04 14:10). Codex runs as `gpt-6-astra` at `xhigh`,
  passed on the command line from ASG's `appsettings.json` (`providers.openai.subscription`), so its own `config.toml`
  does not choose the model. The claim check that converged ran on Claude.

## How the authoring went

| When | What |
|---|---|
| 11-03 09:05 | Section plan, 47 sections; approved after review. |
| 11-03 10:47 – 11-04 12:22 | 47 section mappers, one commit each (`d16b189` … `649bd12`). |
| 11-04 12:22 | Reduce escalated: `DISCOVERY_UNKNOWN [kit-acceptance]` on steps 421–440, 460 and 476 (ASG defect 1 below). |
| 11-04 13:46 – 14:20 | After the ASG fix, an empty re-submit ran reduce (green) and the whole-spec critics; committed `5b54faf`. |
| 11-04 15:47 – 21:41 | Claim-check auto-loop, 10 spec cycles and 11 claim checks; ended red. |
| 11-04 21:57 – 11-05 06:19 | A second auto-loop converged; gates cycles; handoff. |

The first auto-loop's findings per claim check ran 45 → 7 → 6 → 4 → 3 → 2 → 3 → 6 → 3 → 4 → 5. They did not converge
because three passages were contested every round:
- §3.1 (line 91), §3.16 (line 113) and §3.23 (line 128);
- the checker inferred that `kit-acceptance` must run at steps 2, 3, 5, 7, 339, 340 and 344–346, because those steps
  edit paths in the gate's `applies_to`;
- the gate is inert before step 421 creates `KitAcceptanceTests.fs`, the file its `requires_path` waits for, and
  ASG's own compiler leaves it out of those steps.

Two further findings were bare `OpticalConstructor.*` paths in §4 (lines 192–193), missing their `Berreman/` prefix.

## ASG defects found and fixed (all in `ASG-ERRORS.md`, committed in ASG `e24cf19c`)

1. **A suite the spec creates could never be discovered.** MTP's `--list-tests` refuses an empty listing, so a gate
   whose first test the spec writes has no census row, and every step creating or selecting such a test failed
   `DISCOVERY_UNKNOWN`. The section mappers had reported the gap since S12.1, but per-section checks do not block, so
   it surfaced only at reduce, 26 hours later.
   - Fix: `executability/temporal._founds_suite`. A `create` whose `path` is in its gate's `requires_path` founds
     that suite empty; later steps extend it.
   - Wiring: `checker.check_plan` and `runtime_tests` pass the gate catalog in.
   - With the fix, `check_spec` over 0050 reported no failure.
   - Test: `tests/executability/test_founded_suite.py`.
2. **Every section mapper's `.history` row carried the cycle's start time** (`2026-11-03T15:18:31Z`).
   - Cause: `_run_sectioned_campaign` gave every planner, mapper and revision prompt the cycle's single `at`.
   - Fix: each prompt gets the current time.
   - `commit_ref` stays the last `.spec-md` commit, by contract.
   - Not fixed: S10.2's row is `kind: "output"` instead of `"decision"`, and nothing validates the kind of rows the
     agents write.
3. **After an escalation, Submit with an empty comment did nothing visible.**
   - Cause: a settled plan-approval checkbox renders `disabled`, so the browser never posts it. The route then
     rejected the empty comment with a banner that landed above the fold.
   - Fix: `ui/spec_writer_routes._campaign_resumable`. An unfinished sectioned campaign that is not awaiting
     approval resumes without a comment.
4. **Submit returned a plain-text 409 that htmx never shows.** Logged as BRM-0050 and still open: the cycle routes'
   409s (stale code, a cycle already running, a locked spec) should answer as the commit pre-flight does, with a
   popup.

Also observed, not fixed:
- The spec page shows the campaign as `complete` as soon as reduce passes, while the whole-spec critics still run.
  No stage label tells the operator that a cycle is still running.
- The claim checker works out its own gate plan, on a different rule from ASG's compiler (the cause of the
  non-convergence above).

## What the arc should expect

**Gates.** Every test gate is filtered except the light ones and the final whole-tree proof:

| Gate | Steps whole | Steps filtered | Note |
|---|---|---|---|
| `build`, `elevation` | 494, 493 | — | No selector. `elevation` scopes itself to the projects a step touches. |
| `elevation-full` | 494 only | — | Final stage. |
| `constructor-unit-tests` | 426 | 7 | Light: 13 s whole. |
| `ui-smoke` | 212 | — | Light: 27 s whole. |
| `ui-tests` | 494 only | 218 | 61 classes. The heaviest it often picks is `FsxAcceptanceTests` (~1.5 min, 42 steps). |
| `unit-tests` | 494 only (~52 min) | 66 | 12 classes. The inverse-fit classes run up to ~16 min each. |
| `kit-acceptance` | 494 only | 38 | See below. |
| Section invariants | every step of their section | — | `git grep` checks, seconds each. |

**Watch `kit-acceptance` (steps 421–494).**
- All 38 filtered steps select the one class `KitAcceptanceTests`, so every run is in effect the whole suite,
  including AT-2's 946-second fit. That is up to ~19 hours at the gate's 1,800-second estimate.
- The selector filters by class, so one method cannot be chosen.
- If the time becomes a problem, put AT-2 in its own class, or narrow the gate's `applies_to`. Either is a spec
  change.

**Unit-tests time.** The selected inverse-fit runs total several hours: `MuellerInverseTests` 20 steps,
`FeasibleBiaxialInverseTests` 10, `AbsorbingBiaxialInverseTests` 8, `BiaxialInverseTests` 7.

**Codex differs from Claude on one point.** Under Claude the worker cannot rewrite `.history`, because a guard blocks
it. Under Codex no guard exists, and only an instruction in the prompt keeps the file intact.

## Machine notes

- **ASG web app:** restarted 2026-11-04 13:43 (PID 115364) with every fix above. Restart it after any ASG source
  change, or cycles are refused because the app's code is stale.
- **PowerShell execution policy on this machine:**
  - PowerShell 7 (`pwsh`): LocalMachine is `Unrestricted`.
  - Windows PowerShell 5.1: LocalMachine was still `AllSigned` on 11-04, and blocked
    `.yolo\Launch-YoloCodexRelay.ps1`.
  - To fix it, run once in an elevated `powershell.exe`:
    `Set-ExecutionPolicy Unrestricted -Scope LocalMachine -Force`.
- **ASG's git:** refuses the claude-agent account ("dubious ownership"). Read-only commands work with
  `git -c safe.directory='*'`.
