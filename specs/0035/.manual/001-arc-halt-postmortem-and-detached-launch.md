# Arc-runner halt at slice 005 — postmortem & detached-launch guidance

**Incident date:** 2026-07-07 (local, UTC−4). **Spec:** 0035. **Branch:** `0035`.

This is a human-reference note (per `.manual/README.md` the spec-writer does
not read this folder). It records why the first arc-runner run for spec 0035
stopped mid-flight, what was proven vs. left unknown, and how to relaunch so it
cannot happen again.

---

## 1. What happened

The arc-runner was launched via the CC background-Bash tool
(`run_in_background: true`, task id `bi8hrii4d`). It booted cleanly, committed
**4 of 19 slices**, then its entire process tree was terminated from outside at
**~20:17:51 local (00:17:51Z 2026-07-08)** while slice 005's worker was
actively running. The harness reported the task status as **`killed` / "was
stopped"** (not `failed exit N`).

Committed before the halt (branch `0035`):

| Slice | Commit  | UTC       |
|-------|---------|-----------|
| 001   | `5f7cc59` | 23:34:00 |
| 002   | `8d8b883` | 23:43:27 |
| 003   | `1bf670d` | 23:58:19 |
| 004   | `666a90a` | 00:14:51 |

Slice 005 was mid-implementation and is **not** committed. State file
(`.manifest.state.json`) is frozen at `current_slice idx=4 status="running"` —
i.e. it never finalized, so a re-run resumes from slice 005. The working tree
may hold partial, uncommitted slice-005 edits.

## 2. Death signature (certain)

- Structured log froze at 00:14:56Z; the slice-005 **worker console** kept
  emitting supervisor heartbeats until **00:17:51Z** (`elapsed 180s, idle
  0s/3600s`, transcript growing 172→388 KB), then stopped. The worker was
  **healthy** when killed — not hung, not idle, nowhere near the 3600s
  idle-hangup or 14400s worker timeout.
- **No** `ESCALATE` / `INTERRUPTED` line, **no** traceback, **no** WER /
  Application-Error / .NET-Runtime event. The SIGINT/SIGBREAK handler never
  ran.
- Both runner PIDs (bootstrap `16504` + lock-holding supervisor `36184`) and
  the worker went down together.

This is the fingerprint of an abrupt **`taskkill /T`-style tree-kill of the
task root** — consistent with the harness's `killed`/`stopped` status, and
inconsistent with a self-exit (which shows as `failed exit N`).

## 3. Ruled out (with evidence)

| Hypothesis | Verdict | Evidence |
|---|---|---|
| Internal crash / exception | ❌ | No traceback; no WER / Application-Error / .NET-Runtime events |
| Escalation / timeout / hangup / budget | ❌ | Worker healthy at 180s, idle 0s; no `ESCALATE` |
| Supervisor-lock conflict | ❌ | Single clean owner (pid 36184) from boot; no `REFUSE` |
| Machine sleep / power loss | ❌ | No Kernel-Power events in the window |
| Antivirus / Defender / EDR | ❌ | No Defender/Operational events; no Sysmon installed |
| Human UI cancel | ❌ | Operator confirms zero UI activity |
| My `Stop-Process` on the monitor cascading | ❌ | **Reproduced** — see §4 |

### 4. The reproduction (key experiment)

To test whether hard-killing the hidden monitor's process (`Stop-Process -Id
7468 -Force`, done earlier that session) could have collaterally killed the
arc-runner:

1. Launched two independent background-Bash tasks. Their ancestry: **each has
   its own private `bash.exe` chain**; they converge only at the session
   process `claude.exe 35812`. Not shared.
2. Hard-killed task A's python with `Stop-Process -Force`. **Task B stayed
   fully alive**, with **no `killed` notification**. A reported `failed exit
   127` — exactly like the monitor did.

**Conclusion:** killing one background task's process cannot reach a sibling,
and a killed child always yields `failed exit 127`, never `killed`/`stopped`.
The operator's `Stop-Process` is experimentally exonerated.

## 5. What is NOT recoverable — and why

**Which actor issued the stop cannot be determined from this machine.** Every
record that would name it is unavailable:

- Windows **process auditing is OFF** — Security log has no 4688/4689.
- **No Sysmon**, **no Defender action** — no third-party audit trail.
- The CC harness keeps **no on-disk stop-log** — the session `tasks/` folder
  holds only `.output` files; nothing records why/when a background task was
  stopped.
- The processes are dead; nothing can be inspected live.

**Best-supported explanation:** by elimination, the actor is the **CC harness's
own background-task machinery** (the `killed`/`stopped` status *means* "the
harness stopped this task", and every external cause is ruled out). The
*trigger* is not logged anywhere and remains unproven.

## 6. Recommendations

1. **Launch the arc detached from the CC harness (variant 1).** Start it via
   `Start-Process` in its own console (or a Scheduled Task), **not** the CC
   background-Bash tool, so a harness-side task-stop cannot reach it. This is
   both the **mitigation** and a **confirming experiment**: if a
   harness-independent arc runs to completion where the tracked one died, the
   harness's task management is confirmed as the culprit.
   - Trade-off: a detached arc emits **no harness completion notification**.
     Observe it via the arc-runner log + state file and a **visible**
     `arc_monitor` window.
2. **Instrument the next run to catch the killer:** enable
   `auditpol /set /subcategory:"Process Termination" /success:enable` (and
   Process Creation), or run a `Win32_ProcessStopTrace` WMI watcher that logs
   each PID's exit + parent to a file. Then a recurrence names the killer's
   PID.
3. **Monitor visibly.** The `arc_monitor` is meant to be watched live —
   `python -m ai_strategy_generator.arc_monitor <spec-folder>` in a visible
   console window, **not** a hidden background-Bash task.

## 7. Resume notes

- A re-run resumes from **slice 005** (4/19 committed); the arc-runner's own
  restart logic reconciles the frozen `running` state.
- The working tree may carry partial slice-005 edits from the killed worker.
  Do not hand-edit git — the arc-runner owns commits and re-runs the slice.
- Tool server (`:7351`, the persistent shared service) and the ASG `web_app`
  are unrelated to the halt and were left running.

## 8. Evidence appendix

- Lock owner: `.supervisor-lock.diag` → `pid=36184 acquired_at 22:56:30Z`.
- Bootstrap→supervisor model: `arc-runner.py` is a pass-through that adds
  `src/` to `sys.path` and calls
  `ai_strategy_generator.multistep._common.runtime.main.main`; the forward-slash
  bootstrap (`16504`) spawns the resolved-path supervisor child (`36184`).
- File mtimes at the halt: log `00:14:56Z`, state `00:14:51Z`, slice-005
  console `00:17:51Z`.
- Session process `claude.exe 35812` (session `c92ba779`); the separate
  interactive session is `claude.exe 36536`.
