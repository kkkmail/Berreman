# 0033 — `asg-error`: visible empty worker window under the detached runner; monitor launched

**Task (operator, this session):** the arc's worker paints an extra visible
"Claude" window that emits nothing — 0075 was supposed to remove all worker
windows. Find + fix the ASG bug, document here. Also: launch the Python
arc monitor that supersedes the CLI UI.

**Status:** ✅ Root cause found (with live process-tree evidence and an
isolated repro). ✅ ASG code fixed (uncommitted, see §5). ✅ Monitor found
and launched. ⏳ Operator to advise whether to restart the current run
(the fix does not retro-apply to the already-running arc, §6).

---

## 1. The monitor

It exists and works: `src/ai_strategy_generator/arc_monitor.py`
(spec 0075 F.4, from STL 0176 `.manual/012` item 3). Read-only,
start/stop-anytime, owns nothing — killing it affects nothing. It tails
`.manifest.state.json`, the structured `.manifest-arc-runner.log`, the
current `CONSOLE-FILE` roll under `.artifacts/`, and probes the runner pid
from `.supervisor-lock.diag`; it renders slice progress, gate results,
commits, escalations, and derived alerts (runner-gone, console-silent).

Launched for this arc in its own console window
("Arc Monitor - Berreman 0033"). To start one yourself, any time,
as many as you like:

```
python -m ai_strategy_generator.arc_monitor C:\GitHub\Berreman\specs\0033
```

## 2. Symptom (the extra window)

A visible console window titled `claude`, showing nothing, appeared as soon
as slice 001's worker spawned. Process-tree evidence:

```
claude.exe  (PID 15656, HAS a visible window, stdout/stderr piped)
  └─ parent: python arc-runner.py (PID 20012 — the 0075 DETACHED re-spawn)
       └─ parent: python arc-runner.py (PID 27444 — the launcher's first process)
```

No `cmd.exe` wrapper in the chain and no `tool_agents` section in
Berreman's `.specrc` → this is the **background** spawn path
(`_spawn_anthropic_background_mode`, piped, `multistep_console` default
False) — the path whose docstring says "pipes only, **no window**".

## 3. Root cause (ASG side) — 0075's detach exposed a missing `CREATE_NO_WINDOW`

Windows rule: a **console-subsystem child** spawned by a **console-less
parent** gets a brand-new **visible** console window unless
`CREATE_NO_WINDOW` is passed.

- **Pre-0075:** the runner re-spawned itself with `CREATE_NEW_CONSOLE` and
  owned a visible console; every piped child (worker, gate, git, taskkill)
  silently *attached to that existing console* — no new window, so the
  missing flag was invisible.
- **Spec 0075 F.1:** the runner now re-spawns `DETACHED_PROCESS |
  CREATE_BREAKAWAY_FROM_JOB` — it has **no console at all**. Every piped
  child spawn that never passed `creationflags` now allocates its own
  fresh visible console: the empty `claude` window per worker attempt —
  and, later in the slice, the same would happen for **every gate
  subprocess** (dotnet builds/tests — minutes-long empty windows) and as
  **window flashes** for every short-lived `git` / `taskkill` /
  lifecycle-step call (the `wait_for` poller runs one every few seconds).

Isolated repro (scratch, no ASG code): a `DETACHED_PROCESS` python parent
spawning a piped `cmd /c ping ...` child — without flags a visible window
appears; with `no_window_creationflags()` none does. Confirmed live on this
machine.

## 4. The fix (ASG repo, branch `0075`)

One shared helper so the discipline lives in one place:

- **New** `multistep/_common/spawn/no_window.py` —
  `no_window_creationflags()` → `CREATE_NO_WINDOW` on Windows, `0`
  elsewhere. (The child still gets a hidden console, so console APIs keep
  working; all callers pipe or capture stdio.)
- **Applied at every headless spawn in the multistep runtime (9 sites):**
  - `transport/cli_transport.py` — anthropic + codex background worker
    `Popen`s (the observed window);
  - `gates/evaluate_gate.py` — gate main-command `Popen`, lifecycle-step
    `subprocess.run` (setup / wait_for / teardown / analyzer), and the
    silence-watchdog `taskkill`;
  - `spawn/kill_process_tree.py` — the worker-tree `taskkill`;
  - `git_ops/git_run.py` + `git_ops/porcelain_paths.py` and the inline
    `git rev-parse HEAD` in `run_arc_runner.py` — the commit machinery's
    constant git calls.
- **Deliberately untouched:** spec 008-17 console mode's
  `CREATE_NEW_CONSOLE` (a visible window by request), the api-as-cli
  transport's operator-intervention console (visible by design), and the
  spec-writer's `agent_subprocess` (its stop path delivers
  `CTRL_BREAK_EVENT`, which requires a shared console — hiding it would
  break `stop_agent`).

**Tests** `tests/multistep/test_no_window_spawn.py` (7 tests) pin the flag
at each seam: both worker background spawns, gate main command, lifecycle
step, taskkill helper, `git_run`, and the helper's platform value.

## 5. Verification & repo state

- New tests: **7 passed**. Full `tests/multistep tests/step_compiler`:
  **645 passed, 10 skipped, 0 failed**.
- Live repro (§3) demonstrates bug-without-flag / no-window-with-flag using
  the real helper.
- ASG changes **left uncommitted** on branch `0075` (same protocol as
  before): 6 modified files (+40 lines, comments included) + 2 new files
  (`no_window.py`, the test). The 002 boot-ordering fix was committed by
  you as `1b6fa01` — this change is stacked on top of it, say the word to
  commit.
- Berreman repo: untouched except this log file.

## 6. Effect on the CURRENT run — restart is the operator's call

The running arc (booted 2026-07-06 00:24 UTC) loaded the OLD code at boot;
a fix on disk cannot reach a live Python process. Until restarted, the
current run will keep painting one empty `claude` window per worker attempt
and empty/flashing windows for gates and git calls. Functionally it is
unaffected — the windows are cosmetic; state, gates, commits, logs are all
identical.

If you decide to restart: interrupt the arc (kill the background shell /
the runner pid from `.supervisor-lock.diag`; the SIGINT handler writes
`operator-interrupt` to the state file), then relaunch with the same
command — the state-file resume logic continues from the interrupted
slice. Waiting for the arc to finish and picking up the fix on the next
arc is equally fine. Your call, per your note — I have not touched the
running arc.

`asg-error`: tooling defect in AI-Strategy-Generator (0075 F.1 detach did
not propagate no-window discipline to child spawns), fixed there — not a
Berreman repo issue.
