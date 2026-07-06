# 0033 — `asg-error`: monitor blind during worker phases (false "stuck loop"); fixed

**Task (operator):** fix the two monitor observability gaps found during this
arc's run and document them. No other changes.

**Status:** ✅ Both gaps fixed in ASG (uncommitted, see §4). ✅ Tested +
live-smoked. Berreman untouched except this log file.

---

## 1. The incident that exposed the gaps

During the resumed run (slice 026, 14:35–14:59 UTC), the monitor showed
`console idle=Ns <-- SILENT (stuck loop / hung gate?)` while the worker was
in fact perfectly healthy — its session transcript was growing ~1 KB/s the
whole time. Conversely, during the *real* stall earlier that day (machine
hibernated ~2 h mid-slice-025), nothing could have pointed at the worker
either, because the monitor had no way to see the worker's own liveness.

## 2. The two gaps (both ASG, both in the 0075 file-first design's seams)

**Gap A — the runner's console stream is legitimately silent for the whole
worker phase.** In background mode, claude.exe's stdout is block-buffered by
Node on a non-TTY (documented in `cli_transport.py`) and is tee'd to its own
`NNN-MM-worker-console-*.log` anyway; the runner writes nothing to the
multiplexed per-attempt console file between `SPAWN` and the first gate. A
0-byte console file for 20+ minutes is indistinguishable from a wedged
runner, so any file-watching monitor false-alarms on every healthy worker.

**Gap B — the watched transcript path was only logged at worker EXIT.** The
runner discovers the worker's live transcript JSONL seconds after spawn and
uses it internally for idle detection, but only logged it in the
`SPAWN-EXIT` row. The STL 0176 proposal (item 3) explicitly wanted the
monitor tailing "the current WORKER-TRANSCRIPT JSONL — last tool call, idle
seconds, API errors/timeouts (the 'stuck in a loop' and 'API timeout'
signals)" — that plumbing never landed, so the monitor could not tell a
stalled stream (the hibernate case) from a healthy quiet one.

## 3. The fixes (ASG repo, branch `0075`)

**Runner side** (`multistep/_common/transport/cli_transport.py`):

- `[worker|heartbeat] <label> elapsed Ns -- idle Ns/limit transcript=NB`
  written to the multiplexed console stream every 30 s
  (`_WORKER_HEARTBEAT_INTERVAL_SEC`) in **both** background loops
  (anthropic + codex) — the same discipline the gate pump already has. The
  per-attempt console file now always carries a liveness signal, and during
  a real stall the operator sees the idle counter climbing toward the
  hangup limit.
- `WORKER-TRANSCRIPT label=<label> watching=<path> initial_size=<N>` logged
  at transcript **discovery** (anthropic loop; codex has no mid-run
  transcript — its rollout is resolved at exit). The `SPAWN-EXIT` row is
  unchanged. Format matches the pre-existing `api_as_cli_transport` row and
  the 0176 proposal's wording.

**Monitor side** (`arc_monitor.py`):

- Tracks the live transcript from `WORKER-TRANSCRIPT` rows (clears it on
  `SPAWN-EXIT` / `WORKER_OUTCOME`), and renders a new status segment:
  `worker-transcript idle=Ns`, with a distinct alert
  `<-- WORKER-SILENT (stream stalled / machine slept?)` when it exceeds
  `--silent-alert`. This is the alert that would have caught the hibernate
  stall in minutes.
- `WORKER-TRANSCRIPT` and `SPAWN-EXIT` added to the rendered event feed.

Interpretation guide after the fix: **console silent** → runner-side
problem (wedged runner / hung gate); **worker-transcript silent** →
worker-side problem (dead API stream, machine slept); both ticking →
healthy, regardless of how quiet the windows look.

## 4. Verification & repo state

- New tests `tests/multistep/test_monitor_liveness_signals.py` (4): the
  anthropic loop announces the transcript at discovery + heartbeats reach
  the console stream; the codex loop heartbeats; the monitor parses the
  exact row the real `Log` emitter produces (including a path with spaces)
  and renders it; non-transcript rows are ignored.
- Full `tests/multistep tests/step_compiler`: **649 passed, 10 skipped,
  0 failed**.
- Live smoke: the modified monitor ran against this spec folder and
  rendered correctly (`completed=26 remaining=0`, runner GONE, no false
  worker alerts, no crash).
- ASG changes **left uncommitted** on branch `0075` (the `.manual/003`
  no-window fix was committed by you as `a04cfdb` in the meantime):
  `cli_transport.py` (M), `arc_monitor.py` (M), plus the new test file
  `test_monitor_liveness_signals.py`. Say the word to commit.
- Not changed (pre-existing, by design): a monitor started against an
  already-finished arc keeps polling until Ctrl+C (its auto-exit arms only
  after it has seen the runner alive once).

`asg-error`: tooling defect in AI-Strategy-Generator (0075 F.2/F.4 left the
console stream empty during worker phases and withheld the live transcript
path from monitors), fixed there — not a Berreman repo issue.
