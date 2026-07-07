# 0033 — `asg-error`: arc-runner boot health-check runs before the v7 slice emit

**Task:** `.manual/001-task.txt` — the launcher command
(`Launch the loop on C:/GitHub/Berreman/specs/0033/.manifest ...`) boot-failed
with exit code 4. Find + fix the ASG-side bug directly in
`C:\GitHub\AI-Strategy-Generator`, log it here, and say whether 0033 needs a
re-opened spec / re-submitted handoff.

**Status:** ✅ Root cause found. ✅ ASG code fixed (uncommitted, see §5).
✅ Fix verified end-to-end against a copy of this exact spec folder.
✅ Berreman repo untouched. **No re-handoff needed** (§6).

---

## 1. Symptom

`python arc-runner.py 'C:/GitHub/Berreman/specs/0033/.manifest'` exited 4
(`EXIT_BOOT_FAILED`) with a single `ESCALATE` row in
`.manifest-arc-runner.log`:

```
health-check failed: per-slice gate snapshot missing:
  C:\GitHub\Berreman\specs\0033\.slices\001.gates; ... (all 26 slices)
```

There was no `.slices/` directory at all — neither the `.slice-md`
descriptors nor the `.gates` snapshots the manifest names.

## 2. Root cause (ASG side) — an ordering gap left by the 0065/0176 fixes

This is the direct successor of the bug documented in STL 0176
`.manual/002-manifest-not-authored-root-cause-and-fix.md`:

- **Spec 0065 B.4 (v7 cutover):** the LLM splitter was retired; the
  deterministic step-compiler (`step_compiler/compile.py::compile_spec`)
  re-runs at **arc-runner boot** and regenerates `.slices/<NNN>.slice-md` +
  `.slices/<NNN>.gates` every boot.
- **STL 0176 fix:** handoff now authors `<spec_folder>/.manifest` — and
  *deliberately* left `.slices/` emission to arc-runner boot (0176 doc §1
  called the missing `.slices/` "expected and harmless", §6.4 offered
  emitting at handoff as an option, not taken). So the at-rest v7 shape is:
  manifest present, **no `.slices/`**.
- **The gap:** the boot health-check (spec 005-05 §7, fourth invariant:
  every `<slice-stem>.gates` exists and parses) lives in
  `multistep/_common/runtime/main.py::_boot_health_check` and runs
  **before** `run_arc_runner` — but the B.4 `compile_spec` emit lives
  **inside** `run_arc_runner` (`run_arc_runner.py` ~line 1524). The health
  check therefore fails on exactly the files the compiler would have
  written one step later. First boot of any v7-handed-off spec dies;
  the arc can never reach the code that would heal it.

**Why nobody hit it before 0033:** 0176 never booted from the pure at-rest
shape — its `.slices/` were rendered manually out-of-band alongside the
manifest (0176 doc §3), so the health check found them on disk. **Berreman
0033 is the first spec to boot from the true v7 at-rest shape** (manifest
only), and the "benign" deferral turned out not to be benign.

## 3. The fix (ASG repo, branch `0075`)

`src/ai_strategy_generator/multistep/_common/runtime/main.py`:

- New helper `_pre_emit_missing_step_slices(manifest, log)` — wired into
  `_boot_health_check` immediately before `run_health_check`. It runs
  `compile_spec(spec_folder, repo_root, specrc)` (the same §9-checked,
  deterministic emit B.4 runs later) so the fourth invariant validates the
  descriptors the arc will actually run.
- Deliberately narrow, mirroring B.4 semantics:
  - **new-shape only** — skipped when `<spec_folder>/.spec-jsonl` is absent
    (old-shape specs keep today's behavior exactly);
  - **missing-only** — skipped when every `.slice-md` + `.gates` named by
    the manifest already exists (no rewrite of on-disk snapshots);
  - **graceful-degrade parity** — skipped when `.specrc` fails to load,
    matching the B.4 "no origin source" rule;
  - compile violations/crashes are folded into the health-check `ESCALATE`
    row as `boot pre-emit step-compiler violation: ...` instead of the
    misleading 26 × "gate snapshot missing" rows.
- The B.4 emit inside `run_arc_runner` is untouched (idempotent re-emit,
  exactly as before).

**Test** `tests/multistep/_common/runtime/test_boot_pre_emit_slices.py`
(4 tests): emits missing slices and the invariant then passes; no-op when
snapshots exist (sentinel content survives); skipped for old-shape specs;
§9 violations surface the compiler failure text.

## 4. Verification

- New tests: **4 passed**.
- `pytest tests/multistep tests/step_compiler` → **638 passed, 10 skipped,
  0 failed**. `pytest tests/spec_writer` → **616 passed, 1 skipped**.
- **End-to-end against this spec:** copied `specs/0033` to a scratch
  location (Berreman untouched) and drove the real `_boot_health_check`
  against the copy. Result: `boot pre-emit: step-compiler emitted 26 slice
  descriptor(s) + gate snapshot(s) ahead of the health-check` →
  `health-check passed`; 26 `.slice-md` + 26 `.gates` present. The exact
  0033 failure is healed.

## 5. Repo state

- ASG changes are **left uncommitted** on branch `0075` (same protocol as
  the 0176 fix — I don't commit on your behalf): `main.py` (M, +87/−3) and
  the new test file. Say the word to commit.
- Berreman: **no changes** beyond this log file. The prior failed boot left
  `.manifest-arc-runner.log`, `.manifest.boot-failed.log`, `.artifacts/`
  and `.supervisor-lock*` in `specs/0033` — all benign; the boot-failed log
  self-clears on the next successful boot.

## 6. Reopen the spec / re-submit the handoff? — **No.**

0033's authored content is intact and was never the problem: `.spec-md` +
`.spec-jsonl` (26 steps), `.spec-gates`, `.spec-bundle`, `.locked` and the
handoff-authored `.manifest` are all present and correct — the boot
simulation compiled all 26 steps with **0 violations**. The failure was
purely the ASG boot-ordering defect. With the fix in place, relaunch with
the original command unchanged:

```
Launch the loop on C:/GitHub/Berreman/specs/0033/.manifest using
C:/GitHub/AI-Strategy-Generator/arc-runner.launcher-md
```

Boot will emit `.slices/` itself (that is now the designed, tested path for
every v7 spec — nothing 0033-specific to patch by hand).

`asg-error`: tooling defect in AI-Strategy-Generator (health-check /
step-compiler boot ordering, v7 cutover residue), fixed there — not a
Berreman repo issue.
