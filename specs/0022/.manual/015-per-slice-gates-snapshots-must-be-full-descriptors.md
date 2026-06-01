# 015 — per-slice `.gates` snapshots must be full gate descriptors, not the bare-list short form

## The issue

After the manifest framing was repaired (see
[014](./014-manifest-slice-gates-fence-comment.md)) the arc-runner was
launched on `C:\GitHub\Berreman\specs\0022\.manifest`. It booted past the
sentinel / manifest checks but **escalated immediately at the pre-loop
health-check**, before slice 001 ran. Exit code **4**; no `.state.json`
written; no slices touched.

Log line (one per slice, ×16):

```
ESCALATE — health-check failed: per-slice gate snapshot does not parse:
C:\GitHub\Berreman\specs\0022\.slices\001.gates
(gate descriptor 'name' must be a non-empty string
 (C:\GitHub\Berreman\specs\0022\.slices\001.gates))
... (002.gates ... 016.gates, same error)
```

## Root cause

The splitter wrote every `.slices/NNN.gates` snapshot in the **bare-list
short form** — the same shorthand the manifest's `slice_gates:` block uses:

```yaml
# Slice 001 gate roster -- Berreman spec 0022
# Mirrors the manifest slice_gates: entry for this slice ...
gates: [build, unit-tests, constructor-unit-tests]
```

But the arc-runner reads each per-slice snapshot through
`spec_writer/gates/descriptor.py::read`, which expects a **YAML stream of
full `GateDescriptor` documents** (one per gate, `---`-separated), each
carrying at minimum `name`, `description`, `kind`, and — for
`kind: command` — a non-empty `command`. A document with only a `gates:`
key has no `name`, so `validate()` raises
`gate descriptor 'name' must be a non-empty string` for all 16 files and
the health-check escalates the whole arc.

In other words the splitter confused two different on-disk shapes:

- the **manifest** `slice_gates:` block — a *roster* of gate **names** per
  slice (short form, correct as written); and
- the per-slice `<slice-stem>.gates` **snapshot** — the *executable* gate
  **descriptors** the arc-runner runs, which must be the same shape as the
  spec-level locked snapshot `.spec-gates`.

This is almost certainly the splitter going astray on one of its
round-5 / POST-SPLIT passes (the same run that needed the 014 fix): it
emitted name-only rosters into the snapshot files instead of expanding
each roster against `.spec-gates`.

## How to fix it

For each `.slices/NNN.gates`, replace the bare `gates: [...]` line with the
**full descriptor documents** for exactly the gates that slice's roster
names, copied verbatim from the spec-level locked snapshot
`C:\GitHub\Berreman\specs\0022\.spec-gates`. All 16 slices in this spec
share the identical roster `[build, unit-tests, constructor-unit-tests]`,
so each snapshot is the same three descriptors (`build`, `unit-tests`,
`constructor-unit-tests`) as a `---`-separated stream, e.g.:

```yaml
name: build
description: dotnet build Berreman.slnx Release (F# numerical optics solver)
kind: command
command: "dotnet build Berreman.slnx -c Release -nologo -v:m"
cwd: Berreman
pass_when:
  exit_code: 0
  stdout_match: (?s)^(?!.*error).*$
runtime_estimate_seconds: 120
tags: [build]
applies_to:
  - paths: ["Berreman/**/*.fs", "Berreman/**/*.fsproj", "Berreman/**/*.slnx", "Berreman/Directory.Build.props"]
qualifier: build
---
name: unit-tests
...
---
name: constructor-unit-tests
...
```

(A leading `#` comment block ahead of the first descriptor is tolerated —
`yaml_lite` strips full-line comments — so a per-slice header comment is
fine.) Do **not** touch the manifest's `slice_gates:` block; the name-only
roster there is the correct short form for that file.

## What I did

1. Read the parser contract in
   `C:\GitHub\AI-Strategy-Generator\src\ai_strategy_generator\spec_writer\gates\descriptor.py`
   (`read` / `validate` / `_build_descriptor`) to confirm the required
   snapshot shape, and read the authoritative descriptor bodies from
   `.spec-gates` (the handoff-locked gate snapshot).
2. Rewrote all 16 `.slices/001.gates … 016.gates` from the bare-list short
   form into the three-document descriptor stream (`build`, `unit-tests`,
   `constructor-unit-tests`), bodies copied verbatim from `.spec-gates`,
   each with a short per-slice header comment recording the repair.
3. Validated the fix read-only against the real reader: loaded all 16 via
   `descriptor.read(...)`; every file parsed and yielded exactly
   `['build', 'unit-tests', 'constructor-unit-tests']`. No other files
   changed; the arc-runner was **not** relaunched (left for the operator).

## Prevention

The splitter's snapshot-emit step must expand each slice's `slice_gates:`
roster against the spec-level `.spec-gates` and write the **full
descriptor documents** into `<slice-stem>.gates` — never the name-only
`gates: [...]` list. Equivalently, a POST-SPLIT validator should run each
emitted `.gates` file through `descriptor.read` (exactly the arc-runner's
health-check) and fail the split there, so this never reaches an
arc-runner boot. Until the splitter is fixed, re-running it on this spec
will re-introduce the bare-list snapshots and clobber this hand-repair.
