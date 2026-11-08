# Step 0181 — state of the world

# Where we are

Spec 0050 section S5.3 builds the kit window: title, Kit menu, lock, reload and mode. This step gives every kit folder
one spelling, `KitFolder.canonical`.

- The kit history (step 164) now names a kit through that key.
- The one-window-per-kit rule (step 190) will name a kit the same way (001 §E "one window per kit"; 001 §F, one history
  entry per folder).

The history's ordinal-ignore-case text comparison is deleted, so one rule decides when two folders are one kit.

# What's working

- Add `KitFolder.canonical`: `/` separators, no trailing `/`, upper-cased. It touches no disk and is a key, never a path
  to show.
- Route `KitHistory`'s one private folder comparison through `canonical`, and delete its `OrdinalIgnoreCase` comparison.
- `star`, `unstar`, `forget`, `bump` and `relocate` now match a kit by its folder in any spelling.
- Add the facts `two spellings of one kit folder share one canonical form` and `a kit is matched by its folder in any
  spelling`.

# Tests

This worker ran no build, test, analyzer or invariant gate: the IMPLEMENT protocol (Invariant 6) gives gate execution to
the gate engine. The engine runs:

- build and elevation;
- constructor-unit-tests, all discovered;
- the fourteen s21 invariants.

Checked without running a gate:

- every edited file is LF;
- every existing kit-history folder keeps its old matches under the new rule.

Test identities:

- two facts are created, `KitTests.two spellings of one kit folder share one canonical form` and
  `KitHistoryTests.a kit is matched by its folder in any spelling`;
- none is removed or renamed.

The count below is inherited from checkpoint 179 and was not measured this round. With the two new facts, the
constructor-unit-tests count is expected to measure 1188.

```yaml
gates:
  constructor_unit_tests: 1186
```

# Architecture

- `canonical` is an intrinsic extension member on `KitFolder`, in `Kit.fs` beside `tryKitPathOf`, so every consumer of
  `KitFolder` sees it without a new `open`. It answers a `KitFolder`, so a key compares to a key with plain union
  equality.
- `KitHistory.tryMatchFolder` stays the history's single folder rule. The verbs reach it unchanged (.spec-md §4: "A reuse
  is a move or a call").

# Deferred

Nothing. Every requirement of the step landed this round.

# Gotchas

- `canonical` is never shown or opened. History entries keep the folder text the user chose, and the new history fact
  asserts it.
- `entriesMatching`'s `OrdinalIgnoreCase` `Contains` is the filter box's substring rule, not folder identity. It is kept.
- `TrimEnd('/')` keys a drive root `C:/` as `C:`. That is harmless for a key, and such a folder is never a kit
  (`namedKitFile` refuses it).

# Changelog

- 2026-11-08 — Step 0181: `KitFolder.canonical` added; `KitHistory` matches folders through it; two facts added.
