# Slice 006 — impl log

## Progress

- [x] Read the worker system prompt (implement_contract_worker + arc-runner base), project prompt (Operator note: empty), slice spec, gates snapshot, step-003/005 outputs, and the current `MaterialLibrary` / `Library` modules + `MaterialProxyTests`.
- [x] Impl-plan written.
- [x] Red: `MaterialProxyTests.fs` rewritten against `MaterialProxy.createInMemory (samplesReferencing …)` / `SampleStructure.referencedMaterials` — build failed with `FS0039: The type 'MaterialProxy' does not define … 'createInMemory'` and `FS0039 … 'referencedMaterials'` (the missing production symbols; counts as red per the worker base; log `006-red-tdd.log`).
- [x] Green: the stateful in-memory store implemented (`MaterialLibrary.fs` pure pieces + the `type MaterialProxy with static member createInMemory` augmentation and `samplesReferencing` in `ElementId.fs`); `constructor-unit-tests` now 350 passed / 0 failed (`006-constructor-unit-tests.log`).
- [x] Registry lifecycle → `implemented` (`specs/0033/.contracts-json`: STORE_XDUO_0001 `lifecycle: implemented`, `implementStep: 6`; CRLF preserved byte-checked, JSON re-validated).
- [x] Remaining gates run locally (advisory) — all five green; logs in `.artifacts/`.
- [x] State-of-the-world written.

## Files modified

- `specs/0033/.slices/006-impl-plan.md` (new)
- `specs/0033/.slices/006-impl-log.md` (new, this file)
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/MaterialLibrary.fs` — the step-003 validate-only mock `createInMemory` DELETED (grep: only the step-003 tests consumed it); new pure search seam `byQuery : MaterialQuery -> MaterialLibrary -> MaterialEntry list` (reuses `byNameContains` → `byCategory` → the `DispersionFilter` facet over the private `hasDispersion`); `validateEntry` de-privatized (reused by the store in `ElementId.fs`); `MaterialProxy` / `hasDispersion` / `validateEntry` doc comments updated to the implemented lifecycle.
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/ElementId.fs` — `SampleStructure.referencedMaterials : Set<MaterialId>` (films with `Repeated` cells counted once, substrate plate, lower half-space); `type MaterialProxy with static member createInMemory (samplesReferencing : MaterialId -> Sample list) : MaterialProxy` — the real store: `ref Map<MaterialId, MaterialEntry>` seeded from `builtInEntries`, mutation confined to the closure, `removeMaterial` hard-blocks with `MaterialStillReferenced` naming the referencing samples (never cascades, never silently deletes); module-level `samplesReferencing (samples : SampleProxy)` — the composition-root lookup backed by the LIVE step-005 samples store.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/MaterialProxyTests.fs` — rewritten for the stateful store: every test builds a FRESH proxy (`fun _ -> []` referencing, or the composed `SampleProxy`-backed pair); step-003 read-surface tests kept (list/search text/category/dispersion partition/facet composition/tryGet); +stateful round-trips (add-then-list+search, stateful duplicate-add, seeded duplicate-add, blank-name add/update leave the store unchanged, update-then-get, unknown update/remove, remove-then-search incl. remove-again-unknown); +the referenced-block acceptance (glass152 blocked naming the referencing seeds, both stores unchanged; glass200 unreferenced removal succeeds; a lower-half-space-only reference blocks; the lookup is LIVE — removing the referencing sample unblocks); +the pure `referencedMaterials` facet test; the step-003 stub six-function acceptance kept.
- `specs/0033/.contracts-json` — STORE_XDUO_0001 → `lifecycle: implemented`, `implementStep: 6` (CRLF preserved).

## Decisions

- **The store lands in `ElementId.fs`, not beside the mock it replaces.** The pinned
  parameter `samplesReferencing : MaterialId -> Sample list` is `Sample`-typed and
  `Sample` (`Library` module) compiles AFTER `MaterialLibrary.fs` — so
  `MaterialProxy.createInMemory` is a type augmentation after the `SampleProxy`
  augmentation (the step-005 precedent, adapted: this one is an OPTIONAL extension,
  the type being declared in another file).
- Because an optional extension cannot reach `MaterialLibrary`-private bindings,
  `validateEntry` loses `private`; `hasDispersion` STAYS private behind the new pure
  `byQuery` seam, which is what the store's `searchMaterials` answers through — this
  also honours the slice's "reuses the pure byCategory / byNameContains filters"
  literally (both are called by name inside `byQuery`).
- The slice's parenthetical defines dispersive as "epsWithDisp is the func case"; the
  step-003 `hasDispersion` also treats a mu/rho func case as dispersive. Every
  dispersive entry (silicon, langasite) carries the eps func case, so the definitions
  coincide on all built-ins; the broader, already-reviewed step-003 classifier is kept
  (a compatible superset — an eps-func entry is always classified dispersive).
- `removeMaterial`'s `MaterialStillReferenced` reason carries the entry name, the id's
  Guid string, the referencing-sample count, and the sorted quoted sample names — the
  slice's "a reason naming the referencing samples".
- The composition-root lookup `samplesReferencing` filters `listSamples ()` by the new
  pure `SampleStructure.referencedMaterials`; it consults the CURRENT map every call
  (live — no snapshot), which the "removing the referencing sample unblocks" test pins.
- The step-003 blank-name `InvalidMaterial` validation is kept in the real store
  (dropping it would orphan the error case and break pinned tests).

## Testing state

TDD sequence: red first (`FS0039` on the missing `MaterialProxy.createInMemory` /
`SampleStructure.referencedMaterials` — the missing-production-symbol red the worker
base names), then the implementation, then green. Local (ADVISORY — the arc-runner
gate engine is the sole gate authority and re-runs after exit):

- `build` — `dotnet build Berreman.slnx -c Release` (cwd `Berreman/`): Build
  succeeded, `0 Error(s)`; 0 occurrences of lowercase "error" in the log (the gate
  regex); warnings are the pre-existing NU1901/02/04 package advisories, MSB3277,
  FS1125 (`SeriesDataTests.fs`), FS0044 (`TestWindows/ChartWindow.fs`), SYSLIB0051
  (vendored MathNetNumerics) — none from touched files.
- `unit-tests` (BerremanTests, `--no-build`): 84 passed, 5 skipped (pre-existing
  skips), 0 failed — matches the 005 baseline.
- `constructor-unit-tests`: 350 passed, 0 failed (+6 over the 344 baseline).
- `ui-smoke`: 54 passed, 0 failed.
- `ui-tests`: 249 passed, 0 failed.

No CRLF churn: `git diff --numstat` equals `--ignore-cr-at-eol`; all touched source
files are LF-only; the registry stays CRLF (byte-checked with `od`).

## Artifacts

All in `specs/0033/.artifacts/`:

- `006-red-tdd.log` — the failing (red) build naming the missing production symbols.
- `006-build.log` — the build-gate command output.
- `006-unit-tests.log` — BerremanTests run.
- `006-constructor-unit-tests.log` — the 350-test green run.
- `006-ui-smoke.log` / `006-ui-tests.log` — the two headless UI gate runs.

## Gotchas

- The slice cites `MaterialLibrary.fs:53,57,79` for `byCategory`/`byNameContains`/
  `builtInEntries` — those line numbers predate steps 001–003 (today: 108, 112, 141).
  The named symbols are unambiguous; the citation drift is cosmetic.
- Unlike step 005's intrinsic augmentation, `type MaterialProxy with` in
  `ElementId.fs` is an OPTIONAL extension (the type lives in `MaterialLibrary.fs`) —
  call sites need `open OpticalConstructor.Domain.Library` for
  `MaterialProxy.createInMemory` to resolve, and the extension cannot reach
  `MaterialLibrary`-private bindings (hence `validateEntry` de-privatized, `byQuery`
  introduced over the still-private `hasDispersion`).
- `samplesReferencing`'s pinned signature (`MaterialId -> Sample list`) carries no
  error channel. The in-memory `listSamples` is total (always `Ok`), so the
  degenerate `Error → []` branch is unreachable today; a future store whose listing
  can fail must supply its own conservative lookup — documented on the function.
- Dispersive classification kept from step 003 (eps OR mu OR rho func case), a
  compatible superset of the slice's "epsWithDisp is the func case" — identical on
  every built-in (silicon and langasite both carry the eps func case).
- A stateful proxy shared across tests is order-dependent (the step-005 gotcha) —
  every `MaterialProxyTests` test builds a fresh proxy; keep it that way.
- `listMaterials` returns Map (Guid) order, not seed order — the contract's
  `MaterialEntry list` carries no order guarantee and no test asserts one; anything
  needing display order should sort explicitly.
- glass200 is the ONLY built-in no seeded sample references — the "removing an
  unreferenced material succeeds" acceptance leg depends on it; if a future seed
  references glass200, that test needs a new unreferenced material.
- `.manifest.state.json` shows as modified in `git status` — the arc-runner's own
  file (same as slices 001–005), left alone.
