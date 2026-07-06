# Slice 005 — impl log

## Progress

- [x] Read the worker system prompt (implement_contract_worker + arc-runner base), project prompt, slice spec, gates snapshot, step-004 outputs, and the current `Library` module / `SampleProxyTests`.
- [x] Impl-plan written.
- [x] Red: round-trip tests written against `SampleProxy.createInMemory` — build failed with `FS0039: The type 'SampleProxy' does not define … 'createInMemory'` (the missing production symbol; counts as red per the worker base).
- [x] Green: the stateful in-memory store implemented in `ElementId.fs` (type augmentation `type SampleProxy with static member createInMemory`); `constructor-unit-tests` now 344 passed / 0 failed (`005-constructor-unit-tests.log`).
- [x] Registry lifecycle → `implemented` (`specs/0033/.contracts-json`: STORE_XDUO_0002 `lifecycle: implemented`, `implementStep: 5`; CRLF preserved, JSON re-validated).
- [x] Remaining gates run locally (advisory) — all five green; logs in `.artifacts/`.
- [x] State-of-the-world written.

## Files modified

- `specs/0033/.slices/005-impl-plan.md` (new)
- `specs/0033/.slices/005-impl-log.md` (new, this file)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/SampleProxyTests.fs` — every test now builds a FRESH `SampleProxy.createInMemory ()` (a shared stateful proxy would be order-dependent); +5 round-trip tests with fixed literal Guids (add-then-list, stateful duplicate-add, update-then-get, fixed-Guid unknown-update, remove-then-search incl. remove-again-unknown); header/section comments updated; the step-004 stub acceptance test kept verbatim.
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/ElementId.fs` — `createInMemorySampleProxy` (validate-only mock) REPLACED by `type SampleProxy with static member createInMemory () : SampleProxy`: `ref Map<SampleId, Sample>` seeded from the `SampleItem`s of `seedEntries`, mutation confined to the closure; `SampleProxy` type + `validateSample` doc comments updated to the implemented lifecycle.

## Decisions

- `SampleProxy.createInMemory` lands as an intrinsic type augmentation (`type SampleProxy with`) placed after `seedEntries` / `validateSample`, giving the slice's exact call shape `SampleProxy.createInMemory : unit -> SampleProxy` — the module-level name `createInMemory` is taken by the `LibraryProxy` builder (step-004 gotcha).
- The step-004 validate-only mock `createInMemorySampleProxy` is REPLACED (deleted), per the IMPLEMENT_CONTRACT obligation "replacing the mock with genuine behaviour"; its only consumers were the step-004 tests, which now build a fresh real proxy per test.
- The blank-name `InvalidSample` validation from step 004 is kept in the real store (the declared contract carries the case; dropping it would orphan the error case and break two pinned tests).
- Seeding reads the `SampleItem`s out of `Library.seedEntries` (the slice's wording), which is exactly `SeedSamples.all`.

## Testing state

TDD sequence: red first (`FS0039` on the missing `SampleProxy.createInMemory` —
the missing-production-symbol red the worker base names), then the
implementation, then green. Local (ADVISORY — the arc-runner gate engine is the
sole gate authority and re-runs after exit):

- `build` — `dotnet build Berreman.slnx -c Release` (cwd `Berreman/`): Build
  succeeded, 0 errors; 0 occurrences of lowercase "error" in the log (the gate
  regex); the 93 warnings are the pre-existing NU1901/NU1902/NU1904
  CoreWCF.Primitives package-vulnerability advisories — none from touched files.
- `unit-tests` (BerremanTests, `--no-build`): 84 passed, 5 skipped
  (pre-existing skips), 0 failed — matches the 004 baseline.
- `constructor-unit-tests`: 344 passed, 0 failed (+5 over the 339 baseline).
- `ui-smoke`: 54 passed, 0 failed.
- `ui-tests`: 249 passed, 0 failed.

No CRLF churn: `git diff --numstat` equals `--ignore-cr-at-eol`; all touched
source/markdown files are LF-only; the registry stays CRLF (arc-runner-owned).

## Artifacts

All in `specs/0033/.artifacts/`:

- `005-build.log` — the build-gate command output.
- `005-unit-tests.log` — BerremanTests run.
- `005-constructor-unit-tests.log` — the 344-test green run.
- `005-ui-smoke.log` / `005-ui-tests.log` — the two headless UI gate runs.

## Gotchas

- The step-004 SoW forecast the real store would land in
  `OpticalConstructor.Storage`; this slice's `touches` pins
  `OpticalConstructor.Domain` + `OpticalConstructor.Tests` and its how-to
  specifies an in-memory `ref Map` closure — the slice spec wins; the store
  lives in `ElementId.fs` beside the contract.
- `SampleProxy.createInMemory` must be a TYPE AUGMENTATION placed after
  `seedEntries` (F# is order-dependent, and the module-level name
  `createInMemory` is taken by the `LibraryProxy` builder — the step-004
  gotcha). An intrinsic extension in the same module reaches the
  module-private `validateSample` fine.
- A stateful proxy shared as a module-level test value would make the
  step-004 tests ORDER-DEPENDENT (xUnit guarantees no ordering) — every test
  now builds a fresh proxy; keep it that way when adding tests.
- `listSamples` / `searchSamples` now return Map (Guid) order, not seed
  order — the contract's `Sample list` carries no order guarantee and no test
  asserts one; anything needing display order should sort explicitly.
- The IMPLEMENT_CONTRACT worker prompt explicitly assigns the registry
  lifecycle update to the worker ("Update the contract's lifecycle in the
  registry to `implemented`"), unlike step 004 where the supervisor
  pre-recorded the declaration — `.contracts-json` edited in place, CRLF
  preserved, `implementStep: 5` recorded.
- `createInMemorySampleProxy` (the validate-only mock) is deleted; grep
  confirmed its only consumers were the step-004 tests. The blank-name
  `InvalidSample` validation is KEPT in the real store — dropping it would
  orphan the error case and break two pinned tests.
