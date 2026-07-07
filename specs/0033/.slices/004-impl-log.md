# Slice 004 — impl log

## Progress

- [x] Recon: read task file, `add_contract_worker.system-md`,
      `arc-runner.system-md`, project prompt, slice 004, slice 003
      (impl-log/SoW — the MaterialProxy precedent this step mirrors),
      `ElementId.fs` (Library module: Sample/SampleId/SubstrateKind/SeedSamples,
      LibraryProxy convention), `MaterialLibrary.fs` (step-003 block),
      `MaterialProxyTests.fs`, the Tests fsproj, `004.gates`,
      `.contracts-json` (STORE_XDUO_0002 pre-registered as `declared`).
- [x] Domain/ElementId.fs: after `LibraryProxy`, the step-004 surface —
      `SampleQuery` record (text / substrate option, plus a `static member empty`
      match-everything query mirroring `MaterialQuery.empty`); `SampleError`
      (UnknownSampleId | DuplicateSampleId | InvalidSample, each `of reason : string`);
      `[<ReferenceEquality>] SampleProxy` with the six `Result`-returning fields
      exactly per the slice signature. At the end of the `Library` module (after
      `createInMemory`, which the mock must follow because it closes over
      `SeedSamples.all`): a private `validateSample` (blank name ⇒ `InvalidSample`)
      and the mock `createInMemorySampleProxy () : SampleProxy` — an inline stub
      record over the FIXED `SeedSamples.all` list: reads answer from the list
      (search = case-insensitive name fragment + substrate facet); writes VALIDATE
      and return the typed outcome WITHOUT persisting (contract stays `declared`;
      the real store is the later IMPLEMENT_CONTRACT step). Distinct mock name
      because `Library.createInMemory` (the `LibraryProxy` mock) already exists —
      a duplicate module-level `let` is a compile error.
- [x] Tests/SampleProxyTests.fs (new, registered after MaterialProxyTests.fs):
      mock-driven tests over `createInMemorySampleProxy` (list distinct/complete;
      search by empty query / text case-insensitive / substrate facet incl. the
      empty Wedge result / composed facets; tryGet hit+miss; add fresh Ok /
      duplicate / blank name; update known Ok / unknown / blank name; remove
      known Ok / unknown), a reference-equality pin, and THE acceptance test: a
      stub `SampleProxy` built inline over a fixed two-sample list exercising all
      six functions through their exact signatures — every `SampleError` case
      surfaces through the stub.
- [x] Local verification (advisory — the arc-runner gate engine re-runs the
      authoritative gates after exit): build 0 errors (`--warnaserror+:25`
      clean, no warnings from touched files; the MSB3277/FS1125 warnings are
      pre-existing in other projects); BerremanTests 84 passed / 5 skipped
      (pre-existing skips); OpticalConstructor.Tests 339 passed (+14, none
      removed); ui-smoke 54 passed; ui-tests 249 passed. Capture:
      `specs/0033/.artifacts/004-local-verify.log`.
- [x] LF policy: `git diff --numstat` identical with and without
      `--ignore-cr-at-eol`; zero CR characters in every touched/new file.
- [x] Contract registry: `specs/0033/.contracts-json` already records
      `STORE_XDUO_0002` as `declared` / `declaringStep: 4`
      (supervisor-maintained) — nothing for the worker to write.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/ElementId.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/SampleProxyTests.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj`

## Testing state

All five suites pass locally (advisory run): build clean (0 errors,
`--warnaserror+:25` respected), unit-tests 84 passed / 5 skipped (pre-existing
skips), constructor-unit-tests 339 passed (+14 added, none removed), ui-smoke
54 passed, ui-tests 249 passed. Every `count_at_least` baseline holds or grows
(constructor 325 → 339). No blockers; every requirement of the slice landed
this round (the contract stays `declared` by design — ADD_CONTRACT ships the
surface, mock, and test only).

## Artifacts

- `specs/0033/.artifacts/004-local-verify.log` — captured build/test outcomes
  for all five gates (advisory).

## Gotchas

- **The mock is named `createInMemorySampleProxy`, not `createInMemory`.**
  The `Library` module already binds `createInMemory` (the `LibraryProxy`
  mock), and a duplicate module-level `let` is a compile error (FS0037) —
  unlike step 003, where `MaterialProxy`'s mock lived in its own module and
  could reuse the conventional name.
- **"Beside Library" placed the surface INSIDE the `Library` module**
  (`ElementId.fs`), mirroring step 003's "beside MaterialLibrary" precedent
  (which landed inside the `MaterialLibrary` module): the types sit right
  after `LibraryProxy` so the module's proxy seams read together; the mock
  sits at the END of the module because it closes over `SeedSamples.all`,
  which is declared after the proxy types.
- **`SampleError` is a NEW DU, not an in-place extension** (unlike step 003's
  `MaterialError`), so no existing exhaustive match anywhere could break — no
  TestWindows-style collateral edit was needed this round; `touches` was
  honoured exactly.
- **The mock's write functions persist nothing** (an inline stub over the
  fixed `SeedSamples.all` list, per the slice text): they validate against the
  fixed list and return the typed outcome (`DuplicateSampleId` on a seeded id,
  `InvalidSample` on a blank name, `UnknownSampleId` on update/remove of an
  unknown id). This keeps the contract at `declared` lifecycle — no real
  behaviour was built.
- **Every `SampleError` case surfaces through both the mock and the test
  stub** — there is no `MaterialStillReferenced`-style case that only a stub
  can produce, so the acceptance stub routes `removeSample` of an unknown id
  to `UnknownSampleId` instead.
- **`SampleQuery.text` is a plain `string`** — dictated by the slice signature
  verbatim, consistent with step 003's `MaterialQuery.text` and with
  `Sample.name` (display/search text is not elevated anywhere in this domain).
- **Search matches the sample NAME only** (case-insensitive fragment, empty
  matches all) — mirroring the `MaterialProxy` mock's `byNameContains`
  semantics; the slice does not ask for description search.
- Advisory-verification vs the family prompt's Invariant 6 ("act only, run no
  checks"): the local build/test run is recorded as ADVISORY only — the
  deterministic gate engine remains the sole gate authority. This follows the
  accepted slice-003 precedent, and the SoW's required `gates:` YAML baseline
  block needs real counts.
- `specs/0033/.manifest.state.json` carries CRLF but is the arc-runner's own
  file (already modified before this session started) — left alone, same note
  as slices 001–003.
- Operator note: the project prompt's "Operator note" section is present but
  empty — no operator constraints in flight this attempt.
