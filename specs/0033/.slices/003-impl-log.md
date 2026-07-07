# Slice 003 — impl log

## Progress

- [x] Recon: read slice, task file, `add_contract_worker.system-md`,
      `arc-runner.system-md`, project prompt, `MaterialLibrary.fs`,
      `ElementId.fs` (LibraryProxy convention), `Dispersion.fs`
      (`OpticalPropertiesWithDisp` shape), `ExperimentProxyTests.fs` /
      `LibraryProxyTests.fs` / `DispersionModelsTests.fs` (test conventions),
      gates snapshot, slice-002 impl-log/plan/SoW (precedent),
      `.contracts-json` (STORE_XDUO_0001 pre-registered as `declared`).
- [x] Domain/MaterialLibrary.fs: `MaterialError` extended in place with
      `DuplicateMaterialId` / `MaterialStillReferenced` / `InvalidMaterial`
      (each `of reason : string`); after `standard`, the step-003 block:
      `DispersionFilter` (AnyDispersion | OnlyDispersive | OnlyNonDispersive),
      `MaterialQuery` record (text / category option / dispersion, plus a
      `static member empty` match-everything query mirroring
      `ExperimentDraft.empty`), `[<ReferenceEquality>] MaterialProxy` with the
      six `Result`-returning fields exactly per the slice signature, a private
      `hasDispersion` (pattern-match on the `*WithDisp` cases), a private
      `validateEntry` (blank name ⇒ `InvalidMaterial`), and the mock
      `createInMemory () : MaterialProxy` — an inline stub record over the
      FIXED `standard.entries`: reads answer from the list (search reuses
      `byNameContains` + linear category/dispersion filters); writes VALIDATE
      and return the typed outcome WITHOUT persisting (contract stays
      `declared`; the real store is the later IMPLEMENT_CONTRACT step).
- [x] TestWindows/TableAndElementRotationView.fs: `materialErrorText` extended
      to render the three new cases (or-pattern over their `reason`) — the
      match was exhaustive-single-case and would break the solution build
      under `--warnaserror+:25` (FS0025). Outside declared `touches` — see
      Gotchas.
- [x] Tests/MaterialProxyTests.fs (new, registered after PropagationTests.fs):
      mock-driven tests over `createInMemory` (list distinct/complete; search
      by empty query / text case-insensitive / category / dispersion facets /
      composed facets; tryGet hit+miss; add fresh Ok / duplicate / blank name;
      update known Ok / unknown / blank name; remove known Ok / unknown),
      a reference-equality pin, and THE acceptance test: a stub
      `MaterialProxy` built inline over a fixed two-entry list exercising all
      six functions through their exact signatures — including
      `MaterialStillReferenced`, which only a stub can surface (the domain
      mock cannot see the seeded samples' references; compile order puts
      `ElementId.fs` after `MaterialLibrary.fs`).
- [x] Local verification (advisory — the arc-runner gate engine re-runs the
      authoritative gates after exit): build 0 errors (`--warnaserror+:25`
      clean, no warnings from touched files); BerremanTests 84 passed /
      5 skipped (pre-existing skips); OpticalConstructor.Tests 325 passed
      (+15, none removed); ui-smoke 54 passed; ui-tests 249 passed. Capture:
      `specs/0033/.artifacts/003-local-verify.log`.
- [x] LF policy: `git diff --numstat` identical with and without
      `--ignore-cr-at-eol`; the new files are LF.
- [x] Contract registry: `specs/0033/.contracts-json` already records
      `STORE_XDUO_0001` as `declared` / `declaringStep: 3`
      (supervisor-maintained) — nothing for the worker to write.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/MaterialLibrary.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.TestWindows/TableAndElementRotationView.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/MaterialProxyTests.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj`

## Testing state

All five suites pass locally (advisory run): build clean (0 errors,
`--warnaserror+:25` respected), unit-tests 84 passed / 5 skipped
(pre-existing skips), constructor-unit-tests 325 passed (+15 added, none
removed), ui-smoke 54 passed, ui-tests 249 passed. Every `count_at_least`
baseline holds or grows. No blockers; every requirement of the slice landed
this round (the contract stays `declared` by design — ADD_CONTRACT ships the
surface, mock, and test only).

## Artifacts

- `specs/0033/.artifacts/003-local-verify.log` — captured build/test outcomes
  for all five gates (advisory).

## Gotchas

- **TestWindows touched outside the slice's declared `touches`.**
  `materialErrorText` (`TableAndElementRotationView.fs`) was an exhaustive
  single-case match on `MaterialError`; the three new cases made it
  incomplete, which is a build ERROR under `--warnaserror+:25` (FS0025).
  Extended it with an or-pattern rendering the write-seam cases' `reason` —
  the minimal collateral edit that keeps the `build` gate green.
- **`MaterialStillReferenced` is exercised only through the test-local stub.**
  The domain mock closes over the FIXED `standard.entries` and cannot see the
  seeded samples' layer references — compile order puts `ElementId.fs`
  (where `SampleStructure` lives) AFTER `MaterialLibrary.fs`. Reference
  tracking is the real store's job (IMPLEMENT_CONTRACT); the stub in
  `MaterialProxyTests` returns the case with a diagnostic reason, pinning its
  shape.
- **The mock's write functions persist nothing** (an inline stub over a fixed
  entry list, per the slice text): they validate against the fixed list and
  return the typed outcome (`DuplicateMaterialId` on an id the library holds,
  `InvalidMaterial` on a blank name, `UnknownMaterialId` on update/remove of
  an unknown id). This keeps the contract at `declared` lifecycle — no real
  behaviour was built.
- **`Assert.True(p = same)` needs double parens in F#** —
  `Assert.False(p = createInMemory ())` parses `p = …` as a named-argument
  assignment (FS0505). Written `Assert.False((p = …))`.
- **`MaterialQuery.text` is a plain `string`** — dictated by the slice
  signature verbatim; consistent with `MaterialEntry.name`/`description`
  (display/search text is not elevated anywhere in this domain).
- `specs/0033/.manifest.state.json` carries CRLF but is the arc-runner's own
  file — left alone (same note as slices 001/002).
- Operator note: the project prompt's "Operator note" section is present but
  empty — no operator constraints in flight this attempt.
