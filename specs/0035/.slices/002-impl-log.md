# Step 002 — impl-log (ADD_CONTRACT STORE_XDUO_0003 CategoryProxy)

## Progress

- [done] Declared the category write-seam in
  `OpticalConstructor.Domain/MaterialLibrary.fs`, beside the step-1 category catalogue
  (immediately after `tryFindCategoryByName`, before `MaterialComplexity`):
  - `CategoryError` — errors-as-values DU, each case carrying `reason : string`:
    `UnknownCategoryId`, `DuplicateCategoryId`, `CategoryStillReferenced`,
    `BuiltInNotRemovable`, `InvalidCategory`. Mirrors `MaterialError` /
    `SampleError`, adding `BuiltInNotRemovable` for the refuse-a-built-in outcome.
  - `[<ReferenceEquality>] CategoryProxy` — a record of camelCase `Result`-returning
    functions: `listCategories : unit -> Result<MaterialCategory list, CategoryError>`,
    `addCategory : MaterialCategory -> Result<unit, CategoryError>`,
    `updateCategory : MaterialCategory -> Result<unit, CategoryError>`,
    `removeCategory : CategoryId -> Result<unit, CategoryError>`. Same functional-proxy
    convention as `MaterialProxy` / `SampleProxy` / `LibraryProxy`; reference equality
    keeps an Elmish host that holds one comparable by identity.
  - `validateCategory : MaterialCategory -> Result<unit, CategoryError>` — the
    `validateEntry` precedent: a blank/whitespace `name` is `InvalidCategory`;
    not `private` (a future store augmentation in another file must reach it).
- [done] Added the mock-driven `OpticalConstructor.Tests/CategoryProxyTests.fs`: a STUB
  `CategoryProxy` over a FIXED category list (built-in glass, hidden built-in vacuum, a
  minted user category) exercises all four functions through their EXACT signatures —
  every `CategoryError` outcome (`InvalidCategory`, `DuplicateCategoryId`,
  `UnknownCategoryId`, `BuiltInNotRemovable`, `CategoryStillReferenced`) plus the
  reference-equality-by-identity check. A dedicated third stub isolates
  `CategoryStillReferenced` on a referenced user category.
- [done] Registered `CategoryProxyTests.fs` in `OpticalConstructor.Tests.fsproj`
  (after `SampleProxyTests.fs`).
- [done] Compile-verified the whole solution and executed the new test file.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/MaterialLibrary.fs` — the
  declared contract (`CategoryError`, `CategoryProxy`, `validateCategory`), +43 lines,
  0 deletions.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/CategoryProxyTests.fs` — **new**
  mock-driven test.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj` —
  registered the new test file in compile order, +4 lines.

## Testing state

- **Build (minimum diligence, not a self-reported gate):**
  `dotnet build Berreman.slnx -c Release -nologo -v:m` → **0 errors**. The 94 warnings
  in the log are all pre-existing and outside this slice: `MSB3277` (WindowsBase
  4.0.0.0 vs 5.0.0.0 in the test/app projects — a reference-graph conflict predating
  this slice; per `CLAUDE.md` this is ours to fix, but it is not introduced by this
  change and touching the reference graph is out of an ADD_CONTRACT slice's scope) and
  `FS1125` in the untouched `SeriesDataTests.fs`. No warning originates in
  `MaterialLibrary.fs` or `CategoryProxyTests.fs`.
- **New test executed (TDD diligence — a new test that does not pass is worse than no
  test):** `dotnet test … --no-build --filter "FullyQualifiedName~CategoryProxyTests"` →
  **3 passed, 0 failed**.
- **Gates:** Per the ADD_CONTRACT worker's Invariant 6 ("you act; you run no checks — gate
  execution belongs to the arc-runner's deterministic gate engine, which runs the step's
  gates after you exit"), I did NOT run the `unit-tests` / `constructor-unit-tests` /
  `ui-smoke` / `ui-tests` roster as gates and do not self-report their results. The build
  compile-check and the single new-test run above are the minimum diligence to avoid
  shipping a non-building contract or a test that does not pass; the gate engine is the
  sole gate authority. This slice only ADDS a test file (never removes one), so no
  `count_at_least` baseline can regress.

## Gotchas

- **Placement chosen: beside the step-1 catalogue, not beside `MaterialProxy`.** The
  how-to says "beside the step-1 category catalogue" and cites the `MaterialProxy`
  convention only as the *shape* to follow. I placed `CategoryError` / `CategoryProxy` /
  `validateCategory` right after `tryFindCategoryByName` (the last catalogue helper),
  before `MaterialComplexity`. Everything the contract needs (`CategoryId`,
  `MaterialCategory`) is declared above that point, so the file still type-checks
  top-to-bottom.
- **DECLARED, not IMPLEMENTED.** This is an ADD_CONTRACT step: it ships the interface +
  mock + test only. The real stateful store (`CategoryProxy.createInMemory`, a type
  augmentation like `MaterialProxy.createInMemory` in `ElementId.fs`) is deliberately
  NOT written here — it belongs to the later `IMPLEMENT_CONTRACT STORE_XDUO_0003` step.
  The doc comments name that follow-up so the seam reads as intentionally declared.
- **`BuiltInNotRemovable` is a net-new case vs. the `MaterialProxy` precedent.**
  `MaterialError` has no built-in-guard case (materials are all seeded but removable if
  unreferenced); the category contract adds `BuiltInNotRemovable` because a
  `BuiltInCategory` (Glass/Metal/…/Vacuum) must never be deleted — only a `UserCategory`
  may be removed. The stub encodes that guard (`origin` match) so the case is reachable
  through `removeCategory`'s exact signature. Because every seeded catalogue category is
  `BuiltInCategory`, the built-in guard shadows `CategoryStillReferenced` for the seeds;
  a third dedicated stub (a referenced `UserCategory`) exercises that remaining case.
- **Mock reuses the real seeded records.** The stub's fixed list draws `glass` and
  `vacuum` from `standardCategories` (via `CategoryIds`) rather than fabricating
  categories, so the mock stays faithful to the actual step-1 catalogue identities.

## Changelog

- 2026-07-07 — Declared the mutating category write-seam (`CategoryError` +
  `[<ReferenceEquality>] CategoryProxy` + `validateCategory`) beside the step-1 catalogue
  in `MaterialLibrary.fs`, and added a mock-driven `CategoryProxyTests.fs` (stub over a
  fixed category list) exercising all four functions through their exact signatures and
  every error case. Solution builds clean (0 errors); the new test file runs 3 passed.
