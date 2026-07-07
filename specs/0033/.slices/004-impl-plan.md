# Slice 004 — impl plan

## Goal

ADD_CONTRACT STORE_XDUO_0002 `SampleProxy` (kind proxy, DECLARED lifecycle): the
mutating samples write-seam in `OpticalConstructor.Domain`, beside the `Library`
module, mirroring slice 003's `MaterialProxy` shape exactly — plus an in-memory
mock and a mock/stub-driven test suite in `OpticalConstructor.Tests` exercising
all six functions through their exact signatures.

## Approach

1. **`Berreman/OpticalConstructor/OpticalConstructor.Domain/ElementId.fs`**
   (the `Library` module — where `Sample`, `SampleId`, `SubstrateKind`, and
   `SeedSamples` live; "beside Library" per the slice, following the step-003
   precedent of "beside MaterialLibrary" landing inside that module):
   - `SampleQuery` record — `text : string` (case-insensitive name fragment,
     empty matches all) + `substrate : SubstrateKind option`, with a
     match-everything `static member empty` (mirrors `MaterialQuery.empty`).
   - `SampleError` DU — `UnknownSampleId | DuplicateSampleId | InvalidSample`,
     each `of reason : string`.
   - `[<ReferenceEquality>] SampleProxy` with the six `Result`-returning fields
     exactly per the slice signature (`listSamples`, `searchSamples`,
     `tryGetSample`, `addSample`, `updateSample`, `removeSample`).
   - The types go right after `LibraryProxy` (so the module's proxy seams sit
     together); the mock goes at the end of the module after `createInMemory`
     because it closes over `SeedSamples.all`.
   - Mock `createInMemorySampleProxy () : SampleProxy` — an inline stub record
     over the FIXED `SeedSamples.all` list (the name is distinct because
     `Library.createInMemory` already builds the `LibraryProxy`): reads answer
     from the list; writes validate (blank name ⇒ `InvalidSample`; duplicate id
     ⇒ `DuplicateSampleId`; unknown id on update/remove ⇒ `UnknownSampleId`)
     and return the typed outcome WITHOUT persisting — the contract stays
     `declared`; the real store is the later IMPLEMENT_CONTRACT step.

2. **`OpticalConstructor.Tests/SampleProxyTests.fs`** (new, registered after
   `MaterialProxyTests.fs`): mirror `MaterialProxyTests` — read-surface tests
   over the fixed seed list (list distinct/complete; search by empty query /
   text case-insensitive / substrate facet / composed facets; tryGet hit+miss),
   write-surface tests (add fresh minted Ok / duplicate / blank name; update
   known Ok / unknown / blank name; remove known Ok / unknown), the
   reference-equality pin, and THE acceptance test: a stub `SampleProxy` built
   inline over a fixed two-sample list exercising all six functions through
   their exact signatures.

3. **Registry** — `specs/0033/.contracts-json` already records
   `STORE_XDUO_0002` as `declared` / `declaringStep: 4` (supervisor-maintained);
   nothing for the worker to write.

## Risks

- `SampleError` is a NEW DU (unlike step 003's in-place `MaterialError`
  extension), so no existing exhaustive match anywhere can break — no
  TestWindows collateral expected this round.
- Name shadowing inside `Library`: `createInMemory` is taken; the sample mock
  must use a distinct name (duplicate module-level `let` is a compile error).
- F# FS0505 trap on proxy equality assertions — use double parens
  (`Assert.False((p = q))`), per the slice-003 gotcha.
- LF-only endings on every touched file.
