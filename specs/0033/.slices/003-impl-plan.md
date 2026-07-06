# Slice 003 — impl plan

## Goal

ADD_CONTRACT `STORE_XDUO_0001` (`MaterialProxy`, kind `proxy`, lifecycle
`declared`): declare the mutating materials write-seam in
`OpticalConstructor.Domain` beside `MaterialLibrary`, ship a mock (an inline
stub record over a fixed entry list) and a mock-driven test in
`OpticalConstructor.Tests` that builds a stub `MaterialProxy` and exercises
all six functions through their exact signatures. No real (persisting)
behaviour — that is the later `IMPLEMENT_CONTRACT STORE_XDUO_0001` step.

## Approach

1. **Domain/MaterialLibrary.fs** — extend `MaterialError` in place with
   `DuplicateMaterialId`, `MaterialStillReferenced`, `InvalidMaterial`, each
   carrying `reason : string` (the existing `UnknownMaterialId of reason`
   stays). After `standard`, add the step-003 block:
   - `DispersionFilter = AnyDispersion | OnlyDispersive | OnlyNonDispersive`;
   - `MaterialQuery = { text : string; category : MaterialCategory option;
     dispersion : DispersionFilter }` (+ a `static member empty`
     match-everything query, mirroring `ExperimentDraft.empty`);
   - `[<ReferenceEquality>] MaterialProxy` with the six `Result`-returning
     camelCase function fields exactly as the slice dictates, following the
     `LibraryProxy` convention (`ElementId.fs:237-243`);
   - a private `hasDispersion : MaterialEntry -> bool` (pattern-match on the
     `*WithDisp` cases — an entry is dispersive when any component carries a
     wavelength function);
   - `createInMemory () : MaterialProxy` — the mock: reads answer from the
     FIXED `standard.entries` list (search reuses `byNameContains` + linear
     category/dispersion filters); the write functions VALIDATE against that
     fixed list and return the typed outcome WITHOUT persisting (add rejects a
     duplicate id, add/update reject a blank name via `InvalidMaterial`,
     update/remove reject an unknown id).
2. **TestWindows/TableAndElementRotationView.fs** — `materialErrorText` is an
   exhaustive single-case match on `MaterialError`; extend it to render the
   three new cases (all carry `reason`) or the solution build breaks under
   `--warnaserror+:25` (FS0025). Collateral edit outside the declared
   `touches` — recorded in Gotchas.
3. **Tests/MaterialProxyTests.fs** (new, registered in the fsproj after
   `PropagationTests.fs`) — mock-driven tests over
   `MaterialLibrary.createInMemory ()` covering list/search (text, category,
   dispersion facets)/tryGet/add/update/remove and the typed errors, PLUS the
   acceptance test: build a stub `MaterialProxy` inline over a fixed entry
   list and exercise all six functions through their exact signatures
   (including `MaterialStillReferenced`, which only a stub can surface — the
   domain mock cannot see the seeded samples' references, compile order).
4. **Contract registry** — `specs/0033/.contracts-json` already records
   `STORE_XDUO_0001` as `declared` at step 3 (supervisor-maintained); no edit.

## Risks

- FS0025 exhaustive-match breakage on `MaterialError` consumers — only ONE
  production match site exists (`materialErrorText`); tests all use catch-all
  `| other ->` arms. Handled in step 2.
- `MaterialEntry` holds `OpticalPropertiesWithDisp` closures — assertions
  compare ids/names, never whole-entry structural equality (slice 002
  precedent).
- Line endings: keep LF (`git diff --numstat` vs `--ignore-cr-at-eol`).
- Gates: build + all four test suites run locally as ADVISORY only (Invariant
  6 — the arc-runner's gate engine is the sole gate authority after exit).
