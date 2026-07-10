# Reuse critique -- .spec-md cycle 1

## Coverage

- Helper roots walked: `C:\GitHub\Berreman` (repo root)
- Files inspected: 200/200 (cap tripped inside `specs/*/.artifacts` round-trail `.md` files; product-level `.md`/`.json` files number ~10 and were all seen)
- Extensions: `.py`, `.md`, `.json` (no `.py` files exist)
- Recorded interpretation: the diff under review is a spec (`.spec-md`, `.spec-jsonl`, contract-id registry), so its "reinvented helpers" are the F# symbols its steps direct workers to build. The `.fs`-less extension bound cannot surface those, so existing-helper citations below come from targeted reads of the exact files the diff itself cites — without them no finding could cite a path, which the rubric requires.

## Findings

The diff introduces no code; every finding is a step whose text directs (or permits) a worker to rebuild something the tree already has, or leaves an existing equivalent unrouted so duplication survives the arc.

### F1: The old facet-search seams are left unrouted beside the new facet engine

- **Worker added:** steps 9/11 (generic `Facets.fs` engine + material/library facet catalogues) and steps 21/22 (re-shape of the registered `MaterialProxy` / `SampleProxy` surfaces "in place", enumerating the target fields).
- **Existing helper:** `MaterialQuery` / `DispersionFilter` / `MaterialProxy.searchMaterials` at `Berreman/OpticalConstructor/OpticalConstructor.Domain/MaterialLibrary.fs:471-514` (pure engine `byQuery` at `:531-539`); `SampleQuery` / `SampleProxy.searchSamples` at `Berreman/OpticalConstructor/OpticalConstructor.Domain/ElementId.fs:283-326`.
- **Why it matters:** the new engine does the same job (text + category + dispersion filtering of materials; text + substrate filtering of samples). The operator sanctioned a net-new abstraction (`.manual/002-ui-standardization-comments.md:234-237` names these exact types as the ad-hoc predecessors), but the spec never says what happens to them. Steps 13/15 remove the bays that are their only consumers, and steps 21/22 enumerate the re-shaped proxy surfaces *without* `searchMaterials`/`searchSamples` — so a worker must guess between keeping dead fields on registered contracts (§0.2 re-types them in place) or silently dropping contract surface. Either way, two filtering vocabularies (`MaterialQuery`/`SampleQuery` vs `FacetSelection`/`AppliedConstraint`) coexist in Domain with no step owning the reconciliation.
- **Suggested action:** amend steps 21/22 (or 11) to route the predecessors explicitly — retire `MaterialQuery`/`SampleQuery` and the two `search*` fields, or re-implement them over `Facets.filter` — so the arc ends with one filtering vocabulary.

### F2: The Constant-vs-dispersive facet re-derives the private `hasDispersion` classifier

- **Worker added:** step 11's "Constant-vs-dispersive" material facet extractor (and the dependent "Transparent-vs-absorbing offered for CONSTANT materials only" `appliesTo` rule).
- **Existing helper:** `hasDispersion : MaterialEntry -> bool` at `Berreman/OpticalConstructor/OpticalConstructor.Domain/MaterialLibrary.fs:520-523` — the exact wavelength-dependence classification (any of `EpsWithDisp`/`MuWithDisp`/`RhoWithDisp` still a function case), currently `private` to the search seam.
- **Why it matters:** step 11 is scrupulous about reuse elsewhere ("the classes via `availableGyrationClasses` (`MaterialComplexityEditor.fs:373`), never re-derived") but is silent here, and the helper is private — the path of least resistance for a worker is a second copy of the three-way match. Two classifiers of "is this material dispersive" that can drift is precisely the divergence risk the facet counts and the Part J "constant materials never flag" rule then inherit.
- **Suggested action:** direct the worker to publish `hasDispersion` (or relocate it beside the facet catalogue) and consume it from both the facet extractor and whatever survives of the search seam (F1).

### F3: The Library window's "By kind" representation duplicates the hand-built `LibraryTree`

- **Worker added:** step 15's `<UICOMP:LibraryWindow>` with "the seeded default representation 'By kind'" built through the step-9 engine and step-11 kind facet.
- **Existing helper:** `LibraryTree` / `LibraryTreeNode` at `Berreman/OpticalConstructor/OpticalConstructor.Domain/ElementId.fs:255-263`, the hand-built "By kind" `seedTrees` at `:551-606`, served through the `LibraryProxy.libraryTrees` field at `:279`.
- **Why it matters:** after step 15 removes the Library bay (the tree's consumer), `seedTrees`/`libraryTrees`/`LibraryTree` remain a parallel, hand-maintained representation of exactly the grouping the new engine now computes — including niceties the facet tree may not reproduce (the "same glass, different thickness" nesting under a shared material, `:562-568`). No step retires the field, and `LibraryProxy` is the contract the Library window itself consumes, so the duplication sits on the window's own seam.
- **Suggested action:** route it in step 15 — either retire `libraryTrees`/`LibraryTree`/`seedTrees` alongside the bay removal, or state which consumer keeps them and why the two "By kind" trees are allowed to coexist.

### F4: Step 34's parsers diverge from the established text-parser family and mint a third `tryFloat`

- **Worker added:** step 34's pure intensity/ellipsometric CSV parsers, placed "in OpticalConstructor.Domain".
- **Existing helper:** the text-taking parser family in `OpticalConstructor.Storage` — `MaterialImport` (`MaterialImport.fs:26-58`, with `inv`/`tryFloat` at `:39-44` and comma/space row splitting at `:47-49`) and `SpectralImport.parseSpectrumCsv` (`SpectralImport.fs:18-69`), which already parses header-then-`X,Y` comma rows in invariant culture and *duplicates `tryFloat`/`inv` verbatim* at `:25-30`.
- **Why it matters:** the step cites `parseSpectrumCsv` only as a purity *precedent* while relocating the new parsers to a different project, so the family home (Storage, where both existing parsers live and where step 36's file adapter lands anyway) fragments, and the already-twice-copied invariant-culture number parse gets a third hand-rolled copy — Domain cannot reach Storage's private helpers or its `FSharp.Data.CsvFile` dependency. The *semantics* genuinely differ (strict first-malformed-row errors vs `Seq.choose` skipping; explicitly no unit-from-header heuristics), so a new parser body is defensible — the duplication is in the substrate and the placement, not the schema rules.
- **Suggested action:** either co-locate the new parsers with the existing family in `OpticalConstructor.Storage` (Domain keeps the series/error types and `validateAgainstExperiment`; step 35's proxy fields still declare in Domain), or extract the shared `tryFloat`/row-split substrate to one place all three parsers consume; at minimum have step 34 record why a separate Domain-side implementation is intentional.

### F5: `SceneSnapshot` risks a parallel per-element record beside `ElementPlacement`

- **Worker added:** step 26's `SceneSnapshot` — "elements with id, CatalogueKind, placement, zoom, and valueId binding; the view state; the snap flag".
- **Existing helper:** `ElementPlacement` at `Berreman/OpticalConstructor/OpticalConstructor.Domain/Placement.fs:178-208` (already carrying `valueId` at `:188`), which is also the shape the persisted project envelope's `placements` array serializes (`OpticalConstructor.Storage/schema/optical-constructor-project.schema.json:37-45`, spec 0026).
- **Why it matters:** the step's field list re-describes `ElementPlacement` almost verbatim without naming it, so a worker can mint a parallel per-element record that must then be kept in sync with the live model's placements. Lower severity than F1-F4 because the charitable reading ("placement" *means* the record) is available and the separate proxy seam is an explicit operator decision (Q25); note also the deliberate divergence that the project schema excludes pan/zoom as ephemeral while `SceneSnapshot` includes view state — worth one recorded sentence so the future storage cycle doesn't read it as an accident.
- **Suggested action:** have step 26 name `ElementPlacement` as the element payload of `SceneSnapshot` and record the view-state divergence from the 0026 project envelope as intentional.

For balance: outside these five, the spec is unusually reuse-conscious — it pins `availableGyrationClasses`, `Propagation.inputStokes`/`analyzerMueller`, `EmbeddedChart`, `RhoWithDispValue.toRhoWithDisp`, `MuWithDispValue.toMuWithDisp`, the `createInMemory` precedents, and the `MaterialsControls` State+Handlers/AutomationId patterns by path and line; it *subsumes* `EditorLaunchers` rather than paralleling it (step 8); and Part N is itself a de-duplication sweep (single `RecentFiles` store, single `UiIds` module). The 11 new contract-id registry entries follow the existing `XDUO-json` entry shape exactly.

## Bottom line

All five findings are routing/citation gaps in step text, not architectural duplication — the engine, proxies, and parsers are operator-sanctioned net-new; what is missing is an explicit fate for `searchMaterials`/`searchSamples`/`libraryTrees`, a publish-and-reuse directive for `hasDispersion`, a placement/substrate decision for the step-34 parsers, and one naming sentence in step 26. Folding those amendments into steps 11, 15, 21/22, 26, and 34 before workers reach Parts D/H/I/L would prevent every identified duplication at negligible cost. My read: substantive enough to warrant a spec touch-up, not a re-spawn — the judge decides.
