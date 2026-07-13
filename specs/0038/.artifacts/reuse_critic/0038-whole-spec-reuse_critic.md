# Reuse critique -- .spec-md cycle 1

## Coverage

- Helper roots walked: `C:\GitHub\Berreman` (repo root)
- Files inspected: 200/200 — cap tripped inside `specs/*/.artifacts` round-trail `.md` files; product-level `.md`/`.json` files number 8 and were all seen; no `.py` files exist
- Extensions: `.py`, `.md`, `.json`
- Recorded interpretation: the diff under review is the spec itself (the committed `.spec-md`/`.spec-jsonl` plus the working-tree §0.2 claim-check amendment), so its "reinvented helpers" are the F# symbols its steps direct workers to build; the `.fs`-less extension bound cannot surface those, so every existing-helper citation below comes from a targeted read of the exact file the spec cites, verified line-by-line against the tree.

## Findings

The diff introduces no code; every finding is a step whose text directs (or leaves room for) a worker to rebuild something the tree already has, or leaves an existing equivalent unrouted so two copies survive the arc. This critique re-ran after the claim-check fix round; the §0.2 amendment changes no finding but sharpens F4.

### F1: The old search seams are left unrouted beside the new facet engine

- **Worker added:** steps 9/11 (the generic `Facets.fs` engine plus the material/library facet catalogues) and steps 21/22 (the in-place re-shapes of the registered `MaterialProxy` / `SampleProxy` surfaces, each enumerating its target fields).
- **Existing helper:** `DispersionFilter` / `MaterialQuery` / `MaterialProxy.searchMaterials` at `Berreman/OpticalConstructor/OpticalConstructor.Domain/MaterialLibrary.fs:471-514` with the pure engine `byQuery` at `:531-540`; `SampleQuery` / `SampleProxy.searchSamples` at `Berreman/OpticalConstructor/OpticalConstructor.Domain/ElementId.fs:287-326`.
- **Why it matters:** the new engine does the same job (text + category + dispersion filtering of materials; text + substrate filtering of samples). The operator sanctioned the net-new abstraction — `.manual/002-ui-standardization-comments.md:234-237` names these exact types as the ad-hoc predecessors — but no step says what happens to them. Steps 13/15 remove the bays that are their only consumers, and steps 21/22 enumerate the re-shaped proxy surfaces *without* `searchMaterials`/`searchSamples`, so a worker must guess between keeping dead fields on registered contracts (§0.2 re-types them in place) or silently dropping contract surface. Either way two filtering vocabularies (`MaterialQuery`/`SampleQuery` vs `FacetSelection`/`AppliedConstraint`) coexist in Domain with no step owning the reconciliation.
- **Suggested action:** amend steps 21/22 (or 11) to route the predecessors explicitly — retire `MaterialQuery`/`SampleQuery`/`DispersionFilter` and the two `search*` fields, or re-express them over `Facets.filter` — so the arc ends with one filtering vocabulary.

### F2: The Constant-vs-dispersive facet re-derives the private `hasDispersion` classifier

- **Worker added:** step 11's "Constant-vs-dispersive" material facet extractor, plus the dependent "Transparent-vs-absorbing offered for CONSTANT materials only" `appliesTo` rule, plus step 31's "constant materials never flag" gate.
- **Existing helper:** `hasDispersion : MaterialEntry -> bool` at `Berreman/OpticalConstructor/OpticalConstructor.Domain/MaterialLibrary.fs:520-523` — exactly this classification (any of `EpsWithDisp`/`MuWithDisp`/`RhoWithDisp` still a function case), currently `private` to the search seam.
- **Why it matters:** step 11 is scrupulous about reuse elsewhere ("the classes via `availableGyrationClasses` (`MaterialComplexityEditor.fs:373`), never re-derived") but silent here, and the helper is private — the path of least resistance is a second copy of the three-way match. Two "is this material dispersive" classifiers that can drift is precisely the divergence the facet counts, the constant-only `appliesTo` rule, and Part J's never-flag rule would then inherit.
- **Suggested action:** direct the worker to publish `hasDispersion` (or relocate it beside the facet catalogue) and consume it from the facet extractor, the step-31 diagnostic, and whatever survives of the search seam (F1).

### F3: The Library window's "By kind" representation duplicates the hand-built `LibraryTree`

- **Worker added:** step 15's `<UICOMP:LibraryWindow>` with "the seeded default representation 'By kind'" computed through the step-9 engine and the step-11 kind facet.
- **Existing helper:** `LibraryTreeNode` / `LibraryTree` at `Berreman/OpticalConstructor/OpticalConstructor.Domain/ElementId.fs:253-263`, the hand-built by-kind `seedTrees` at `:551-606`, served through the `LibraryProxy.libraryTrees` field at `:279`.
- **Why it matters:** step 15 removes the Library bay — the tree's consumer — yet cites `LibraryProxy` (`:276-281`, which includes `libraryTrees`) as the window's own seam. After the step, `seedTrees`/`libraryTrees`/`LibraryTree` remain a parallel, hand-maintained copy of exactly the grouping the engine now computes — including niceties the facet tree may not reproduce (the "same glass, different thickness" nesting under a shared material label, `:562-568`). No step retires the field, so the duplication sits on the contract the window itself consumes.
- **Suggested action:** route it in step 15 — retire `libraryTrees`/`LibraryTree`/`seedTrees` alongside the bay removal, or state which consumer keeps them and why two "By kind" trees may coexist.

### F4: Step 34's parsers split from the established text-parser family and mint a third `tryFloat`

- **Worker added:** step 34's pure intensity/ellipsometric CSV parsers, placed "in OpticalConstructor.Domain".
- **Existing helper:** the text-taking parser family in `OpticalConstructor.Storage` — `MaterialImport` (`MaterialImport.fs:26-58`, `inv`/`tryFloat` at `:39-44`, comma/space/tab row splitting at `:47-49`) and `SpectralImport.parseSpectrumCsv` (`SpectralImport.fs:18-69`), which already parses header-then-`X,Y` comma rows in invariant culture and *duplicates `tryFloat`/`inv` verbatim* at `:25-30`.
- **Why it matters:** the step cites `parseSpectrumCsv` only as a purity *precedent* while relocating the new parsers to a different project, so the family home fragments — Storage is where both existing parsers live and where step 36's file adapter lands anyway — and the already-twice-copied invariant-culture number parse gets a third hand-rolled copy, since Domain cannot reach Storage's private helpers (the reference points the other way). The amended §0.2 now explicitly names `MaterialImport.importCsv`/`exportCsv` (`:432`/`:459`), so the spec demonstrably knows the family and its home. The *semantics* genuinely differ (strict first-malformed-row typed errors vs `Seq.choose` row-skipping; explicitly no unit-from-header heuristics, which both existing parsers have), so a new parser body is defensible — the duplication is in the substrate and the placement, not the schema rules.
- **Suggested action:** either co-locate the new parsers with the family in `OpticalConstructor.Storage` (Domain keeps the series/error types and `validateAgainstExperiment`; step 35's proxy still declares in Domain), or extract the shared `tryFloat`/row-split substrate to one place all three parsers consume; at minimum have step 34 record why a separate Domain-side implementation is intentional.

### F5: `SceneSnapshot` risks a parallel per-element record beside `ElementPlacement`

- **Worker added:** step 26's `SceneSnapshot` — "elements with id, CatalogueKind, placement, zoom, and valueId binding; the view state; the snap flag".
- **Existing helper:** `ElementPlacement` at `Berreman/OpticalConstructor/OpticalConstructor.Domain/Placement.fs:178-192`, which already carries `catalogueKind` (`:187`) and `valueId` (`:188`) — also the shape the persisted 0026 project envelope serializes (`OpticalConstructor.Storage/schema/optical-constructor-project.schema.json:37-41`).
- **Why it matters:** the step's field list enumerates `CatalogueKind` and `valueId` *beside* "placement" even though the placement record already contains both, so a worker can read it as a mandate for a new parallel per-element record that must then be kept in sync with the live model. Lower severity than F1–F4 because the charitable reading ("placement" *means* `ElementPlacement`) is available and the proxy seam itself is an explicit operator decision (Q25). Note also the deliberate divergence worth one recorded sentence: the 0026 schema documents pan/zoom view state as ephemeral and NOT persisted (`schema.json:44`) while `SceneSnapshot` includes it.
- **Suggested action:** have step 26 name `ElementPlacement` as the element payload of `SceneSnapshot` and record the view-state divergence from the 0026 envelope as intentional.

For balance: outside these five the spec is unusually reuse-conscious — it pins `availableGyrationClasses`, `Propagation.inputStokes`/`analyzerMueller`, `EmbeddedChart`, `RhoWithDispValue.toRhoWithDisp`, `MuWithDispValue.toMuWithDisp`, the `createInMemory` precedents, the pending-bind-clears staleness precedent, and the `MaterialsControls` State+Handlers/AutomationId patterns by path and line; it *subsumes* `EditorLaunchers` rather than paralleling it (step 8); steps 25/31/34 build on the existing `Experiment`/`VariableRange` (`ElementId.fs:882-917`) and `EditSegment.interval` (`WaveLengthInterval`) rather than minting new range types; and Part N is itself a de-duplication sweep (single `RecentFiles` store, single `UiIds` module). The 11 new contract-registry entries follow the existing `XDUO` entry shape exactly.

## Bottom line

All five findings are routing/citation gaps in step text, not architectural duplication — the engine, proxies, and parsers are operator-sanctioned net-new; what is missing is an explicit fate for `searchMaterials`/`searchSamples`/`libraryTrees` (F1/F3), a publish-and-reuse directive for `hasDispersion` (F2), a placement/substrate decision for the step-34 parsers (F4), and one naming sentence in step 26 (F5). Folding those amendments into steps 11, 15, 21/22, 26, and 34 before workers reach Parts D/H/I/L would prevent every identified duplication at negligible cost. My read: substantive enough to warrant a spec touch-up, not substantive enough alone to force a re-spawn — the judge decides.
