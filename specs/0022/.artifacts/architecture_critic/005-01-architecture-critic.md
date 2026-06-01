# Architecture critique -- 005.slice-md cycle 1

## Summary

Clean, well-layered work with one judgment call the judge should weigh: the slice delivers the Avalonia-free edit/page *cores* and explicitly drops the FuncUI *view/shell* layer, justifying it under the §0/P3 testability mandate and a FuncUI 1.6 DSL build risk. The pure cores are faithful to the reused engine seams — no forked solver, immutable record-update edits, schema `$defs` that match the real serialized shape — so AC-B1/B5/B6/B7/B9/B10 are each exercised headlessly. The single most important finding is the spec-fit question of whether "P3" legitimately carves the *entire* render layer out of R-2/R-3/R-4/R-6's "MUST provide …" language.

## Layering

Clean. `StackEditor.fs` and `ConstructionPage.fs` sit in `OpticalConstructor.Ui` and depend only downward — on the engine (`Berreman.Media`/`Fields`/`Solvers`), the slice-002 domain (`BeamTree`/`Project`/`Units`), and `OpticalConstructor.Storage` for save. The Ui→Storage edge is an established project reference (`OpticalConstructor.Ui.fsproj:50`) and `saveProject`/`projectFilePath` calling `ProjectJson.serializeProject` is exactly the §B.9 / R-6 item 6 seam, not a layering shortcut. No reverse edge, no engine fork: `solveNode = BeamTree.solve = OpticalSystemSolver(node.incident, node.system).solution.emSys` (`BeamTree.fs:91`), so AC-B1's "no forked solver in the domain" holds by construction.

## Separation of concerns

Good. The edit logic (`applyStackMsg`), the page reducer (`update`), node addressing (`NodePath`/`tryGetNode`/`updateNodeAt`), and the IO seam (`saveProject`) are cleanly separated, and every transform is a pure `OpticalSystem -> OpticalSystem` or `Msg -> Model -> Model`. One soft note: the solve is a side effect not modeled as a command — `editNode` sets `busy = subtreePaths …` but `update` never triggers the re-solve; the host must observe the busy set, call `solveSubtree`, and dispatch `NodeSolved`. The tests do this manually (`StackEditTests.fs:232-233`). This is a legitimate Elmish split, but the update→host contract is implicit, so a host that forgets the solve leaves a node `busy` forever. A one-line doc-comment on `editNode`/`Msg` naming the required follow-up dispatch would make the seam self-describing.

## Consistency

Strong. The schema `$defs` were captured empirically before authoring and model the real FSharp.SystemTextJson output — `children` as an array of `[branch, node]` pairs (`prefixItems`), `Case`/`Fields` adjacent tags for `Thickness`/`substrate`/`waveLength`, bare strings for fieldless DUs — and stay permissive on the un-owned `opticalProperties`/`constructorElement` anchors (left as the slice-003 `true`), so validate-on-load won't reject a legitimate document. `SubstrateChoice` (`AsThinFilm`/`AsPlate`/`AsWedge`) avoiding the engine `Substrate` case names is the right call. The "group = contiguous gather" reading of an underspecified §B.5 "group" is a defensible interpretation, correctly distinguished from the Part J repeat/period builder and recorded in Gotchas.

## Spec fit

The central question. R-2/R-3/R-4 say "`OpticalConstructor.Ui.fsproj` MUST provide" the editor/selectors/switch, and R-6 says the page "introduces the top-level construction/stack-and-tree page" that "MUST be a top-level navigation entry in the main shell … and MUST be the default landing surface." What landed is the pure data/logic behind these, plus presentation-data helpers (`navEntry`, `layerRowLabels`, `confirmationPrompt`) — but no FuncUI view and no shell. `navEntry` is a `{ title; isDefaultLanding = true }` record, not an actual wired nav entry, so R-6 item 1's discoverability/default-landing requirement is asserted by a flag rather than realized.

Two things make this defensible: (a) the spec's own Risks and Testing-plan invoke P3 — "keep the model/`update` Avalonia-free … unit-tested without a UI" — so an Avalonia-free core is explicitly blessed; and (b) "Files in scope" names only `StackEditor.fs` and `ConstructionPage.fs`, not a separate view file, so no view artefact was dropped from the declared scope. The gap is that P3 was written as a *testability* constraint on the model, and the worker has extended it to justify omitting the *entire* render+shell layer. That is the judge's call: if P3 covers it, this is complete; if R-6 item 1's "in the main shell"/"default landing" is load-bearing this cycle, the nav entry is under-delivered. The worker documented the choice and the FuncUI DSL build risk honestly in the impl-log, which is the right posture for an ambiguous spec.

## Evolvability

Mostly forward-friendly. `NodePath = BeamBranch list` keying, the standard `Model`/`Msg`/`update` triple, and the pure transforms will graft directly onto a FuncUI view with no reshaping, so the deferred view is a thin add, not a cornering. The schema `$defs` are filled without renaming the Part A anchors, leaving the un-owned ones tightenable later. The deferred-view decision does push a real obligation onto a later slice (wiring the shell + actually rendering), but it is additive.

## Risks

- **`BeamNode` stores the Sample system twice.** `element : ConstructorElement` carries `Sample of OpticalSystem` (`BeamTree.fs:38`) *and* `system : OpticalSystem` is a sibling field. `editNode` keeps them aligned only for the `Sample` case (`ConstructionPage.fs:160-164`), and `solve` reads `node.system` while serialization persists both copies. The duplication originates in the slice-002 domain, not here, but this slice is the first writer over it and inherits the drift hazard: any future edit path that updates `system` without the `Sample` re-wrap (or vice versa) will serialize an inconsistent node that still "solves" off the live `system`. Worth a guard or a single source-of-truth note when the domain is next touched.
- **Synchronous solve on what will be the UI thread.** `solveNode`/`solveSubtree` are synchronous; the busy indicator is modeled but no async boundary exists yet. The spec deems a flat-element solve short (§B.10 item 2), so this is fine now, but the busy machinery currently has no producer of latency to hide — a reminder for the view slice, not a defect.

## Bottom line

I would lean **ship**. The architecture is sound: engine seams reused not forked, edits immutable, layering correct, schema grounded in the real serialized shape, and every acceptance criterion unit-tested. The only thing I'd flag for the judge is the spec-fit scope call — whether the P3 carve-out legitimately covers omitting the FuncUI view and the actual shell nav wiring, or whether R-6 item 1's "in the main shell / default landing surface" needed to be realized this cycle rather than represented by a `navEntry` flag. That is a verdict question, not a structural defect, and the worker recorded the decision transparently. I have no authority to bind it; the gates are green and the core is clean, so my read is that this is releasable with the scope call noted.
