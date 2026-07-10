# Architecture critique -- .spec-md cycle 1

## Summary

This is a strong, unusually well-anchored spec: I spot-checked ~35 of its
path:line citations against the tree and found essentially all of them accurate
to the line (`MaterialId` at `MaterialLibrary.fs:27`, `runAnalyzerKind`'s
`IdealLinear` tail at `TableAndElementRotationView.fs:1427`, the `Border.name`
hazards at `LibraryControls.fs:112,148` and the four latent copies, the
n/k-only preview at `NkDispersionChart.fs:40-64`, the picker precedent at
`Shell.fs:403-414`, and so on). The contract table in `.spec-md`, the 47 steps
in `.spec-jsonl`, and the 11 newly minted registry entries in
`.contract-ids/XDUO-json` are mutually consistent (ids, kinds, languages,
declaring projects all agree). The most important finding is a seam-placement
error in step 5/40: the `ConfigKey` module is placed in `OpticalConstructor.App`,
which `OpticalConstructor.Tests` does not reference, yet step 40's database
test must consume those keys.

## Layering

The central move — Part B's relocation — fixes a real inversion and the
proposed graph is sound. Today `OpticalConstructor.App` references
`OpticalConstructor.TestWindows` (its `.fsproj` confirms both references), so
test-scene code sits inside the product dependency graph; after steps 3–4 the
direction is TestWindows → Ui with App dropping TestWindows entirely, which is
the correct arrow. I also verified the one non-obvious reachability claim:
step 32's chart tabs need `RhoWithDispValue.toRhoWithDisp`
(`OpticalProperties/Active.fs:152`) and `MuWithDispValue.toMuWithDisp`
(`Berreman/Berreman/Dispersion.fs:671`), and the chain Ui → Domain → Analytics
→ OpticalProperties → Berreman exists in the `.fsproj`s. `FacetedTreeControls`
staying domain-free in `OpticalConstructor.Controls` matches that project's
current shape (it references only Avalonia/FuncUI, no project references).

## Separation of concerns

**ConfigKey placement (step 5 vs step 40).** Step 5 puts the single
`ConfigKey` literals module "beside the provider read in OpticalConstructor.App".
Step 40's EFC proof test lives in `OpticalConstructor.Tests`, which references
Domain, Storage, Optimization, and Ui — not App — yet must read "the step-5
ConfigKey entries" for provider and connection string. As written, the worker
must either add a Tests → App reference (dragging the Avalonia WinExe into the
domain-test graph, with the attendant warning/MSB3277 exposure §0.6 forbids) or
duplicate the literals, which step 5's "ONE ConfigKey module" forbids. The
smallest fix is to place `ConfigKey` where both ends already reach: Storage
(already carries the `Softellect.Sys` reference and is referenced by Ui and
Tests) or Domain (pure `[<Literal>]`s need no package). The judge should treat
this as a spec-text correction, not an implementation choice.

**WindowRegistry as module-level mutable state (step 8).** The step prescribes
a "module-level mutable Map<WindowKey, Window>" on the
`Shell.setStorageProvider` precedent (`…App/Program.fs:96`, `…Ui/Shell.fs:56-59`).
That precedent is cardinality-one app state that tests never assert on; the
registry is precisely the state steps 8, 13, 15, and 45 write headless
assertions against ("second open activates", "one single instance"), so a
process-global map leaks windows between tests and makes those suites
order-sensitive. The codebase's stronger precedent is the `createInMemory`
proxies closing over a private `ref Map` per instance
(`…Domain/ElementId.fs:652,722`); a `WindowLauncher.create` that closes over
its own registry gives the app exactly one registry at the composition root
(step 47 builds it once) and every test its own. Same runtime behaviour,
testable by construction.

## Consistency

**Preset entry ids stay bare strings inside new contracts.** §0.1 binds every
part to elevated primitives, yet the seeded presets carry raw string ids today
(`PolarizerPreset.id : string`, `…Domain/ElementId.fs:179-184`;
`id = "det-intensity"` etc. at `:532-541`), and step 25's `ElementDescriptor`
plus step 28's `ExperimentCollectionSnapshot` embed "protected presets bind by
entry id" — a bare `string` flowing into a brand-new domain contract. Step 14
re-types the `LibraryEntry` surface anyway (adding `EntryProtection` to every
entry), so the elevation is nearly free there; the spec should either mandate
an elevated preset/entry-id DU in step 14 or record why the raw string is
accepted. Silent as written, it invites a §0.1 violation in a new record.

**SelectionContext equality (step 16).** `SelectionContext` carries
`onSelected`/`onCancelled` function fields and is embedded in
`LibraryWindowMode = Browse | Select of SelectionContext`, which lives in the
window's model. F# records with function fields compile but throw on
structural comparison — and structural equality is exactly the mechanism step
33 leans on for the dirty check and a common test-assertion pattern. The
codebase's convention for function-valued records is `[<ReferenceEquality>]`
(every proxy record, and the spec itself mandates it for `WindowLauncher` and
the STORE proxies); `SelectionContext` should be named for the same attribute.

## Spec fit

Coverage is faithful to the constraints document: all Parts A–N of `.spec-md`
map onto steps 1–47, every contract-table placeholder is minted
(UICOMP_XDUO_0007–0011, SVC_XDUO_0001, STORE_XDUO_0004–0008) with matching
kind/language/declaring-project in the registry, and the
declare-then-implement split (ADD_CONTRACT/IMPLEMENT_CONTRACT pairs for
WindowLauncher, SceneProxy, ExperimentCollectionProxy, ExperimentDataProxy,
SeedingProxy) follows the repo's proxy discipline. The spec is also honest
about self-invalidating citations (step 8 and step 38 both note their cited
precedent lines are retired by step 2 — "in today's tree"). No scope creep
found; the out-of-scope declarations (ConstantMueller editor, `.ocproj`,
NCS/Mueller imports, solver math) are consistently repeated at the step level.

## Evolvability

Step 11 builds the sample facet extractors over `SampleStructure`'s
`MaterialId` references (`…Domain/ElementId.fs:103`), which step 22 then
re-types to `MaterialVersionId` — acknowledged rework ("update … facet
extractors … mechanically"), acceptable but worth the judge knowing the facet
tests written in step 11 get touched again in step 22. The versioning design
itself is well-seamed for the future: the single decision function (step 20),
the `VersionsInUse` record computed from the in-memory experiment store, and
the in-place re-typing of the two registered proxies per the 0035 precedent
all leave the future persistence cycle a narrow swap surface.

## Risks

Step 40's test needs an `appsettings.json` next to the test assembly —
`AppSettingsProvider.tryCreate` opens the file relative to the executing
assembly, and App's content item does not flow into `OpticalConstructor.Tests`
output. The step should say the test project supplies its own
`appsettings.json` content item (with the SQLite connection string), otherwise
the worker discovers this at gate time. Smaller: step 4 has `Ui.Tests`
reference a second Avalonia executable (`TestWindows.App`); the headless
harness boots one `Application`, so `TestLauncherWindow` will render under the
test application while its own `Application`/entry point stays exercised only
by a real launch — the acceptance ("renders … under a headless frame") is
window-level and achievable, but the worker should not chase app-level
headless coverage.

## Bottom line

Ship-shaped as a spec: anchors verified, contracts consistent, layering
direction correct, and the IO/elevation discipline of CLAUDE.md is carried
through the step texts rather than left to worker interpretation. I would
proceed with implementation, but the judge should require two textual
corrections before or alongside the early steps: relocate the `ConfigKey`
module out of App (step 5/40 conflict — the only finding that, uncorrected,
forces a rule violation), and prefer a per-instance closed-over registry for
step 8 to keep the single-instance headless tests isolated. The consistency
items (elevated preset id, `[<ReferenceEquality>]` on `SelectionContext`, the
step-40 test appsettings) are one-line clarifications the resolver can fold in.
