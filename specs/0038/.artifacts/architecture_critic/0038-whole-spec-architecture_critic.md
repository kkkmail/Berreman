# Architecture critique -- .spec-md cycle 1

## Summary

This cycle's diff is the claim-check fix round: a single amendment to §0.2 of
`.spec-md` (`.spec-jsonl` untouched). I re-verified the amendment's every new
citation against the tree and it is accurate and architecturally sound — the
metadata-only library serialization is now named (`Report.fs:17-24`, module
`MaterialLibrary`, `MaterialEntryDto` at `:36` carrying id/name/category/
description only; `MaterialImport.fs` `importRefractiveIndexInfo :407` /
`importCsv :432` / `exportCsv :459`) and correctly scoped as unwired from the
Main-flow composition (`…App/Program.fs:129-147` builds `createInMemory`
stores only). The whole spec remains strong and unusually well-anchored; the
one finding that, uncorrected, forces a rule violation is unchanged from my
read of the full spec: the `ConfigKey` module placement in step 5 versus its
consumption in step 40.

## Layering

The central move — Part B's relocation — fixes a real inversion.
`OpticalConstructor.App` today references `OpticalConstructor.TestWindows`
(`OpticalConstructor.App.fsproj:50`); after steps 3–4 the arrow reverses to
TestWindows → Ui with App dropping TestWindows entirely, which is the correct
direction. Step 32's chart reachability holds: Ui references Analytics
directly (`OpticalConstructor.Ui.fsproj:199`), Analytics references
OpticalProperties (`Analytics.fsproj:72`), and Domain references the Berreman
core (`OpticalConstructor.Domain.fsproj:88`), so `RhoWithDispValue.toRhoWithDisp`
and `MuWithDispValue.toMuWithDisp` are both in reach. `FacetedTreeControls`
staying domain-free in `OpticalConstructor.Controls` matches that project's
current shape (no project references at all).

**ConfigKey placement (step 5 vs step 40) is the one broken edge.** Step 5
puts the single `[<Literal>]` `ConfigKey` module "beside the provider read in
OpticalConstructor.App"; step 40's EFC proof test lives in
`OpticalConstructor.Tests`, which references Domain, Storage, Optimization,
and Ui (`OpticalConstructor.Tests.fsproj:164-167`) — not App — yet must read
"the step-5 ConfigKey entries" for the provider choice and connection string.
As written the worker must either add a Tests → App reference (dragging the
Avalonia WinExe into the domain-test graph, with the MSB3277 exposure §0.6
forbids) or duplicate the literals, which step 5's "ONE ConfigKey module"
forbids. The smallest fix is spec text, not implementation choice: place
`ConfigKey` where both ends already reach — Storage (already carries the
`Softellect.Sys` reference and is referenced by Ui and Tests) or Domain (pure
literals need no package).

## Separation of concerns

**WindowRegistry as module-level mutable state (step 8).** The step prescribes
a "module-level mutable Map<WindowKey, Window>" on the registered-storage-
provider precedent (`…App/Program.fs:96` in today's tree). That precedent is
cardinality-one app state no test asserts on; the registry is precisely the
state steps 8, 13, 15, and 45 write headless assertions against ("second open
activates", "one single instance"), so a process-global map leaks windows
between tests and makes those suites order-sensitive. The codebase's stronger
precedent is the `createInMemory` proxies closing over a private `ref Map` per
instance (`…Domain/ElementId.fs:652,722`); a `WindowLauncher.create` that
closes over its own registry gives the app exactly one registry at the
composition root (step 47 builds it once) and every test its own — same
runtime behaviour, testable by construction.

## Consistency

**Preset entry ids stay bare strings inside new contracts.** §0.1 binds every
part to elevated primitives, yet the seeded presets carry raw string ids today
(`PolarizerPreset.id : string`, `…Domain/ElementId.fs:179-184`; `"det-intensity"`
etc. at `:532-541`), and step 25's `ElementDescriptor` embeds "protected
presets bind by entry id" — a bare `string` flowing into a brand-new domain
contract that step 28's `ExperimentCollectionSnapshot` then transitively
carries. Step 14 re-types the `LibraryEntry` surface anyway (adding
`EntryProtection` to every entry), so an elevated preset/entry-id DU is nearly
free there; the spec should either mandate it or record why the raw string is
accepted. Silent as written, it invites a §0.1 violation in a new record.

**SelectionContext equality (step 16).** `SelectionContext` carries
`onSelected`/`onCancelled` function fields and is embedded in
`LibraryWindowMode = Browse | Select of SelectionContext`, which lives in the
window's model. Records with function fields compile but throw on structural
comparison — the mechanism step 33's dirty check and common test assertions
lean on. Steps 26/28 explicitly mandate `[<ReferenceEquality>]` for the new
proxies; `SelectionContext` should be named for the same attribute.

## Spec fit

The claim-check fix was applied exactly as directed: only §0.2 changed, no
step text moved, and the amendment matches the tree (I read every cited line).
Coverage remains faithful: Parts A–N map onto steps 1–47, and the contract
table agrees with the registry — all 11 minted entries (`UICOMP_XDUO_0007-0011`,
`SVC_XDUO_0001`, `STORE_XDUO_0004-0008`) match on name, kind, language, and
declaring project, alongside the retained `STORE_XDUO_0001/0002` whose
in-place re-typing §0.2 pins to the 0035 precedent. Out-of-scope declarations
(ConstantMueller editor, `.ocproj`, NCS/Mueller imports, solver math) are
consistently repeated at step level. No scope creep found.

## Evolvability

Step 11 builds the sample facet extractors over `SampleStructure`'s
`MaterialId` references (`…Domain/ElementId.fs:103`), which step 22 then
re-types to `MaterialVersionId` — acknowledged rework ("update … facet
extractors … mechanically"), acceptable but the judge should know the facet
tests written in step 11 get touched again in step 22. The versioning design
is well-seamed for the future: one pure decision function (step 20), the
`VersionsInUse` seam over the in-memory experiment store, and in-place proxy
re-types leave the future persistence cycle a narrow swap surface. The §0.2
amendment strengthens this posture: by naming the metadata-only share/export
format and pinning that "neither gains a persisted-state role" this cycle, it
prevents a worker from quietly promoting `Report.fs`/`MaterialImport.fs` into
a state store the no-migration rule depends on not existing.

## Risks

Step 40's test needs an `appsettings.json` beside the test assembly —
`AppSettingsProvider.tryCreate` opens the file relative to the executing
assembly, and step 5 makes `appsettings.json` a build-copied content item of
App only, which does not flow into `OpticalConstructor.Tests` output. The step
should say the test project supplies its own `appsettings.json` content item
(with the SQLite connection string), otherwise the worker discovers this at
gate time. Smaller: step 4 has `Ui.Tests` reference a second Avalonia WinExe
(`TestWindows.App`); the headless harness boots one `Application`, so
`TestLauncherWindow` renders under the test application while its own entry
point is exercised only by a real launch — the acceptance ("renders … headless")
is window-level and achievable, but the worker should not chase app-level
headless coverage.

## Bottom line

Ship-shaped as a spec, and the claim-check amendment landed cleanly with every
pointer verified. I would proceed to implementation, but the judge should
require one textual correction before the early steps: relocate `ConfigKey`
out of App (the step 5/40 conflict is the only finding that, uncorrected,
forces a rule violation), and prefer a per-instance closed-over registry for
step 8 to keep the single-instance headless tests isolated. The remaining
items — elevated preset id (step 14/25), `[<ReferenceEquality>]` on
`SelectionContext` (step 16), the step-40 test `appsettings.json` — are
one-line clarifications the resolver can fold in.
