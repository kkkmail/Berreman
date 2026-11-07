# Implementation log

## Progress

- Read the task, worker protocols, project prompt, AGENTS.md, naming conventions, slice and resolved gate roster. Inspected the existing Materials model and prior slice's shared-picker work.
- Before implementation: inspect the existing export types, fixtures, view mounting and pointer dispatch; add only the six authorized tests and four authorized product/test source edits.
- Added shared outcome captions, the save picker, attribution panel, export model fields/messages, Browse-only verbs, inline results and four stable ids. Export updates call the existing seam and preserve gates, selection and mode; attribution reads the existing proxy.
- Before test implementation: add the six exact facts, reusing BuiltinCatalogue and the existing runtime storage proxy. Include real pointer-driven save/folder exports, picker options, pinning, category closure, refusal/recovery, mode visibility and attribution selection changes. Do not add test identities for picker scenarios.
- Added all six declared facts. The selected-export fact changes selection through the real tree before pressing Export, inspects save options and configured start-folder resolution, checks cancellation and the rendered outcome, then proves a physics edit preserves pinned version 1 and creates version 2. The shown-export fact clicks the category facet and folder-export verb and reads back exactly the two materials plus their user category. The other facts cover ids, duplicate-filename refusal/recovery and rendered colour, missing/read-failed selections and Select-mode affordances, and live CC-BY/CC0/unlicensed attribution.
- Reused LibraryPickerStorageTestProxy and TestAppBuilder for the two picker-backed view facts. MaterialsWindowTests joins the existing nonparallel picker collection because its isolated sessions register process-global Avalonia services. No extra test identity, project entry or package was introduced.
- Before handoff: review the four source diffs, explicit API types and record formatting; write the canonical state-of-the-world and leave all execution/count measurements to the supervisor. Installed Avalonia metadata identifies ShowOverwritePrompt as Nullable<bool>, so the picker sets it explicitly with System.Nullable true.
- Completed source review and the canonical state-of-the-world. The four source diffs have identical sizes with and without ignoring carriage returns; no CRLF churn was introduced. All requested implementation is present. No build, test, analyzer or invariant gate was run; no version, census, baseline, gate, supervisor-state or git-index mutation was made.

### Attempt 02

- Read the retry task and supervisor build diagnostics. The supervisor reported FS0041 at the AppliedConstraint list equality assertion and FS1129 at the attempted tryLatestEnvelope record update; later gates did not run after the build failed.
- Before repair: retain all six authorized test identities and their assertions. Specify AppliedConstraint list explicitly for Assert.Equal. Source inspection confirms tryLatestEnvelope is a member that binds tryHeldEntry to tryHeldEnvelope; inject the read error through the latter record field with its two arguments. No production change is needed for either diagnostic.
- Repaired both reported compile sites in MaterialsWindowTests.fs: Assert.Equal<AppliedConstraint list> retains the facet-preservation assertion, and tryHeldEnvelope now returns the injected read error for the held selection. The existing failed-selection and hidden-export assertions remain intact. No test identity, assertion, product behavior or acceptance requirement changed.
- Reviewed the edited source and diff sizes; the four authorized source diffs have identical numstat output with and without ignoring carriage returns. Updated the canonical plan, log and state-of-the-world. No build, test, analyzer or invariant gate was executed by this worker; the supervisor must measure the repair.

## Files modified

- Created the canonical implementation plan and this log.
- MaterialsWindowView.fs: export model/messages, seam calls, save picker, Browse verbs, inline result and attribution, and refreshed shared press callbacks.
- ConstructorUiIdOfDomain.fs: shared error, entry-export and catalogue-export captions.
- Controls/UiIds.fs: the four prescribed Materials export/attribution ids.
- MaterialsWindowTests.fs: six authorized facts and their shared fixture helpers.
- Wrote 0119-state-of-the-world.md at the supplied path.

## Testing state

No checks or gates run. WIRE_UI invariant 6 assigns build, elevation, ui-smoke, selected ui-tests and eleven s14 invariants to the supervisor. Exactly six new MaterialsWindowTests facts are authorized. No baseline or census changes will be made.

Attempt 02 was triggered by the supervisor's failed build (FS0041 and FS1129); both reported source defects are repaired. The supplied diagnostic is the last measured gate outcome, not a result from this worker. A new supervisor build and the remaining resolved gates are pending; no passing result or commit readiness is asserted.

The selected ui-tests classes remain MaterialsWindowTests, SampleEditorWindowTests, ScrollExtentTests and WindowLauncherTests. The six additions have no ui-smoke trait. The last persisted smoke baseline is 215 at checkpoint 111; the checkpoint file contains no ui_tests baseline. No current count, passing result or commit readiness is asserted.

## Artifacts

No runtime artifacts generated. Persistent artifacts belong in specs/0050/.artifacts.

## Gotchas

- The existing modifications to .manifest.overrides.jsonl and .manifest.state.json belong to the supervisor and remain untouched.
- The worker-specific act-only instruction supersedes the generic local-gate procedure. Verification is pending, not claimed green.
- Source inspection found that verbButtonOn subscribed only when its fixed id changed. Export callbacks capture the selected envelope, so that policy would retain the prior selection's suggested filename. Changed this authorized helper to SubPatchOptions.Always and cover a selection change before export in the declared selected-export fact. This is the only implementation adjustment beyond the slice's prescribed wiring.
