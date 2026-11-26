# Where we are

Slice 0119 wires S33-MATERIALS-EXPORT after the shared window helpers of 0118. The Materials window now calls the existing catalogue export seam and displays licence attribution. All six authorized test facts are implemented. Attempt 02 repaired the two test compile defects reported by the supervisor; gate reruns belong to the arc-runner and remain pending.

# What's working

- Added Browse-mode material and shown-catalogue export verbs with semantic messages, configured pickers and stable ids.
- Rendered shared success/refusal captions while preserving selection, mode and confirmation gates.
- Displayed selected-material licence attribution through the existing catalogue proxy.
- Added six authorized facts covering real view interactions, picker options, pinning, category closure, refusals and attribution.
- Refreshed shared verb callbacks so save suggestions follow the current selection.
- Repaired the explicit facet-list equality type and injected failed envelope reads through the proxy's actual record field.

# Tests

No gates or checks were executed, as required by WIRE_UI invariant 6. The supervisor must run the unchanged resolved roster: build, elevation, ui-smoke, selected ui-tests and all eleven s14 invariants. Selected ui-tests classes remain MaterialsWindowTests, SampleEditorWindowTests, ScrollExtentTests and WindowLauncherTests. Exactly six facts were added, all without the ui-smoke trait; no existing assertion or test identity was removed.

The supervisor's attempt-01 build failed with FS0041 at the AppliedConstraint list equality assertion and FS1129 at a tryLatestEnvelope record update. Attempt 02 repairs those sites with Assert.Equal<AppliedConstraint list> and a two-argument tryHeldEnvelope stub. The reported failed build remains the latest measured result until the supervisor reruns the gates.

The YAML below repeats the last persisted smoke baseline (checkpoint 111), not a result from this round. The checkpoint file contains no ui_tests baseline; selected-case counts must be measured by the supervisor. No checkpoint or baseline file was edited.

```yaml
gates:
  ui_smoke_tests: 215
```

# Architecture

The UI delegates export, referenced-entry closure, duplicate-name validation and version pinning to CatalogueImportContext.exportEntry/exportEntries. Shared extension members render outcomes for both library windows. pickEntryExportFile follows pickLibraryFolder's asynchronous storage-provider and UI-thread callback pattern. Attribution uses the existing EntryAttributionLine projection without recreating licence rules.

The two picker-backed tests reuse LibraryPickerStorageTestProxy and TestAppBuilder, with a window-specific provider factory in isolated headless sessions. MaterialsWindowTests uses the existing nonparallel picker collection to protect Avalonia's process-global services. Test files use fresh GUID directories below AppContext.BaseDirectory/MaterialsWindowTests.

# Deferred

No slice requirement is deferred. Gate execution is the supervisor's required next stage, not a worker-claimed pass. Library-window consumers of the shared helpers remain outside this slice's authorized edits.

# Gotchas

- verbButtonOn previously retained handlers while its fixed id stayed the same. Export handlers capture the envelope for the save suggestion, so SubPatchOptions.Always is necessary to follow changed selections. The selected-export fact exercises that transition through the real tree.
- Avalonia storage interfaces require runtime doubles; this slice reuses the existing DispatchProxy fixture rather than implementing those interfaces directly. ShowOverwritePrompt is Nullable<bool> and is set explicitly.
- The existing .manifest.overrides.jsonl and .manifest.state.json edits belong to the supervisor and were left untouched.
- CatalogueImportProxy.tryLatestEnvelope is a member, not a record field; tests inject read failures through tryHeldEntry or tryHeldEnvelope. This retry uses tryHeldEnvelope so the real latest-envelope lookup reaches the failing version read.
- No successful build, warning status, analyzer outcome or test count has been verified after the retry repair. Commit readiness is not asserted.

# Changelog

- 2026-11-07: Implemented Materials export and attribution wiring, shared outcome captions, stable ids and six authorized wiring/behavior facts; handed verification to the supervisor.
- 2026-11-07 (attempt 02): Repaired both supervisor-reported test compile defects without changing test identities, assertions or production wiring; handed gate reruns to the supervisor.
