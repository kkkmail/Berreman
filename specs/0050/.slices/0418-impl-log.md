# Step 0418 implementation log

## Progress

- Read AGENTS.md, the task, both cross-repository worker prompts, the project prompt, slice 0418, naming conventions and applicable operator rulings 010/013/015.
- Inspected existing Results/Unknowns views, the flow, publishing domain functions and UI fixture infrastructure. Starting source tree is clean; specs/0050/.manifest.state.json is a pre-existing supervisor edit.
- Before implementation: add only the declared Publish surface and authorized test effects, reusing the existing publishing operations.
- Added the exact three authorized test identities and extended the declared flow/ID/read-only facts without removing existing assertions. Shared publication fixture support lives in the earlier-compiled ScrollExtentTests.fs and reuses StudyFitViewFixture and TestAppScope; it loads the shipped black-box model, records runs through writeFitRun and resolves them through ReloadKitChanges / SelectFitRun.
- Before production wiring: implement the view's pure calibration preview, promotion subjects, read-only availability and selected-run keys; add the flow case, IDs and project compile entry.
- Implemented PublishFlowStep after Results in the inverse flow, its stable IDs and subject-derived promotion IDs, the compiled StudyPublishView, and the keyed KitWindowView surface. Forward mode rejects Publish and switching to Forward leaves it for Experiments through the existing rule.
- The view calls calibratedItemsWith once for both preview and action availability. It renders only parameters changed by the pending revision, uses invariant G6 numbers, derives promotion subjects through the stored model resolver, and dispatches the existing messages. A run-keyed subtree replaces retained action handlers when selection changes. Material labels come from the opened study material.
- Reviewed the source against S115-PUBLISH-STEP in the immutable acceptance oracle. Covered empty/error states, both promotion cases and actual button effects inside the declared identities. Reused the Results text collector through a parameterized helper rather than duplicating it.
- Source inspection caught a test's incorrect assumption that readLabFile returns FileByteContent; corrected the assertion to compare the LabFileSource.content fields. No build/test was run to make this observation.
- Normalized all ten edited production/test files to LF. Ordinary and ignore-CR-at-EOL numstats agree. Source edits are limited to the slice's ten authorized paths; the pre-existing supervisor manifest edit remains untouched.
- Completed the assigned state-of-the-world handoff. No implementation requirement or test identity is deferred; verification belongs to the supervisor.

## Files modified

- Controls/UiIds.fs: five Publish IDs.
- Ui/TableAndElementRotationView.fs: flow ordering, caption, ID, inverse-only routing.
- Ui/ConstructorUiIdOfDomain.fs: promotion ID by subject identity.
- Ui/StudyPublishView.fs: new scrollable publication surface.
- Ui/OpticalConstructor.Ui.fsproj: compile the view immediately after Results.
- Ui/KitWindowView.fs: keyed Publish surface.
- Tests/KitWindowModelTests.fs: extend the two declared flow facts.
- Ui.Tests/StudyViewTests.fs: extend IDs and add the preview/promotion and publishing smoke facts.
- Ui.Tests/KitWindowTests.fs: extend the three declared ID/layout/read-only facts.
- Ui.Tests/ScrollExtentTests.fs: shared fixture support and the declared Publish reachability fact.
- Assigned 0418 implementation plan, this log and state-of-the-world paths only.

## Testing state

No builds, tests or gates run: the IMPLEMENT worker's Invariant 6 assigns execution to the supervisor. No passing result or readiness verdict is claimed.

The supervisor must execute the unchanged 0418.gates roster: build, elevation from the arc origin, all constructor tests, all UI smoke tests, the selected KitWindowTests/ScrollExtentTests/StudyViewTests non-smoke tests and twenty s40 invariants. The authorized additions are exactly one smoke identity and two non-smoke identities, with no added constructor identity or retirement. No worker development run exists, so there is no measured duration or deadline overrun to report. commit_ready remains false pending verification.

## Artifacts

No runtime artifacts produced. Persistent worker artifacts, if needed, belong under specs/0050/.artifacts.

## Gotchas

- Preserve the supervisor's manifest edit and all frozen acceptance/gate/census metadata.
- Reuse UI-visible fixtures; constructor test executable fixtures are not automatically available to UI tests.
- The rich publication fixture calibrates CPL1, CPL2 and QZ, promotes the opened material and QZ, and retains every parameter snapshot required by real promotion. QZ uses the shipped black-box ItemModelVersion; the production view resolves its stored version through model.tryItemModelChoice.
- The latest available measured constructor/smoke floors are checkpoint 416's 1754/262. No ui_tests checkpoint exists; the handoff preserves the previous handoff's historical 16 without representing it as this selection's measured count. Only the supervisor publishes new counts.
- No gate, census, baseline, analyzer configuration, external repository, version, notebook, raw lab data or Git history was changed. No substantive scope deviation was needed.
