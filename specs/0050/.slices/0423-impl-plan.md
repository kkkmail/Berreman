# Approach

Attempt 03 repairs the supervisor-reported NullReferenceException at
DataStepTests.fs:503. The unmatched row now contains a ComboBox whose visual
descendants can include a TextBlock with null Text; the reason lookup calls
StartsWith on every descendant TextBlock. Match the exact expected reason
with null-safe F# equality, retaining Seq.exactlyOne and the foreground-color
assertion. Preserve all test identities, assertions and product behavior.
The supervisor owns compilation and gate verification of this repair.

Attempt 02 repairs the supervisor-reported FS0010 at DataStepTests.fs:630.
The final assertion closes four lambdas, but only the recording-window, kit
fixture and headless-session lambdas remain open. Remove the extra closing
parenthesis, preserving the implementation, test identities and assertions.
Update this plan, the implementation log and the state-of-the-world at their
supplied paths. The supervisor owns the next compilation and gate run.

Implement spec 0050 S12.1's setup choice on unmatched split rows, reusing
step 0422's OfferSplitGroupOver message and the existing offer toggle. Add the
central ConstructorUiId member beside the other split-row IDs. Only name/setup
matching failures with at least one kit setup receive the unselected picker;
the existing reason remains visible and choices follow kit order.

Modify only ConstructorUiIdOfDomain.fs, DataStepView.fs and DataStepTests.fs,
plus the three required worker documents. Author the sole declared new fact
and extend the two named existing facts, preserving their assertions. Reuse
the existing folder-kit, recording-window and Elmish-window fixtures to cover
dispatch, both eligible errors, empty setups and the offered toggle lifecycle.

# Risks and verification handoff

The picker must not dispatch on mount or render on matched/offered rows. Its
selection callback must retain the current setup list after a FuncUI patch.
Keep the wrapping reason in its DockPanel row and preserve LF text. No durable
shape, test census, gate configuration, version or supervisor state is changed.

The IMPLEMENT worker's Invariant 6 reserves all build, test, analyzer and
invariant execution for the supervisor. Deliver source and handoff documents
without claiming measured gate results or inventing count baselines.
