# Implementation plan — 0410

Implement S115-WINDOW-STUDIES within the five authorized product/test files. Extend kit opening to read studies and each study's fit runs through the existing ordered Result collector, retaining failure cleanup and overlay rebinding. Extend the shared window content loader and reload path, then add the specified transient study state, context projections and semantic messages using the existing read-only guard and recordStudy boundary.

Add exactly the five authorized test identities: one KitOpeningTests fact and four StudyWindowModelTests facts. Use shipped model versions, real kit files under the test output directory and one BuiltinCatalogue store set per window fixture. Cover ordered reads, failed opens and reloads, selection retention, study creation, refused writes and material resolution through the existing messages.

Risks are preserving kit ownership on read failures, distinguishing domain refusal from persistence refusal, retaining selections only while their content exists, and avoiding additional public primitive signatures. No schemas, gate contracts, test census, versions or supervisor state will change.

The IMPLEMENT worker protocol assigns all checks to the supervisor. Write the implementation log and state-of-the-world handoff; do not execute builds, tests or gates or claim their results. Preserve inherited gate baselines as historical values only.

Attempt 02 repairs the supervisor-reported F# strict-indentation failures in the study message arms. Review the five authorized files for the same nested record-update layout, including the new test fixture, and preserve the implemented behavior and all five test identities and assertions. Record the supplied failed-build evidence separately from the pending supervisor verification of this repair.

Attempt 03 repairs the supervisor-reported FS0039 in StudyWindowModelTests.fs. CatalogueImportContext.create is an extension declared in OpticalConstructor.Storage.CatalogueImportStore, which the fixture has not opened. Add that existing module import, matching KitWindowModelTests, while retaining the shared stores.proxy and all test assertions. Update the required handoff with the supplied 38.86-second failed build and leave gate execution to the supervisor.
