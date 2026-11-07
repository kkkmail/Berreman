# Implementation plan

Wire Materials export and licence attribution to the existing catalogue import/export seam. Add shared outcome captions and the entry save picker, then extend the model, semantic messages, Browse verbs, inline result and selected-material attribution. Preserve selection, mode and confirmation gates during export.

Modify only MaterialsWindowView.fs, ConstructorUiIdOfDomain.fs, Controls/UiIds.fs and MaterialsWindowTests.fs. Add exactly the six authorized facts using the existing in-memory catalogue and sealed-entry fixtures; include headless interaction with the real view and semantic-tree assertions within those facts. Files written by tests use unique folders below the test output directory.

Risks are export pinning versus later material saves, category closure, refused duplicate filenames, Select-mode affordances and headless view refresh. Reuse existing domain operations and picker/verb patterns. Per WIRE_UI invariant 6, do not execute builds, tests, analyzers or invariant gates; the supervisor owns verification and measured counts. Write the canonical implementation log and state-of-the-world without changing gate/census files, versions or existing supervisor edits.

## Attempt 02 repair

Repair the supervisor-reported FS0041 and FS1129 in MaterialsWindowTests.fs without changing the six authorized facts or their assertions. Select the concrete AppliedConstraint list overload for the preserved-facets equality assertion. Inject the failed latest-envelope read through CatalogueImportProxy.tryHeldEnvelope, its actual function-valued record field; tryLatestEnvelope is a member that calls that field after reading the held entry. Keep product wiring and the acceptance contract unchanged. Update the canonical log and state-of-the-world; gate execution remains with the supervisor.
