# Implementation plan — 0107

Implement the S3.2 import dialog over the existing CatalogueImportContext, decision table and payload diff. Preserve input order and defaults; only Apply writes, while Cancel only closes. Reuse the Materials window's six styling declarations without changing their bodies.

Add ImportDialogView.fs and its project entry, domain-derived captions and keys in ConstructorUiIdOfDomain.fs, and centralized import IDs in Controls/UiIds.fs. Add the nine specified ImportDialogTests facts and their project entry using the existing in-memory catalogue and headless view patterns.

Risks are choosing the correct held version for a diff, keeping choice overrides within the offered actions, and preserving FuncUI keyed control behavior as options and previews change. Keep all changes within the slice's seven authorized source/project files.

Session-level developer instructions require local verification despite the worker's act-only wording. Capture an expected missing-contract build, then run the resolved build, elevation, ui-smoke, selected ui-tests and eleven invariant checks. Formal gate acceptance and baseline updates remain supervisor-owned.
