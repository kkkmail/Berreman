# Implementation plan

Extract `TestAppScope.inverseModelOver` from `studyKitIn`, preserving the real
opened-kit overlay and window dependencies. Add only the authorized AT-1 fact
and append its source to the UI test project's compile list.

Build a fresh optics-mueller folder under the test output, copying only the
committed data tree. Drive CreateKit, item forms, parameter edits, table placement,
setup grids and correction setups through the application messages, with the
specified identities and fixed clock. Independently reopen the folder proxy and
assert metadata, item parameters, setup contents, catalogue snapshots and lab bytes.

Risks are composition parity, physical slot order, dial versus placement angles,
and test resource cleanup. Read the existing semantic-message contracts before
editing. No production storage changes, test identity expansion or durable-format
change is authorized. IMPLEMENT Invariant 6 assigns all gate execution to the
supervisor; this worker records source changes and pending verification only.

Attempt 02 repairs the supervisor-reported FS0039 in `KitAcceptanceTests.fs`.
`KitHistoryLimit` is declared in `OpticalConstructor.Domain.WorkbenchSettings`;
import that module, as the existing kit-start tests do. Preserve the complete
fixture, assertions, test identity, shared initializer and project registration.
Record the supplied failed-build evidence and leave all gate execution to the
supervisor under IMPLEMENT Invariant 6.

Attempt 03 repairs the measured catalogue snapshot lookup in AT-1. The app's
`shippedCatalogueFolder` names `Catalogue`, while shipped entries live in its
`Builtin` child. Reuse `CatalogueProxy.builtInFolderUnder` with the required
domain and storage imports; retain every assertion and the existing test
identity. Only `KitAcceptanceTests.fs` and these worker handoff documents need
edits. Record the supplied acceptance failure and return gate execution to the
supervisor without changing any gate, count floor or acceptance obligation.
