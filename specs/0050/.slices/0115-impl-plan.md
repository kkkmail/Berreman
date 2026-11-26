# Implementation plan — 0115

Extend `CatalogueImportProxy` with user-entry enumeration and one-way version pinning. Reuse the in-memory material/sample pin verbs, and add `stagePin` on the database operations extension so material, sample and kind-neutral pins share one staging body. Keep latest provenance filtering independent of lifecycle and preserve the pinned row's lifecycle.

Modify only the seven source/test files authorized by the slice. Add two shared contract facts (executed by both stores) and one SQLite fact covering all six entry kinds. Update the recording proxy and existing database-failure coverage for the new fields.

Risks are accidentally filtering retired user entries, pinning latest rather than the requested version, changing lifecycle, or mapping storage failures to rejection. Follow existing envelope decoders and transaction boundaries. No durable format changes or migration are required.

The session's higher-priority verification instructions require relevant tests and required checks. Run the resolved build, elevation, constructor-unit-tests and eleven section invariants locally; formal acceptance and checkpoint updates remain the supervisor's responsibility. Capture results only under the supplied artifacts folder.
