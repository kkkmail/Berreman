# Implementation plan — 0114

Add `CatalogueImportProxy.attributionLinesOf` beside the proxy's other reads.
Deduplicate ids in input order, read through `tryHeldEntry`, and derive named
lines from the held entry's current provenance using `attributionOpt`. Preserve
the first read error and perform no writes, caching, or per-version reads.

Add the declared fact once to `CatalogueImportContract` so both the in-memory
and SQLite stores inherit it. Reuse `BuiltinCatalogue` content builders and
`putSealedEntries`, importing the shipped catalogue first. Cover the prescribed
PTB, RII, fork, user, built-in, missing, and duplicate inputs, together with
empty input, read order, latest provenance, and error short-circuiting.

Only product edits: `OpticalConstructor.Domain/CatalogueImport.fs` and
`OpticalConstructor.Tests/CatalogueImportInMemoryTests.fs`. No durable shape,
UI, package, version, or project-file changes are needed. The supervisor's
existing `.manifest.state.json` change stays untouched.

The session's higher-priority verification requirement takes precedence over
the worker task's act-only wording. Run the resolved build, elevation,
constructor-unit-tests, and eleven s14 invariants; save persistent output in
the supplied `.artifacts` folder. The supervisor retains formal acceptance and
checkpoint ownership. Check the two declared case identities and the expected
increase from the prior measured 988 cases, without editing any baseline.
