# Implementation plan

Implement S31-IMPORT-LOG in the four authorized source files. Add the elevated UTC import time, entry count and log record to CatalogueImport, extend the proxy, and keep an id-keyed log inside each in-memory store factory. Reuse the existing CatalogueImports entity and LibraryDatabase.run error boundary for SQLite listing, replacement and deletion, with marked row conversion members.

Add one inherited contract fact in CatalogueImportInMemoryTests.fs, discovered under both concrete store classes. Cover empty logs, ordinal folder order, exact record/time preservation, replacement by catalogue id, idempotent forgetting and independent store instances. Extend the recording proxy and the existing disposed-database fact for the new fields. Preserve existing tests and database schema.

The SQLite DateTime reader loses DateTime.Kind, so restore UTC explicitly at the storage boundary without changing ticks. Sort folders with ordinal comparison in both stores. Replacement must update all tracked row columns, including folder, time and count.

Run the resolved Release build, elevation, complete constructor suite and eleven s12 invariants as required by the higher-priority workspace verification instruction. The worker prompt's act-only instruction conflicts with that requirement; measured local results remain separate from supervisor acceptance. Write the implementation log and state-of-the-world to the supplied paths, with persistent verification output only under specs/0050/.artifacts. Do not mutate Git, versions, gate definitions, checkpoints or the supervisor's existing .manifest.state.json change.
