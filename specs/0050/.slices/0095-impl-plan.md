# Implementation plan

Implement STORE_XDUO_0008 directly over LibraryDatabase.run and the existing entry-version operations. Preserve original envelopes and provenance, project version/hash columns without decoding historical payloads, reject duplicate versions and kind conflicts, and map storage failures to ImportStoreUnavailableErr. Move the in-memory adapter into createInMemory and retire ofStores.

Move the four import store facts into CatalogueImportContract, run them for memory and SQLite, and add the specified rejection, envelope fidelity and disposed-database coverage. Migrate the existing database import fact without losing its assertions. Keep production startup identification unconfirmed and ensure populated UI fixtures explicitly apply every default choice.

Modify only the eight source/test files named by slice 0095, the current contract lifecycle registry, and the required slice outputs. Main risks are nested Result flattening at the database boundary, latest-version query semantics, preserving retirement pin state, and accidentally confirming production imports. No durable format changes, cache, extra connection, retries or migrations are needed.

The worker prompt assigns gate execution to the supervisor, while higher-priority workspace instructions require local verification. Run the resolved checks locally, retain supervisor acceptance ownership, and avoid claiming unmeasured results or changing checkpoint baselines.
