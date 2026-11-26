# Slice 0092 implementation plan

Implement S31-MANIFEST-READ in the four authorized F# files. Add the two named regression facts first and extend the existing Builtin-folder fact. Cover the empty in-memory store, a written manifest, a missing entry, an invalid manifest and a missing manifest.

Add CatalogueProxy.readManifest before readCatalogue. Share the in-memory read closure and extract the folder's existing manifest read/parse body into one private member used by both read paths. Add CatalogueProxy.builtInFolderUnder and route createBuiltInUnder through it, retaining one production Builtin literal. No cache, existence check, stored-format change or additional proxy field is needed.

Validation follows the higher-priority workspace requirement: build Release, run elevation over affected projects, run the constructor suite and preserve the eleven resolved s12 invariants. The supervisor remains the acceptance authority. Keep all captured output under specs/0050/.artifacts and report only observed results. Do not change checkpoints or the supervisor-owned manifest state.

Risks are a missed CatalogueProxy record construction, changed error ordering, or duplicated parsing. The source search found only the in-memory and folder constructors; extracting the existing manifest prefix preserves validation and error mapping.
