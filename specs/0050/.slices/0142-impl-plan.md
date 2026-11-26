# Implementation plan

Attempt 03 repairs the measured failure in the divergent-version fact. The fixture pins Silicon version 1 but changes only its description, so `decideVersioning` correctly mutates version 1 and never creates the version 2 the fact expects. Give the successor fixture a different existing catalogue material's physics as well as its new description, and assert the backing store holds both the original version and the successor before checking overlay fallback. Preserve all existing assertions, four test identities and production behavior. Update the log and state of the world; executable gates remain the supervisor's responsibility.

Attempt 02 repairs the supervisor-reported FS0039 errors without changing the overlay contract or test census. Inspect the declaring module for `CatalogueKind`, add the missing import in `KitLibrary.fs`, and review the existing four authorized facts and proxy contracts for related name-resolution problems. Preserve all prior assertions and leave executable verification to the supervisor.

Implement S42-OVERLAY in the two authorized files: `KitLibrary.fs` and `KitLibraryTests.fs`. Reuse the existing entry decoders, store proxies, import seam and `LibraryTree.byKind`. Add the three requested records, read-only `KitLibraryContext.openOverlay`, and store extensions that preserve write fields while applying the specified read precedence.

Add exactly the four authorized facts. Cover opening a snapshotted kit against empty stores, divergent version versus latest lookup, explicit replacement, and unchanged underlying listings with write-through. Extend those facts with typed-error, ordering and highest-version assertions where useful; retain all existing test identities and assertions.

Risks are decoder refusal mapping, mismatched version/id precedence, duplicate listing entries, and accidentally exposing snapshots in persistent library windows. Keep all snapshot conversions inside existing decoders and compose record updates without caching or write-back. Record any implementation interpretation in the log.

Per the IMPLEMENT worker's Invariant 6, do not run build, tests, analyzers or invariant gates. The supervisor owns all executable verification after worker exit. Write the implementation log and state of the world at the task's exact paths; preserve the existing dirty manifest state.
