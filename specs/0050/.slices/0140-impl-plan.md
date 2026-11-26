# Implementation plan — 0140

Add `KitSnapshotOrigin of KitName` to the existing import origin and render it through the kit's name. Keep the existing status classification and catalogue/dropped-file decisions, then adjust kit actions to reuse identical content or skip incoming content by default. Offer the existing import action explicitly; name collisions offer adding followed by held-entry rebindings in input order, except categories, which offer adding alone.

Modify only `OpticalConstructor.Domain/CatalogueImport.fs` and `OpticalConstructor.Tests/ImportDecisionTests.fs`. Reuse the existing kit name, envelope sealing and held-entry fixtures. Add exactly the two fact identities authorized by the slice, retaining all existing assertions. Cover missing ids, equal hashes, unheld built-in versions, divergence, collision order, category exclusion, origin rendering and default choices.

The main risks are accidentally changing the decision precedence or catalogue defaults, moving latest for a kit version, and allowing category rebindings. Review the existing apply seam and origin matches while implementing. No durable format changes or project edits are required because `Kit.fs` already precedes `CatalogueImport.fs`.

The IMPLEMENT worker's act-only protocol reserves build, elevation, constructor-unit-tests and the eleven S16 invariant gates for the supervisor. Record verification as pending and repeat only the existing supervisor baseline in the state-of-the-world; do not run gates or edit their census, oracle, snapshots or checkpoints.
