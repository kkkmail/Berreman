# Implementation plan

Implement S31-FORK-PROVENANCE within the five files authorized by slice 0091. Add the mutually recursive EntryForkInfo record and ForkProvenance case, with user-managed protection and category origin. Extend the existing database projection with tag 5 and no catalogue. Reuse the existing JSON serializer and schema definitions, adding only the recursive fork shape; keep schema version 1.

Extend the two named facts without changing the discovered test count. Cover a fork of the built-in catalogue in the domain fact, and a source-pull fork plus a fork of that fork in the entry-file fact, preserving identity, version, licence, citation and material decoding through the existing round-trip helper.

Inspect existing provenance consumers for exhaustive matches. Main risks are matching the serializer's Case/Fields encoding and the recursive schema reference. No importer decision logic, compatibility readers, version bumps, package changes or catalogue edits belong to this slice.

Perform local verification required by the higher-priority workspace instructions using the resolved build, elevation, constructor-unit-tests and eleven s12 invariant gates. Record observations separately from supervisor acceptance. Store persistent logs only in specs/0050/.artifacts. Leave the pre-existing supervisor-owned .manifest.state.json change untouched.
