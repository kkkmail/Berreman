# Implementation plan — 0133

Implement S42-COPY-IN within the four authorized source files. Reuse
KitDataFolder.copyTargetFor, KitFolder.tryKitPathOf, ContentHash.ofBytes,
FileByteContent and STL ExistingFilePolicy. Preserve the shared lock guard and
all existing fact identities and assertions.

First extend KitTestFolder and author exactly four shared contract facts
(eight discovered cases) plus two filesystem-only facts. Cover unchanged
original files, suffix selection over files and folders, nested folder creation,
lexical location, read-only refusal, exact disk bytes including BOM, and atomic
writer refusal/cleanup. Exercise outside deletion within the authorized facts.

Then extend Domain's KitProxy and in-memory folder, consolidate atomic writing
under ExistingFilePolicy, and wire filesystem copy-in through the new-file
writer and the same decode/hash body used by readDataFile. The in-memory double
retains its one text map and hashes its stored UTF-8 text as specified.

The new atomic writer will take FileByteContent, matching step 0126's measured
ELEV010 repair, rather than reintroducing the slice's raw byte-array signature.
Record this bounded signature deviation and preserve the member's semantics.
No durable format changes, new dependencies, retries or cache are needed.

IMPLEMENT invariant 6 assigns all gates to the supervisor. Do not run builds,
tests, analyzers or invariant commands. Complete source review and the required
implementation log/state-of-world; carry only checkpoint 131's inherited count
of 1,076 constructor cases and report the ten new cases as unmeasured.

## Attempt 02 — staging-file ownership

Address the code judge's occupied-staging finding within the existing shared
writer and the authorized new-file refusal fact. First extend that fact to
exercise an absent destination with an existing sibling `.tmp`, both directly
and through copyInFile. Preserve every current assertion and test identity;
assert typed errors, unchanged staging bytes, no destination and no extra paths.

Then acquire refuse-existing staging files with FileMode.CreateNew and mark
cleanup ownership only after acquisition succeeds. Dispose the stream before
the existing non-overwriting final move. Keep the overwrite branch's write and
cleanup semantics, error mapping and no-retry contract. Leave all shared lock
guard code unchanged. Update the implementation log and state-of-world for the
unchanged supervisor gates, without running checks or claiming gate results.
