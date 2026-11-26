# Step 0048 implementation plan

Use the materials proxy already held by the scene for Details-band names.
Remove the private standard material library, give bandDisplayName and
Sample.bandsState the explicit MaterialProxy parameter, and pass model.materials
from detailsState. One lookup per band returns the stored name or the material
GUID for a missing entry or lookup error. Keep band colours unchanged and
update Schematic's curated-map documentation to name MaterialIds.

First update the existing MainWorkbenchTests band-shape fixture to load the
quarter-wave sample and materials through BuiltinCatalogue, preserving its
assertions. Add the exact declared regression fact for the three catalogue
names and empty-store GUID fallbacks. Then implement the production change.
Review callers and the scoped diff, including LF line endings.

Only TableAndElementRotationView.fs, Schematic.fs and MainWorkbenchTests.fs
change, alongside the required worker documents. There is no persistent shape
change, migration, cache or additional lookup. Risks are missed bandsState
callers and incorrectly handling Result<Option<_>> from tryGetMaterial.

The IMPLEMENT worker protocol assigns all gate execution to the supervisor.
Do not run build, tests, analyzers or invariant gates and do not claim a gate
result or invent a baseline. Leave the pre-existing manifest-state edit alone.

## Attempt 02 repair

The supervisor measured ELEV010P on bandDisplayName's MaterialProxy -> string
signature. Return the existing MaterialLibrary.EntryName instead: preserve the
store entry's name directly and construct the missing/error GUID fallback as an
EntryName on MaterialId. Render its value only when composing the band caption.
This matches the existing CategoryId.categoryName pattern and introduces no new
type, conversion helper, cache or lookup. Only TableAndElementRotationView.fs
needs a source edit; preserve Schematic.fs and both acceptance facts.

This intentionally differs from the slice's private string return annotation:
AGENTS.md gives the measured analyzer rule precedence over document signatures.
The repair stays within the authorized file and preserves the public bandsState
contract and label text. Record the deviation in the handoff; leave gate
execution and readiness certification to the supervisor.
