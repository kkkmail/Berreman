# Step 0061 implementation plan

Implement S21-DB-TAGS in the four authorized F# files. Add the two declared
facts first, covering all six state/tag pairs, unknown tags, catalogue sample
references and two versions of one material. Preserve the existing
referencedMaterials coverage fact unchanged.

Add value and tryOfDb members to EntryLifecycle and EntryProtection, and add
PinState immediately after EntryLifecycle. Move the material-reference walk
to SampleStructure.referencedMaterialVersions and project its result for
referencedMaterials. Keep the existing stores, VersionsInUse and callers
unchanged. No durable format or version bump is introduced.

The risks are accidentally collapsing material versions, expanding repeated
cells unnecessarily, or changing existing identity-only reference behavior.
Use exact set assertions and the unchanged coverage fact to guard these.
Local development build/test validation follows the active coding-session
instructions; formal build, elevation, constructor-unit-tests and nine s08
gate decisions belong to the supervisor. Do not alter gates or checkpoints.

Write the implementation log and state of the world at the supplied paths;
put any captured run output under specs/0050/.artifacts.
