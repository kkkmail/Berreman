# Implementation plan

Implement S43-SCENE-FILE within the six authorized product paths. Extend
SceneName with FileStem.ofText naming, SceneSnapshot with ordered distinct latest
library references, and Scene.fs with the version-1 SceneFile envelope. Reuse
the existing canonical JSON and versioned parser for setup files.

Add the five declared facts to KitContentFileJsonTests before the production
changes. Use a pure scene record with bound source and polarizer, an unbound
detector on the reflected beam, non-default view, snap and pivot. Cover naming,
canonical round-trip, unsupported versions, schema/malformed errors and reference
selection without adding other test identities.

Add setup.schema.json and its PreserveNewest content entry, a lazy schema loader
and validateSceneFile, then SceneFile.fileText and KitFileText.parseSceneFile.
Keep version-1 body validation conditional so future positive versions reach
UnsupportedSchemaVersionErr even without a body. Serializer-owned scene values
remain on the shared serializer path. Reuse KitContentFiles' existing JsonText
conversion rather than introducing another parser or duplicate conversion.

Write the implementation log and state of the world at the task's exact paths.
The IMPLEMENT worker runs no checks under Invariant 6; the supervisor owns the
build, elevation, constructor suite and eleven s17 invariant gates. Preserve
the supervisor-owned manifest-state change, test census, checkpoint counts,
version locks and sibling repositories.
