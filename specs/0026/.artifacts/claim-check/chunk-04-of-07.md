{
  "claims": [
    {
      "id": 1,
      "phrase": "The key map MUST be configurable (the rotation step already is)",
      "spec_location": "§E.8.1",
      "evidence": "\"already\" assumption-shape phrase with no <file>:<symbol> pointer in the sentence; the cited prior configurability is a spec MUST (E.3.1), not an existing artefact. Grep for rotation-step configuration (rotation.?step / rotationStep / rotation_step, case-insensitive) over Berreman/OpticalConstructor returns 0 hits; Preferences (UserEnvironment.fs:94) carries no rotation-step field. No existing artefact resolves the 'already configurable' claim.",
      "verdict": "REFUTED-NO-POINTER"
    },
    {
      "id": 2,
      "phrase": "the configured key map MUST persist with the environment settings (Part I / UserEnvironment.fs:141). Reuse that settings store rather than adding a parallel preferences file.",
      "spec_location": "§E.8.1",
      "evidence": "Read UserEnvironment.fs:141 -> matched `type EnvironmentSettings` (the persistent user-environment settings store; favorites/recentFiles/layout/theme/preferences fields). Existing settings store confirmed at the cited pointer.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 3,
      "phrase": "The domain already enumerates element kinds (BeamTree.fs:35).",
      "spec_location": "§F.0",
      "evidence": "Read BeamTree.fs:35 -> matched `type ConstructorElement =` with cases Source/Polarizer/Sample/Lens/CurvedMirror/FlatMirror/Analyzer/Detector (lines 35-43). Element-kind enumeration exists at the cited pointer.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 4,
      "phrase": "Map catalogue kinds onto the existing `ConstructorElement` cases (BeamTree.fs:35) via the A.5.2 function",
      "spec_location": "§F.3",
      "evidence": "Read BeamTree.fs:35 -> `type ConstructorElement` with DU cases at lines 36-43. Existing ConstructorElement cases confirmed at the cited pointer.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 5,
      "phrase": "do not add new element DU cases for LP/CP (they are `Polarizer`)",
      "spec_location": "§F.3",
      "evidence": "Read BeamTree.fs:37 -> matched `| Polarizer` case within the `ConstructorElement` union (BeamTree.fs:35). Reuse-target case exists.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 6,
      "phrase": "do not remove `Analyzer` (BeamTree.fs:42)",
      "spec_location": "§F.3",
      "evidence": "Read BeamTree.fs:42 -> matched `| Analyzer` case within the `ConstructorElement` union. Cited file and line both match.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 7,
      "phrase": "building on the existing favourites concept (`FavoriteGroup`, UserEnvironment.fs:126)",
      "spec_location": "§G.1.3",
      "evidence": "Read UserEnvironment.fs:126 -> matched `type FavoriteGroup = { name : string; pins : FavoritePin list }`. Existing favourites type confirmed at the cited pointer.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 8,
      "phrase": "consistent with each branch carrying its own field, BeamTree.fs:98",
      "spec_location": "§G.2.1",
      "evidence": "Read BeamTree.fs:98 -> matched `let branchEmField (branch : BeamBranch) (ems : EmFieldSystem) : EmField`, which returns `ems.reflected`/`ems.transmitted` per branch; the doc comment (lines 94-97) states each branch carries its own field state so detectors on different branches report independent results. Cited file and line both match.",
      "verdict": "CONFIRMED"
    }
  ]
}
