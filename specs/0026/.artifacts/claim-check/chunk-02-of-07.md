{
  "claims": [
    {
      "id": 1,
      "phrase": "mapping each catalogue kind to its existing domain case (BeamTree.fs:35): Light Source -> `Source`",
      "spec_location": "§A.5.2",
      "evidence": "Read BeamTree.fs:35 -> `type ConstructorElement =`; `Source` case present at line 36",
      "verdict": "CONFIRMED"
    },
    {
      "id": 2,
      "phrase": "both Linear Polarizer and Circular Polarizer -> `Polarizer` (BeamTree.fs:37)",
      "spec_location": "§A.5.2",
      "evidence": "Read BeamTree.fs:37 -> `| Polarizer`",
      "verdict": "CONFIRMED"
    },
    {
      "id": 3,
      "phrase": "The existing `Analyzer` case (BeamTree.fs:42) MUST NOT be surfaced as a catalogue kind and MUST be left untouched",
      "spec_location": "§A.5.2",
      "evidence": "Read BeamTree.fs:42 -> `| Analyzer` (present in ConstructorElement DU)",
      "verdict": "CONFIRMED"
    },
    {
      "id": 4,
      "phrase": "consistent with the existing mirror rule that a mirror carries the reflected branch only -- `BeamNode.attach`, BeamTree.fs:74",
      "spec_location": "§A.7.2",
      "evidence": "Read BeamTree.fs:74 -> `static member attach`; body returns `Error MirrorBranchMustBeReflected` for (isMirror, Transmitted), enforcing reflected-only",
      "verdict": "CONFIRMED"
    },
    {
      "id": 5,
      "phrase": "The `constructorElement` `$def` is currently permissive (`true`, schema file `:201`)",
      "spec_location": "§A.8.1",
      "evidence": "Read optical-constructor-project.schema.json:201 -> `\"constructorElement\": true`",
      "verdict": "CONFIRMED"
    },
    {
      "id": 6,
      "phrase": "without renaming any of the nine reserved anchors (Project.fs:42)",
      "spec_location": "§A.8.1",
      "evidence": "Read Project.fs:42 -> `let schemaDefAnchors =`; list holds exactly nine entries (opticalSystem, layer, opticalProperties, beamNode, beamBranch, constructorElement, materialEntry, sourceSpec, unitOfMeasure); cited count 9 matches live count",
      "verdict": "CONFIRMED"
    },
    {
      "id": 7,
      "phrase": "The project aggregate `OpticalConstructorProject` (Project.fs:26) MUST be extended to carry the table and per-element placement",
      "spec_location": "§A.8.2",
      "evidence": "Read Project.fs:26 -> `type OpticalConstructorProject =`",
      "verdict": "CONFIRMED"
    },
    {
      "id": 8,
      "phrase": "The placement data MUST round-trip through the existing serialization (ProjectFile.fs:20/:31)",
      "spec_location": "§A.8.2",
      "evidence": "Read ProjectFile.fs:20 -> `saveProject` (serialize + write); ProjectFile.fs:31 -> `openProject` (read + deserialize) -- the save/load round-trip seam",
      "verdict": "CONFIRMED"
    },
    {
      "id": 9,
      "phrase": "and pass schema validation (SchemaValidation.fs:61)",
      "spec_location": "§A.8.2",
      "evidence": "Read SchemaValidation.fs:61 -> `let validate (element : JsonElement) : Result<unit, StorageError>`",
      "verdict": "CONFIRMED"
    },
    {
      "id": 10,
      "phrase": "For angles use the engine's existing `Angle` type and its constructors rather than introducing a new angle type",
      "spec_location": "§A.9",
      "evidence": "class-anchored `Angle`; Grep `type Angle` returns exactly one production match -> Geometry.fs:34 `type Angle =`",
      "verdict": "CONFIRMED"
    },
    {
      "id": 11,
      "phrase": "For canonical-meter values reuse the `Units` conversions (Units.fs:43/:71)",
      "spec_location": "§A.9",
      "evidence": "Read Units.fs:43 -> `let toMeters (u : UnitOfMeasure) (x : float) : float<meter>`; Units.fs:71 -> `let fromMeters (u : UnitOfMeasure) (m : float<meter>) : float` -- the two canonical-meter conversion functions",
      "verdict": "CONFIRMED"
    },
    {
      "id": 12,
      "phrase": "rather than overloading `BeamNode` (BeamTree.fs:53) with a flag-heavy spatial payload",
      "spec_location": "§A.9",
      "evidence": "Read BeamTree.fs:53 -> `type BeamNode =`",
      "verdict": "CONFIRMED"
    },
    {
      "id": 13,
      "phrase": "the quantitative result at each element still comes from the existing beam-tree solver (BeamTree.fs:91)",
      "spec_location": "§B.0",
      "evidence": "Read BeamTree.fs:91 -> `let solve (node : BeamNode) : EmFieldSystem =` (calls OpticalSystemSolver)",
      "verdict": "CONFIRMED"
    },
    {
      "id": 14,
      "phrase": "the drawn branches MUST stay consistent with the reflected/transmitted branches the engine computes (BeamTree.fs:98 `branchEmField`)",
      "spec_location": "§B.0",
      "evidence": "Read BeamTree.fs:98 -> `let branchEmField (branch : BeamBranch) (ems : EmFieldSystem) : EmField =`",
      "verdict": "CONFIRMED"
    },
    {
      "id": 15,
      "phrase": "MUST correspond to the engine branches (`Reflected`/`Transmitted`, BeamTree.fs:21)",
      "spec_location": "§B.2.1",
      "evidence": "Read BeamTree.fs:21 -> `type BeamBranch =` with cases `Reflected` (line 22) and `Transmitted` (line 23)",
      "verdict": "CONFIRMED"
    }
  ]
}
