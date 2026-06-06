{
  "claims": [
    {
      "id": 1,
      "phrase": "layered on top of the existing Avalonia + FuncUI (Elm-style MVU) desktop app, that reuses the existing beam-tree physics, units, JSON storage, and snapshot undo/redo rather than re-inventing them",
      "spec_location": "intro paragraph (lines 1-10)",
      "evidence": "Intro summary; each named subsystem resolves and is concretely pointered later in this same chunk (§0.1 BeamTree.fs, §0.2 Units.fs, §0.7 ProjectFile.fs/History.fs). Glob confirms Berreman/OpticalConstructor/{OpticalConstructor.Domain,OpticalConstructor.Storage,OpticalConstructor.Ui} all exist.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 2,
      "phrase": "The constructor MUST drive the existing beam tree (Berreman/OpticalConstructor/OpticalConstructor.Domain/BeamTree.fs:35 defines ConstructorElement; :53 defines BeamNode; :91 solve; :117 routeAndSolve) and the project aggregate (Berreman/OpticalConstructor/OpticalConstructor.Domain/Project.fs:26, OpticalConstructorProject)",
      "spec_location": "§0.1",
      "evidence": "Read BeamTree.fs:35 -> 'type ConstructorElement'; :53 -> 'type BeamNode'; :91 -> 'let solve (node : BeamNode)'; :117 -> 'let routeAndSolve'. Read Project.fs:26 -> 'type OpticalConstructorProject'. All five pointers exact.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 3,
      "phrase": "matching the units spine (Berreman/OpticalConstructor/OpticalConstructor.Domain/Units.fs:20, UnitOfMeasure; :43 toMeters; :71 fromMeters) ... a per-object preferred display unit (display metadata only, mirroring BeamNode.defaultUnit, BeamTree.fs:59)",
      "spec_location": "§0.2",
      "evidence": "Read Units.fs:20 -> 'type UnitOfMeasure'; :43 -> 'let toMeters'; :71 -> 'let fromMeters'. Read BeamTree.fs:59 -> 'defaultUnit : Units.UnitOfMeasure' inside BeamNode record. Phrase 'mirroring' inferred as assumption-shape (existing-artefact reference); all pointers exact.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 4,
      "phrase": "matching the existing root model (Berreman/OpticalConstructor/OpticalConstructor.Ui/Shell.fs:94, RootModel)",
      "spec_location": "§0.3",
      "evidence": "Read Shell.fs:94 -> 'type RootModel' (with [<ReferenceEquality>] on :93). Pointer exact.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 5,
      "phrase": "The audit-gated FuncUI clone at C:/GitHub/Avalonia.FuncUI.Clone/ MUST NOT be referenced, linked, or built (Berreman/OpticalConstructor/OpticalConstructor.Ui/AppShell.fs:8 records the gate)",
      "spec_location": "§0.3",
      "evidence": "Read AppShell.fs:8 -> '**FuncUI-clone gate (constraint 5 / §A.6 / §A.9 / AC-J8).**' stating the clone at C:\\GitHub\\Avalonia.FuncUI.Clone\\ MUST NOT be referenced/linked/built. fsproj grep confirms only the public 'Avalonia.FuncUI' v1.6.0 NuGet is referenced (Ui/App .fsproj), never the clone. Pointer exact.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 6,
      "phrase": "every geometry rule is provable headless, exactly as the existing schematic does (Berreman/OpticalConstructor/OpticalConstructor.Ui/Schematic.fs:18)",
      "spec_location": "§0.3",
      "evidence": "Read Schematic.fs:18 -> 'module OpticalConstructor.Ui.Schematic'; header comment confirms pure headless geometry values with the FuncUI/Avalonia view deferred. Pointer exact.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 7,
      "phrase": "a schema-version mismatch is a validation error, matching Berreman/OpticalConstructor/OpticalConstructor.Storage/schema/optical-constructor-project.schema.json:12",
      "spec_location": "§0.6",
      "evidence": "Read schema.json:12 -> description 'A differing value is a validation error, not a migration trigger (Part I, out-of-scope migration).' under the schemaVersion const. Phrase 'matching <file>:line' inferred as existing-artefact reference; pointer exact.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 8,
      "phrase": "EN/RU localization (no CultureInfo-based localization, ResourceManager, or .resx exists; the only CultureInfo uses are InvariantCulture number parsing) ... net-new (verified absent today)",
      "spec_location": "§0.7",
      "evidence": "Grep ResourceManager|\\.resx|CultureInfo over Berreman/OpticalConstructor: zero ResourceManager hits, zero .resx (Glob Berreman/OpticalConstructor/**/Placement.fs and *.resx -> none); all CultureInfo hits in the OpticalConstructor solution are CultureInfo.InvariantCulture (Export.fs:41, MaterialImport.fs:34, SpectralImport.fs:25, InverseFit.fs:56, SourceView.fs:77). The MathNetNumerics hits are vendored third-party code, out of the solution. Absence claim holds.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 9,
      "phrase": "The following are reused as-is: the beam-tree solver (BeamTree.fs:91), the units spine (Units.fs), JSON project storage (ProjectFile.fs:20 saveProject, :31 openProject), schema validation (SchemaValidation.fs:61 validate), snapshot undo/redo (History.fs:20 EditHistory), and the environment/settings store (UserEnvironment.fs:141 EnvironmentSettings)",
      "spec_location": "§0.7",
      "evidence": "Read BeamTree.fs:91 -> 'let solve'; Units.fs exists. ProjectFile.fs:20 -> 'let saveProject'; :31 -> 'let openProject'. SchemaValidation.fs:61 -> 'let validate'. History.fs:20 -> 'type EditHistory'. UserEnvironment.fs:141 -> 'type EnvironmentSettings'. All pointers exact.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 10,
      "phrase": "Today an element on the beam tree (BeamNode, BeamTree.fs:53) has no spatial placement: it carries its kind, its solved OpticalSystem, its incident light, its branch children, and a display unit, but no position, no orientation, and no bounding geometry",
      "spec_location": "§A.0",
      "evidence": "Read BeamTree.fs:53 -> 'type BeamNode' with fields element, system (OpticalSystem), incident (IncidentLightInfo), children (Map<BeamBranch,BeamNode>), defaultUnit; no placement/position/orientation/box field present. Current-state claim holds.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 11,
      "phrase": "r1, r2, r3 ... each carried as the engine's Angle value so the existing angle constructors are reused, not re-derived",
      "spec_location": "§A.1.1",
      "evidence": "Class-anchored on Angle. Grep 'type Angle' -> unique match in Berreman/Berreman/Geometry.fs (engine geometry module); spec/claim-file hits aside, it is the sole source definition. Existing Angle type confirmed.",
      "verdict": "CONFIRMED"
    }
  ]
}
