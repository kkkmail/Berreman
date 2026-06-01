namespace OpticalConstructor.Domain

open Berreman.Media

/// The root project aggregate (§A.7). This slice realizes §A.7's domain share:
/// the aggregate record with the fields whose types already exist (the §A.4 beam
/// tree and the per-element optical stacks reused as-is from the engine) and the
/// nine reserved JSON-Schema `$defs` anchor names. The serialization itself
/// (`System.Text.Json` + FSharp.SystemTextJson), the schema file
/// `optical-constructor-project.schema.json`, and validate-on-load
/// (JsonSchema.Net) are owned by slice 003 (storage core); nothing here
/// serializes the model.
module Project =

    /// The canonical project definition (§A.7). Introduced with only the fields
    /// whose types already exist:
    ///   * `beamTree` — the NET-NEW §A.4 orchestration aggregate.
    ///   * `systems` — per-element optical stacks reused as-is from the engine
    ///     (`OpticalSystem`, `Media.fs:94`); NO alternate stack type.
    /// Later slices extend THIS record (and the schema) for new top-level
    /// components — materials (slice 004), sources (slice 007), chart settings
    /// (slice 012), sweeps/sidecar refs — each tracing to its own directive.
    /// Per the canonical-units rule every physical quantity reachable from here
    /// stays in the SI base (meters; `WaveLength.value`); per-element default
    /// units (`BeamNode.defaultUnit`, D.4 / B.8) are display metadata only.
    type OpticalConstructorProject =
        {
            beamTree : BeamTree.BeamTree
            systems : OpticalSystem list
            /// The source / illumination specifications (§A.7 / §E.8). This slice
            /// (007) owns the `sources` list semantics and the `sourceSpec` `$def`
            /// shape; each `SourceSpec` expands into weighted `IncidentLightInfo`
            /// values combined incoherently on `StokesVector` output (Part E).
            sources : SourceSpec.SourceSpec list
        }

    /// The nine JSON-Schema `$defs` anchor names reserved by §A.7. Parts B–J
    /// fill these `$def` shapes (e.g. `unitOfMeasure` is filled by D.1 as a
    /// string `enum` of the seven `UnitOfMeasure` member names); they MUST NOT
    /// rename the anchors. The schema document carrying these `$defs` is authored
    /// by slice 003.
    let schemaDefAnchors =
        [
            "opticalSystem"
            "layer"
            "opticalProperties"
            "beamNode"
            "beamBranch"
            "constructorElement"
            "materialEntry"
            "sourceSpec"
            "unitOfMeasure"
        ]
