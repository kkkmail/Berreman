namespace OpticalConstructor.Domain

open Berreman.Geometry                              // Angle (the placement/orientation summary)
open OpticalConstructor.Domain.Placement           // CatalogueKind / Emission / ElementPlacement / TablePoint
open OpticalConstructor.Domain.Library              // ElementId / DetectorKind / SampleVersionId / SampleId
open OpticalConstructor.Domain.Lifecycle           // VersionRef / VersionsInUse (step 020)

/// Spec 0027 (028) / spec 0038 Part I (step 025) — the Experiments domain, redesigned around a
/// multi-step, EDITABLE experiment built from the live setup:
///   1. the SETUP is the scene itself — captured on commit as an ordered `ElementDescriptor` list
///      (each descriptor: the catalogue kind, a placement/orientation summary, and the bound entry
///      reference BY VERSION where versioned) with the varied element identified WITHIN the list;
///   2. the user picks WHICH element to vary — and the element's CATALOGUE KIND determines what may
///      be varied. This is DATA (`variablesFor`), attached to the element through its kind, NOT
///      hard-coded at the bay: a light source varies only its wavelength, a polarizer only its R1, a
///      sample its R1 and R2 (for now), and other kinds nothing yet;
///   3. the user picks what to CAPTURE (transmitted / reflected / both), sets the numeric range, and
///      CONFIRMS, which ADDS the experiment to a collection that persists. Experiments can be EDITED
///      (re-confirm updates in place — never duplicates) and REMOVED.
/// Sample and light source are OPTIONAL (spec 0038 Part I): `E1 = LS+LP+S+LP(rotate)`,
/// `E2 = LS+LP+LP(rotate)` (no sample), `E3 =` an empty dark line are all first-class. The detector
/// kind is part of the chain and determines the expected data-file shape (consumed by step 34). Every
/// primitive is elevated (the variable, the measurement, the id, the range, the bound-entry version
/// reference and the data-file path are DUs / records, not bare strings / floats), and an experiment
/// references its elements by serializable `ElementId` / `VersionRef`, so it survives save-load.
///
/// Moved out of `ElementId.fs` at step 025: the descriptor binding reuses the step-020 `VersionRef`
/// (`Lifecycle.fs`, which compiles AFTER `ElementId.fs`), so the module compiles after `Lifecycle.fs`.
/// The fully-qualified module name is unchanged, so every `open …Experiments` consumer is untouched.
module Experiments =

    /// A quantity an experiment can vary on an element (spec 028). An elevated DU — not an enum / string.
    type VariableParameter =
        | VaryWaveLength
        | VaryR1
        | VaryR2

        /// A short, stable code (the automation ids and the Controls-layer mirror map back through this).
        member this.code : string =
            match this with
            | VaryWaveLength -> "wavelength"
            | VaryR1 -> "r1"
            | VaryR2 -> "r2"

        /// A human-readable name for the varied quantity.
        member this.label : string =
            match this with
            | VaryWaveLength -> "Wavelength"
            | VaryR1 -> "Rotation R1"
            | VaryR2 -> "Incidence R2"

        /// The display unit of the varied quantity.
        member this.unitLabel : string =
            match this with
            | VaryWaveLength -> "nm"
            | VaryR1 | VaryR2 -> "°"

    /// What each catalogue kind permits varying (spec 028) — declared HERE as DATA (attached to the element
    /// through its kind) so the Experiments bay READS the allowed set rather than hard-coding it. A light
    /// source varies only its wavelength; a linear / circular polarizer only its R1; a sample its R1 and R2
    /// (for now); a lens / mirror / detector expose nothing to vary yet (future kinds add cases here).
    let variablesFor (kind : CatalogueKind) : VariableParameter list =
        match kind with
        | LightSource -> [ VaryWaveLength ]
        | LinearPolarizer
        | CircularPolarizer -> [ VaryR1 ]
        | Sample -> [ VaryR1; VaryR2 ]
        | Lens
        | FlatMirror
        | CurvedMirror
        | Detector -> []

    /// What an experiment CAPTURES at the detector (spec 028): the transmitted branch, the reflected
    /// branch, or both. A DU — not a bool / enum — that REPLACES the previously hard-coded "always T"
    /// (samples) and "mirror ⇒ R"; it defaults from the varied element's `Emission` via `ofEmission`.
    type MeasurementMode =
        | CaptureTransmitted
        | CaptureReflected
        | CaptureBoth

        member this.code : string =
            match this with
            | CaptureTransmitted -> "t"
            | CaptureReflected -> "r"
            | CaptureBoth -> "both"

        member this.label : string =
            match this with
            | CaptureTransmitted -> "Transmitted (T)"
            | CaptureReflected -> "Reflected (R)"
            | CaptureBoth -> "Both (T + R)"

        member this.capturesTransmitted : bool =
            match this with
            | CaptureTransmitted | CaptureBoth -> true
            | CaptureReflected -> false

        member this.capturesReflected : bool =
            match this with
            | CaptureReflected | CaptureBoth -> true
            | CaptureTransmitted -> false

        /// The natural default measurement for an element, from its emission metadata (spec 028: a mirror
        /// emits only its reflected branch, everything else both — so a varied mirror defaults to R, a
        /// varied sample to both; the user can still switch it).
        static member ofEmission (e : Emission) : MeasurementMode =
            match e.emitsReflected, e.emitsTransmitted with
            | true, true -> CaptureBoth
            | true, false -> CaptureReflected
            | false, _ -> CaptureTransmitted

    /// A numeric variation range: [min, max] over `points` samples, in the varied quantity's display unit.
    type VariableRange =
        {
            min : float
            max : float
            points : int
        }

        /// The default range for a variable (spec 028): R1 0…360° (73 pts); R2 0…89° incidence (91 pts —
        /// 90° itself is not computable, so the top is 89, drawn to 90 by the chart); wavelength 200…800 nm
        /// (91 pts).
        static member forVariable (v : VariableParameter) : VariableRange =
            match v with
            | VaryR1 -> { min = 0.0; max = 360.0; points = 73 }
            | VaryR2 -> { min = 0.0; max = 89.0; points = 91 }
            | VaryWaveLength -> { min = 200.0; max = 800.0; points = 91 }

    /// A stable identity for an experiment in the collection (a monotonic int — deterministic, so add /
    /// edit / remove are unit-testable without a Guid clock).
    type ExperimentId =
        | ExperimentId of int

        member this.value = let (ExperimentId i) = this in i
        static member create (i : int) : ExperimentId = ExperimentId i

    /// An elevated filesystem path to an experiment's measured data file (spec 0038 Part I, step 025) —
    /// a single-case DU, never a bare `string` in the `Experiment` record or the serialized contract.
    /// `.value` is the raw path, reached ONLY at the IO boundary (the future `ExperimentDataProxy` file
    /// adapter, §15.4). Mirrors the sibling `Library.ElementId` (a `create` + `.value`, no `tryCreate`):
    /// parse-time validation of a picked path lands with the data-file parsers (Part L).
    type DataFilePath =
        | DataFilePath of string

        member this.value = let (DataFilePath p) = this in p
        static member create (path : string) : DataFilePath = DataFilePath path

    /// How an experiment descriptor references its bound Library entry (spec 0038 Part I / §13.1). A
    /// versioned entry pins the EXACT version it was built against (a sample by `SampleVersionRef`, a
    /// material-backed binding by `MaterialVersionRef`) so a later mint of the entry's `.next` version
    /// never rewrites the experiment's physics; a protected preset (source / detector / polarizer — never
    /// versioned) binds by its stable entry id; an element the user has not bound yet is `Unbound`. Reuses
    /// the step-020 `VersionRef` (`Lifecycle.fs`) rather than a parallel version-ref DU — the same
    /// reference the `VersionsInUse` seam collects.
    type ElementBinding =
        | BoundByVersion of VersionRef
        | BoundByEntryId of entryId : string
        | Unbound

        /// The versioned reference this binding pins, if any — the projection `boundVersions` folds over
        /// the whole setup (preset / unbound descriptors contribute none).
        member this.versionRefOpt : VersionRef option =
            match this with
            | BoundByVersion versionRef -> Some versionRef
            | BoundByEntryId _ | Unbound -> None

    /// A serializable snapshot of an element's placement / orientation captured on commit (spec 0038
    /// Part I): the table position and the three rotation angles. Elevated fields only (`TablePoint`,
    /// engine `Angle`), so the descriptor survives the live element's removal and a save-load without
    /// holding an in-memory element reference. Built from the live `ElementPlacement` via `ofPlacement`.
    type PlacementSummary =
        {
            position : TablePoint
            r1 : Angle
            r2 : Angle
            r3 : Angle
        }

        static member ofPlacement (p : ElementPlacement) : PlacementSummary =
            { position = p.placementPoint; r1 = p.r1; r2 = p.r2; r3 = p.r3 }

    /// One element of an experiment's captured setup (spec 0038 Part I): its stable serializable id and a
    /// human label (captured at commit-time so the row survives the element's later removal), its
    /// catalogue kind, a placement/orientation summary, and the bound entry reference (by version where
    /// versioned). The ordered list of these on an `Experiment` IS the setup — the light-traversal chain
    /// including the detector, whose kind determines the expected data-file shape (§15.3, consumed by
    /// step 34).
    type ElementDescriptor =
        {
            elementId : ElementId
            label : string
            kind : CatalogueKind
            placement : PlacementSummary
            binding : ElementBinding
        }

    /// The element an experiment varies, identified WITHIN the setup (spec 0038 Part I): its serializable
    /// id (resolvable to a descriptor in `setup`) and the swept quantity. An experiment with NO varied
    /// element (`Experiment.varied = None`) is a dark-line / calibration measurement (E3).
    type VariedElement =
        {
            elementId : ElementId
            variable : VariableParameter
        }

    /// A fully-specified, editable experiment (spec 028 / spec 0038 Part I): the ordered `setup` captured
    /// from the live scene (kind + placement/orientation summary + versioned binding per element, the
    /// detector included), the varied element identified within that setup (`None` for a dark line), the
    /// capture mode, the numeric range, and an optional attached measured-data file. The single
    /// `elementId`/`elementLabel` pair of the old shape is GONE — the varied element is one entry of the
    /// setup list.
    type Experiment =
        {
            id : ExperimentId
            setup : ElementDescriptor list
            varied : VariedElement option
            measurement : MeasurementMode
            range : VariableRange
            dataFileOpt : DataFilePath option
        }

        /// The descriptor of the varied element within the setup (`None` for a dark line, or if the
        /// varied id is somehow absent from the captured setup). The setup is a frozen snapshot including
        /// the varied element, so for a well-formed experiment this always resolves.
        member this.variedDescriptor : ElementDescriptor option =
            match this.varied with
            | Some v -> this.setup |> List.tryFind (fun d -> d.elementId = v.elementId)
            | None -> None

        /// The human label of the varied element (from its captured descriptor), or "" for a dark line.
        member this.variedLabel : string =
            this.variedDescriptor |> Option.map (fun d -> d.label) |> Option.defaultValue ""

        /// The detector element of the chain, if the setup captured one — the descriptor whose kind is
        /// `Detector`. Its binding (a protected-preset entry id) resolves through the Library to the
        /// `DetectorKind` (Intensity vs Ellipsometer) that fixes the expected data-file shape (step 34).
        member this.detectorDescriptorOpt : ElementDescriptor option =
            this.setup |> List.tryFind (fun d -> d.kind = Detector)

        /// A short, human-readable description (the collection row + readout use this). A varied
        /// experiment names the swept element / quantity / range / capture; a dark line names its element
        /// count and capture.
        member this.description : string =
            match this.varied with
            | Some v ->
                $"%s{this.variedLabel}: vary %s{v.variable.label} over %g{this.range.min}…%g{this.range.max} %s{v.variable.unitLabel} (%d{this.range.points} pts), capture %s{this.measurement.label}"
            | None ->
                $"Dark line (%d{List.length this.setup} element(s)), capture %s{this.measurement.label}"

    /// Every versioned reference the experiment's setup binds (spec 0038 Part I): each descriptor
    /// binding projected to its `VersionRef` (preset / unbound descriptors contribute none).
    let versionRefsOf (experiment : Experiment) : VersionRef list =
        experiment.setup |> List.choose (fun d -> d.binding.versionRefOpt)

    /// The set of versioned references currently bound across a live experiment collection (spec 0038
    /// Part I / §13.1). This is the step-020 "versions in use" set — a version in it is IMMUTABLE (its
    /// physics change mints the next version) and un-deletable. Pure and unit-testable.
    let boundVersions (experiments : Experiment list) : Set<VersionRef> =
        experiments |> List.collect versionRefsOf |> Set.ofList

    /// Build the step-020 `VersionsInUse` seam over a live source of experiments (spec 0038 Part I): the
    /// composition root injects `fun () -> currentExperiments` so the material / sample stores' saves
    /// (steps 21/22) see the live collection each time they consult it. Replaces the `VersionsInUse.empty`
    /// the stores took while nothing bound a version.
    let versionsInUseSeam (currentExperiments : unit -> Experiment list) : VersionsInUse =
        { versionsInUse = fun () -> boundVersions (currentExperiments ()) }

    /// The in-progress experiment being built or edited (spec 028, the multi-step editor). `elementId` /
    /// `variable` are `None` until chosen; the interactive add needs both. When `editingId` is `Some` the
    /// next `commit` UPDATES that stored experiment (never duplicates); otherwise it APPENDS a new one.
    type ExperimentDraft =
        {
            elementId : ElementId option
            elementLabel : string
            variable : VariableParameter option
            measurement : MeasurementMode
            range : VariableRange
            editingId : ExperimentId option
        }

        static member empty : ExperimentDraft =
            {
                elementId = None
                elementLabel = ""
                variable = None
                measurement = CaptureTransmitted
                range = VariableRange.forVariable VaryR1
                editingId = None
            }

    /// The editable collection of experiments plus the id counter and the live draft (spec 028). Pure —
    /// choose / commit / edit / remove are unit-tested without any UI.
    type ExperimentCollection =
        {
            experiments : Experiment list
            nextId : int
            draft : ExperimentDraft
        }

        static member empty : ExperimentCollection =
            { experiments = []; nextId = 1; draft = ExperimentDraft.empty }

    /// Whether the draft is complete enough for the interactive add (an element AND a variable are
    /// chosen). Dark-line experiments (no varied element) are constructed directly, not through the bay's
    /// gated add — the bay disables Add when this is false.
    let canCommit (c : ExperimentCollection) : bool =
        match c.draft.elementId, c.draft.variable with
        | Some _, Some _ -> true
        | _ -> false

    /// Start a brand-new draft (clears the editor and the editing cursor).
    let newDraft (c : ExperimentCollection) : ExperimentCollection =
        { c with draft = ExperimentDraft.empty }

    /// Choose the element to vary (spec 028 step 2). The allowed variables (`variablesFor kind`) and the
    /// default measurement (from the element's emission) are supplied by the host; the draft's variable
    /// defaults to the first allowed one (`None` when the element exposes nothing to vary), and its range
    /// to that variable's default.
    let chooseElement
        (id : ElementId)
        (label : string)
        (allowed : VariableParameter list)
        (defaultMeasurement : MeasurementMode)
        (c : ExperimentCollection) : ExperimentCollection =
        let variable = List.tryHead allowed
        let range = variable |> Option.map VariableRange.forVariable |> Option.defaultValue c.draft.range
        { c with
            draft =
                { c.draft with
                    elementId = Some id
                    elementLabel = label
                    variable = variable
                    measurement = defaultMeasurement
                    range = range } }

    /// Choose the varied quantity (spec 028). Resets the range to that variable's default.
    let chooseVariable (v : VariableParameter) (c : ExperimentCollection) : ExperimentCollection =
        { c with draft = { c.draft with variable = Some v; range = VariableRange.forVariable v } }

    /// Choose what the experiment captures (T / R / both, spec 028).
    let chooseMeasurement (m : MeasurementMode) (c : ExperimentCollection) : ExperimentCollection =
        { c with draft = { c.draft with measurement = m } }

    let setRangeMin (v : float) (c : ExperimentCollection) : ExperimentCollection =
        { c with draft = { c.draft with range = { c.draft.range with min = v } } }

    let setRangeMax (v : float) (c : ExperimentCollection) : ExperimentCollection =
        { c with draft = { c.draft with range = { c.draft.range with max = v } } }

    let setRangePoints (n : int) (c : ExperimentCollection) : ExperimentCollection =
        { c with draft = { c.draft with range = { c.draft.range with points = max 2 n } } }

    /// Confirm the draft (spec 028 step 3 / spec 0038 Part I), capturing the ordered `setup` the bay built
    /// from the live scene (kind + placement/orientation summary + versioned binding per element, the
    /// detector included). The varied element (`draft.elementId` + `draft.variable`) is identified within
    /// that setup. When editing an existing experiment (`editingId = Some`) this UPDATES it in place,
    /// preserving its attached data file; otherwise it APPENDS a new experiment — but only when the draft
    /// identifies an element AND a variable (the bay gates this via `canCommit`; a dark-line experiment is
    /// constructed directly, not through the interactive draft). Leaves the draft EDITING the appended
    /// experiment so a follow-up confirm updates rather than duplicates.
    let commit (setup : ElementDescriptor list) (c : ExperimentCollection) : ExperimentCollection =
        // Qualify the first field so the {elementId; variable} literal resolves to `VariedElement` and not
        // the also-in-scope `ExperimentDraft` (which shares both labels).
        let variedOpt =
            Option.map2 (fun id variable -> { VariedElement.elementId = id; variable = variable }) c.draft.elementId c.draft.variable
        match c.draft.editingId with
        | Some existing ->
            let experiments =
                c.experiments
                |> List.map (fun e ->
                    if e.id = existing then
                        { e with
                            setup = setup
                            varied = variedOpt
                            measurement = c.draft.measurement
                            range = c.draft.range }
                    else e)
            { c with experiments = experiments }
        | None ->
            match variedOpt with
            | Some _ ->
                let exp =
                    {
                        id = ExperimentId c.nextId
                        setup = setup
                        varied = variedOpt
                        measurement = c.draft.measurement
                        range = c.draft.range
                        dataFileOpt = None
                    }
                { c with
                    experiments = c.experiments @ [ exp ]
                    nextId = c.nextId + 1
                    draft = { c.draft with editingId = Some exp.id } }
            | None -> c

    /// Attach (or replace) the measured-data file of a stored experiment (spec 0038 Part I): the inverse
    /// flow gives each experiment one data file. A no-op for an unknown id. Pure — the picker IO that
    /// yields the `DataFilePath` lives at the edge (Part L).
    let attachDataFile (id : ExperimentId) (path : DataFilePath) (c : ExperimentCollection) : ExperimentCollection =
        let experiments =
            c.experiments
            |> List.map (fun e -> if e.id = id then { e with dataFileOpt = Some path } else e)
        { c with experiments = experiments }

    /// Load an existing experiment into the draft for editing (spec 028: "if the user chooses an experiment
    /// then the user can change anything in the experiment"). A later `commit` updates it in place. A
    /// dark-line experiment loads with no varied element (`elementId` / `variable` unset).
    let edit (id : ExperimentId) (c : ExperimentCollection) : ExperimentCollection =
        match c.experiments |> List.tryFind (fun e -> e.id = id) with
        | Some e ->
            { c with
                draft =
                    {
                        elementId = e.varied |> Option.map (fun v -> v.elementId)
                        elementLabel = e.variedLabel
                        variable = e.varied |> Option.map (fun v -> v.variable)
                        measurement = e.measurement
                        range = e.range
                        editingId = Some e.id
                    } }
        | None -> c

    /// Remove an experiment from the collection (spec 028). If it was the one being edited the draft resets.
    let remove (id : ExperimentId) (c : ExperimentCollection) : ExperimentCollection =
        let experiments = c.experiments |> List.filter (fun e -> e.id <> id)
        let draft = if c.draft.editingId = Some id then ExperimentDraft.empty else c.draft
        { c with experiments = experiments; draft = draft }

    /// The Experiments error channel (errors as values; each case carries a `reason`).
    type ExperimentError =
        | UnknownExperiment of reason : string
        | ExperimentUnavailable of reason : string

    /// The mock Experiments IO seam (the functional-proxy convention): a record of camelCase
    /// `Result`-returning functions. A test substitutes a stub of the SAME shape. Function-valued
    /// fields have no structural equality, so the proxy compares by reference — letting a host model
    /// that holds the proxy keep its (Elmish-required) equality.
    [<ReferenceEquality>]
    type ExperimentProxy =
        {
            listExperiments : unit -> Result<Experiment list, ExperimentError>
            tryGetExperiment : int -> Result<Experiment option, ExperimentError>
        }

    /// A dark-line seed template (spec 0038 Part I): an EMPTY setup with nothing varied — the "how much
    /// ambient light reaches the detector with no source" calibration (operator, 001). Listable; the live
    /// bay builds real experiments against the present scene.
    let seedExperiments : Experiment list =
        [
            {
                id = ExperimentId 1
                setup = []
                varied = None
                measurement = CaptureTransmitted
                range = VariableRange.forVariable VaryR1
                dataFileOpt = None
            }
        ]

    /// The in-memory mock proxy (spec §3 / Q7): closes over the seed templates, no IO, deterministic for
    /// tests. A real disk-backed `create` would live in `OpticalConstructor.Storage`, leaving the
    /// bay / logic unchanged.
    let createInMemory () : ExperimentProxy =
        let seeds = seedExperiments
        {
            listExperiments = fun () -> Ok seeds
            tryGetExperiment = fun i -> Ok (seeds |> List.tryFind (fun e -> e.id.value = i))
        }
