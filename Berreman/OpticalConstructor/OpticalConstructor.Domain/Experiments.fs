namespace OpticalConstructor.Domain

open OpticalConstructor.Domain.Kit
open Berreman.Fields                                // WaveLength (a tunable source's set wavelength, spec 0046)
open OpticalConstructor.Domain.Placement           // CatalogueKind / Emission
open OpticalConstructor.Domain.Library              // ElementId
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.Propagation         // SweptValue / CurvePointCount (a range's ends and points)

/// The editable experiments, each a run recorded over one revision of a kit setup: the varied slot, the capture and
/// the range are the experiment's, the slots the setup's (spec 0028; spec 0038 Part I; spec 0050 S8.2).
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
        static member allowedFor (kind : CatalogueKind) : VariableParameter list =
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
            min : SweptValue
            max : SweptValue
            points : CurvePointCount
        }

        /// The default range for a variable (spec 028): R1 0…360° (73 pts); R2 0…89° incidence (91 pts —
        /// 90° itself is not computable, so the top is 89, drawn to 90 by the chart); wavelength 200…800 nm
        /// (91 pts).
        static member forVariable (v : VariableParameter) : VariableRange =
            match v with
            | VaryR1 -> { min = SweptValue 0.0; max = SweptValue 360.0; points = CurvePointCount 73 }
            | VaryR2 -> { min = SweptValue 0.0; max = SweptValue 89.0; points = CurvePointCount 91 }
            | VaryWaveLength -> { min = SweptValue 200.0; max = SweptValue 800.0; points = CurvePointCount 91 }

    /// A stable identity for an experiment in the collection (a monotonic int — deterministic, so add /
    /// edit / remove are unit-testable without a Guid clock).
    type ExperimentId =
        | ExperimentId of int

        member this.value = let (ExperimentId i) = this in i
        static member create (i : int) : ExperimentId = ExperimentId i

        member this.next : ExperimentId = ExperimentId (this.value + 1)

        /// Names an experiment's kit file by its identity, E<n> (spec 0050 S4.3).
        member this.fileStem : FileStem = FileStem.ofText $"E{this.value}"

    /// What a recorded run is for; its disk tag is an int, 1 to 7 in case order (spec 0050 S9.1, 001 §K).
    type ExperimentRole =
        | ScienceRun
        | CalibrationRun
        | DarkRun
        | DriftCheckRun
        | DiagnosticRun
        | IgnoredRun
        | PlannedRun

        member this.value : int =
            match this with
            | ScienceRun -> 1
            | CalibrationRun -> 2
            | DarkRun -> 3
            | DriftCheckRun -> 4
            | DiagnosticRun -> 5
            | IgnoredRun -> 6
            | PlannedRun -> 7

        /// The role a disk tag names; any other tag is `None`, the caller's typed error (spec 0050 S9.1).
        static member tryOfWire (tag : int) : ExperimentRole option =
            match tag with
            | 1 -> Some ScienceRun
            | 2 -> Some CalibrationRun
            | 3 -> Some DarkRun
            | 4 -> Some DriftCheckRun
            | 5 -> Some DiagnosticRun
            | 6 -> Some IgnoredRun
            | 7 -> Some PlannedRun
            | _ -> None

        member this.text : string =
            match this with
            | ScienceRun -> "Science"
            | CalibrationRun -> "Calibration"
            | DarkRun -> "Dark"
            | DriftCheckRun -> "Drift check"
            | DiagnosticRun -> "Diagnostic"
            | IgnoredRun -> "Ignored"
            | PlannedRun -> "Planned"

        /// Every role in tag order (spec 0050 S9.1).
        static member all : ExperimentRole list =
            [ ScienceRun; CalibrationRun; DarkRun; DriftCheckRun; DiagnosticRun; IgnoredRun; PlannedRun ]

    /// The measurement session a run belongs to when the user sets it by hand; S11.1's study splits the rest at
    /// its Diagnostic runs (spec 0050 S9.1).
    type SessionName =
        | SessionName of string

        member this.value : string = let (SessionName text) = this in text

        /// The text as given; an empty or white-space text names no session (spec 0050 S9.1).
        static member tryCreate (text : string) : SessionName option =
            if System.String.IsNullOrWhiteSpace text then None else Some (SessionName text)

    /// A reviewer's free-text notes on a recorded run; they live in its file (spec 0050 S9.1, 001 §E).
    type ExperimentNotes =
        | ExperimentNotes of string

        member this.value : string = let (ExperimentNotes text) = this in text

        static member none : ExperimentNotes = ExperimentNotes ""

    /// An element's human label as an experiment captures it ("<kind> #<n>"), so a row outlives the element.
    type ElementLabel =
        | ElementLabel of string

        member this.value = let (ElementLabel l) = this in l

        /// No element chosen yet (a fresh draft, a dark line).
        static member blank : ElementLabel = ElementLabel ""

    /// The slot an experiment varies, named by its id in the referenced setup revision, with its label as the experiment
    /// captured it so a row outlives the slot; `Experiment.varied = None` is a dark line (spec 0038 Part I; spec 0050 S8.2).
    type VariedElement =
        {
            elementId : ElementId
            label : ElementLabel
            variable : VariableParameter
        }

    /// A fully-specified, editable experiment: the setup revision it was recorded over, whose slots stay as they were
    /// (editing a referenced setup mints its next revision), the varied slot (`None` for a dark line), the capture, the
    /// range and an optional measured-data attachment (spec 0038 Part I; spec 0050 S8.2).
    type Experiment =
        {
            id : ExperimentId
            setup : SetupReference
            varied : VariedElement option
            measurement : MeasurementMode
            range : VariableRange
            /// The measured-data kit-relative path and its recorded hash (spec 0050 S4.2).
            dataFileOpt : DataAttachment option
            /// Spec 0046: the wavelength a TUNABLE source is set to while the experiment varies something
            /// else; `None` = the source's own setting (always the case for a fixed line).
            sourceWaveLengthOpt : WaveLength option
        }

        /// The varied slot's label as the experiment captured it, or blank for a dark line (spec 0050 S8.2).
        member this.variedLabel : ElementLabel =
            match this.varied with
            | Some varied -> varied.label
            | None -> ElementLabel.blank

        /// A short, human-readable description (the collection row + readout use this). A varied
        /// experiment names the swept element / quantity / range / capture; a dark line names its setup
        /// revision and capture (spec 0050 S8.2).
        member this.description : string =
            match this.varied with
            | Some v ->
                $"%s{this.variedLabel.value}: vary %s{v.variable.label} over %g{this.range.min.value}…%g{this.range.max.value} %s{v.variable.unitLabel} (%d{this.range.points.value} pts), capture %s{this.measurement.label}"
            | None ->
                $"Dark line on setup %s{this.setup.describe}, capture %s{this.measurement.label}"

        /// Copies into a kit holding `taken`, in order: a free id is kept, a clash takes the next of the highest id
        /// taken, incoming or given so far; only `id` changes (spec 0050 S6).
        static member renumberedFor (taken : ExperimentId list) (incoming : Experiment list) : Experiment list =
            let incomingIds = incoming |> List.map (fun experiment -> experiment.id)
            incoming
            |> List.fold (fun (given : ExperimentId list, renumbered : Experiment list) experiment ->
                let id =
                    if List.contains experiment.id taken || List.contains experiment.id given then
                        (List.max (taken @ incomingIds @ given)).next
                    else
                        experiment.id
                let copied =
                    {
                        experiment with id = id
                    }
                id :: given, copied :: renumbered) ([], [])
            |> snd
            |> List.rev

    /// One experiment's versioned .experiment-json content (spec 0050 S4.3).
    type ExperimentFile =
        {
            schemaVersion : FileSchemaVersion
            experiment : Experiment
        }

        /// The only supported experiment file schema version (spec 0050 S4.3).
        static member currentSchemaVersion : FileSchemaVersion = FileSchemaVersion 1

    type Experiment with

        /// Wraps the experiment in the current file schema (spec 0050 S4.3).
        member this.experimentFile : ExperimentFile =
            {
                schemaVersion = ExperimentFile.currentSchemaVersion
                experiment = this
            }

    /// The in-progress experiment being built or edited (spec 028, the multi-step editor). `elementId` /
    /// `variable` are `None` until chosen; the interactive add needs both. When `editingId` is `Some` the
    /// next `commit` UPDATES that stored experiment (never duplicates); otherwise it APPENDS a new one.
    type ExperimentDraft =
        {
            elementId : ElementId option
            elementLabel : ElementLabel
            variable : VariableParameter option
            measurement : MeasurementMode
            range : VariableRange
            editingId : ExperimentId option
            /// Spec 0046: the wavelength a tunable source is set to for this run (see `Experiment`).
            sourceWaveLengthOpt : WaveLength option
        }

        static member empty : ExperimentDraft =
            {
                elementId = None
                elementLabel = ElementLabel.blank
                variable = None
                measurement = CaptureTransmitted
                range = VariableRange.forVariable VaryR1
                editingId = None
                sourceWaveLengthOpt = None
            }

    /// The editable collection of experiments plus the id counter and the live draft (spec 028). Pure —
    /// choose / commit / edit / remove are unit-tested without any UI.
    type ExperimentCollection =
        {
            experiments : Experiment list
            nextId : ExperimentId
            draft : ExperimentDraft
        }

        static member empty : ExperimentCollection =
            { experiments = []; nextId = ExperimentId 1; draft = ExperimentDraft.empty }

        /// A loaded collection (spec 0038 Part L, 037): the next id advances past the loaded ids so a later add
        /// never collides, and the draft starts fresh.
        static member ofLoaded (experiments : Experiment list) : ExperimentCollection =
            {
                experiments = experiments
                nextId = ExperimentId ((experiments |> List.map (fun e -> e.id.value) |> List.fold max 0) + 1)
                draft = ExperimentDraft.empty
            }

        /// Whether the draft is complete enough for the interactive add (an element AND a variable are
        /// chosen). Dark-line experiments (no varied element) are constructed directly, not through the bay's
        /// gated add — the bay disables Add when this is false.
        member this.canCommit : bool =
            match this.draft.elementId, this.draft.variable with
            | Some _, Some _ -> true
            | _ -> false

        /// Start a brand-new draft (clears the editor and the editing cursor).
        member this.newDraft : ExperimentCollection =
            { this with draft = ExperimentDraft.empty }

    /// Choose the element to vary (spec 028 step 2). The allowed variables (`variablesFor kind`) and the
    /// default measurement (from the element's emission) are supplied by the host; the draft's variable
    /// defaults to the first allowed one (`None` when the element exposes nothing to vary), and its range
    /// to that variable's default.
    let chooseElement
        (id : ElementId)
        (label : ElementLabel)
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

    let setRangeMin (v : SweptValue) (c : ExperimentCollection) : ExperimentCollection =
        { c with draft = { c.draft with range = { c.draft.range with min = v } } }

    let setRangeMax (v : SweptValue) (c : ExperimentCollection) : ExperimentCollection =
        { c with draft = { c.draft with range = { c.draft.range with max = v } } }

    let setRangePoints (n : CurvePointCount) (c : ExperimentCollection) : ExperimentCollection =
        { c with draft = { c.draft with range = { c.draft.range with points = CurvePointCount (max 2 n.value) } } }

    /// Set (or clear, with `None`) the wavelength a tunable source runs at for the draft (spec 0046). The
    /// host clamps it to the source's limits.
    let setSourceWaveLength (w : WaveLength option) (c : ExperimentCollection) : ExperimentCollection =
        { c with draft = { c.draft with sourceWaveLengthOpt = w } }

    /// Set the whole variation range at once (spec 0046 — the host clamps a wavelength sweep to a tunable
    /// source's limits).
    let setRange (range : VariableRange) (c : ExperimentCollection) : ExperimentCollection =
        { c with draft = { c.draft with range = range } }

    /// Confirm the draft over the setup revision the table was recorded as (spec 028 step 3; spec 0050 S8.2), the
    /// varied slot named with the draft's label. When editing an existing experiment (`editingId = Some`) this UPDATES
    /// it in place, preserving its attached data file; otherwise it APPENDS a new experiment — but only when the draft
    /// identifies an element AND a variable (the bay gates this via `canCommit`; a dark-line experiment is
    /// constructed directly, not through the interactive draft). Leaves the draft EDITING the appended
    /// experiment so a follow-up confirm updates rather than duplicates.
    let commit (setup : SetupReference) (c : ExperimentCollection) : ExperimentCollection =
        // Qualify the first field so the literal resolves to `VariedElement` and not the also-in-scope
        // `ExperimentDraft` (which shares `elementId` and `variable`).
        let variedOpt =
            Option.map2
                (fun id variable -> { VariedElement.elementId = id; label = c.draft.elementLabel; variable = variable })
                c.draft.elementId
                c.draft.variable
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
                            range = c.draft.range
                            sourceWaveLengthOpt = c.draft.sourceWaveLengthOpt }
                    else e)
            { c with experiments = experiments }
        | None ->
            match variedOpt with
            | Some _ ->
                let exp =
                    {
                        id = c.nextId
                        setup = setup
                        varied = variedOpt
                        measurement = c.draft.measurement
                        range = c.draft.range
                        dataFileOpt = None
                        sourceWaveLengthOpt = c.draft.sourceWaveLengthOpt
                    }
                { c with
                    experiments = c.experiments @ [ exp ]
                    nextId = c.nextId.next
                    draft = { c.draft with editingId = Some exp.id } }
            | None -> c

    /// Commits the draft, editing nothing, once over each setup in order, each experiment taking the next id; the answer's
    /// draft edits nothing, and an incomplete draft appends nothing (spec 0050 S8.3).
    let commitOverSetups (references : SetupReference list) (c : ExperimentCollection) : ExperimentCollection =
        if c.canCommit then
            let fresh = { c.draft with editingId = None }
            let committed =
                references
                |> List.fold (fun (collection : ExperimentCollection) (reference : SetupReference) ->
                    commit reference { collection with draft = fresh }) c
            { committed with draft = fresh }
        else
            c

    /// Binds one kit-relative measured-data path and hash, replacing any attachment; an unknown id is unchanged
    /// (spec 0038 Part I; spec 0050 S4.2).
    let attachDataFile (id : ExperimentId) (attachment : DataAttachment) (c : ExperimentCollection) : ExperimentCollection =
        let experiments =
            c.experiments
            |> List.map (fun e ->
                if e.id = id then
                    {
                        e with dataFileOpt = Some attachment
                    }
                else
                    e)
        {
            c with experiments = experiments
        }

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
                        sourceWaveLengthOpt = e.sourceWaveLengthOpt
                    } }
        | None -> c

    /// Remove an experiment from the collection (spec 028). If it was the one being edited the draft resets.
    let remove (id : ExperimentId) (c : ExperimentCollection) : ExperimentCollection =
        let experiments = c.experiments |> List.filter (fun e -> e.id <> id)
        let draft = if c.draft.editingId = Some id then ExperimentDraft.empty else c.draft
        { c with experiments = experiments; draft = draft }

    /// The Experiments error channel (errors as values; each case carries a `reason`).
    type ExperimentError =
        | UnknownExperimentErr of reason : string
        | ExperimentUnavailableErr of reason : string

    /// The mock Experiments IO seam (the functional-proxy convention): a record of camelCase
    /// `Result`-returning functions. A test substitutes a stub of the SAME shape. Function-valued
    /// fields have no structural equality, so the proxy compares by reference — letting a host model
    /// that holds the proxy keep its (Elmish-required) equality.
    [<ReferenceEquality>]
    type ExperimentProxy =
        {
            listExperiments : unit -> Result<Experiment list, ExperimentError>
            tryGetExperiment : ExperimentId -> Result<Experiment option, ExperimentError>
        }

    /// An in-memory proxy holding no experiment (spec 0050 S1.7).
    let createInMemoryExperiments () : ExperimentProxy =
        {
            listExperiments = fun () -> Ok []
            tryGetExperiment = fun _ -> Ok None
        }
