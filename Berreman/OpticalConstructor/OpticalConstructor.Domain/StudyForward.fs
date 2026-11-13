namespace OpticalConstructor.Domain

open Berreman.Fields
open Berreman.Solvers
open Analytics.AnalysisFunctions
open OpticalConstructor.Domain.CrystalPhysics
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.ItemModels
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.Experiments
open OpticalConstructor.Domain.Setups
open OpticalConstructor.Domain.Propagation
open OpticalConstructor.Domain.MeasuredData
open OpticalConstructor.Domain.MuellerInverse
open OpticalConstructor.Domain.ImportProfiles
open OpticalConstructor.Domain.Studies
open OpticalConstructor.Domain.ExperimentData
open OpticalConstructor.Domain.ItemEvaluation

/// The forward model of an experiment and the study residual (spec 0050 S11.2).
module StudyForward =

    /// Why an experiment's forward model or the study residual refused (spec 0050 S11.2).
    type StudyForwardError =
        /// A mapped point the experiment's scan cannot place (spec 0050 S11.2).
        | UnpairedPointsErr of reason : string
        /// A parameter vector that does not hold one value per entry of the study's layout (spec 0050 S11.2).
        | VectorLengthErr of reason : string

        member this.describe : string =
            match this with
            | UnpairedPointsErr reason -> $"the data cannot be placed on the scan: {reason}"
            | VectorLengthErr reason -> $"the parameter vector does not fit the study: {reason}"

    /// What a detector records, predicted or measured: an intensity detector's signal, an ellipsometer's Ψ and Δ, or a
    /// polarimeter's absolute Mueller matrix, its m₀₀ the throughput as S9.2's readings hold it (spec 0050 S11.2).
    type RecordedReading =
        | RecordedSignal of MeasuredIntensity
        | RecordedPsiDelta of PsiDelta
        | RecordedMueller of MuellerMatrix

    /// One measured point with what fixes its prediction: the scan settings it was taken at, at its own wavelength when it
    /// carries one, or an observation carrying its own configuration (spec 0050 S11.2).
    type MeasuredRow =
        | SettingsRow of settings : ScanSetting list * waveLengthOpt : WaveLength option * measured : RecordedReading
        | ObservationRow of observation : MuellerObservation

    type MeasuredAbscissa with

        /// The abscissa as a scan value: an X,Y file writes it in the varied quantity's display unit (spec 0050 S9.2, S11.2).
        member this.toSweptValue : SweptValue = SweptValue this.value

    type MappedPoints with

        /// The points in order, each placed on `scan.quantities` by the rules `validatedFor` checks; a point the scan cannot
        /// place answers `UnpairedPointsErr` naming its number and the counts (spec 0050 S11.2; 001 §K).
        member this.measuredRowsFor (scan : ExperimentScan) : Result<MeasuredRow list, StudyForwardError> =
            let quantities = scan.quantities

            let settingOf (quantity : ScanQuantity) (value : SweptValue) : ScanSetting =
                {
                    elementId = quantity.elementId
                    variable = quantity.variable
                    value = value
                }

            // An X,Y point or a capture row holds one scan value; a dark line varies nothing (spec 0050 S9.1, S9.2).
            let oneValueSettings (number : MappedPointNumber) (value : SweptValue) : Result<ScanSetting list, StudyForwardError> =
                match quantities with
                | [] -> Ok []
                | [ quantity ] -> Ok [ settingOf quantity value ]
                | _ -> Error (UnpairedPointsErr $"point {number.value} holds one scan value, and the scan varies {quantities.Length}")

            let rows : Result<MeasuredRow, StudyForwardError> list =
                match this with
                | SeriesPoints (IntensityData series) ->
                    series.points
                    |> List.mapi (fun index point ->
                        oneValueSettings (MappedPointNumber (index + 1)) point.x.toSweptValue
                        |> Result.map (fun settings -> SettingsRow (settings, None, RecordedSignal point.intensity)))
                | SeriesPoints (EllipsometricData series) ->
                    let incidenceOpt = quantities |> List.tryFind (fun quantity -> quantity.variable = VaryR2)

                    series.points
                    |> List.map (fun point ->
                        let settings : ScanSetting list =
                            match incidenceOpt, point.aoiOpt with
                            | Some quantity, Some aoi -> [ settingOf quantity (SweptValue aoi.degrees) ]
                            | Some _, None
                            | None, _ -> []

                        let psiDelta : PsiDelta = { psi = point.psi; delta = point.delta }
                        Ok (SettingsRow (settings, Some point.waveLength, RecordedPsiDelta psiDelta)))
                | CapturePoints captures ->
                    captures
                    |> List.mapi (fun index capture ->
                        oneValueSettings (MappedPointNumber (index + 1)) (SweptValue capture.description.degrees)
                        |> Result.map (fun settings -> SettingsRow (settings, None, RecordedSignal capture.avgTotal)))
                | ObservationPoints observations -> observations |> List.map (fun keyed -> Ok (ObservationRow keyed.observation))
                | ColumnPoints mapped ->
                    mapped
                    |> List.mapi (fun index row ->
                        if row.scan.Length = quantities.Length then
                            // The i-th scan cell is the i-th quantity's; a σ is not read, a study weighs runs (001 §L).
                            let settings = List.map2 (fun (cell : ScanCell) quantity -> settingOf quantity cell.displayValue) row.scan quantities

                            let measured =
                                match row.reading with
                                | SignalReading (signal, _) -> RecordedSignal signal
                                | MuellerReading matrix -> RecordedMueller matrix

                            Ok (SettingsRow (settings, None, measured))
                        else
                            Error (UnpairedPointsErr $"point {index + 1} holds {row.scan.Length} scan value(s), and the scan varies {quantities.Length}"))

            ItemEvaluation.sequenced rows

    /// What a study's forward model reads: the kit as the study messages see it, the library, material and data stores,
    /// an item's model as the window's stores resolve it, and the solver parameters a cut plate is solved with, passed
    /// rather than defaulted as `createBerremanForward` asks (spec 0050 S11.2).
    [<ReferenceEquality>]
    type StudyForwardContext =
        {
            kit : StudyContext
            library : Library.LibraryProxy
            materials : MaterialProxy
            experimentData : ExperimentDataProxy
            choiceOf : Item -> ItemModelChoice option
            solverParameters : SolverParameters
        }

    /// What a study reads from a run: residual rows, its session's dark level, or nothing. Derived from the run's role,
    /// never stored, so it carries no tag (spec 0050 S11.2; 001 §L).
    type StudyRunUse =
        | ResidualRunUse
        | DarkLevelRunUse
        | UnreadRunUse

    type ExperimentRole with

        /// A Science, Calibration or Drift-check run gives residual rows, a Dark run its session's dark level; a
        /// Diagnostic, Ignored or Planned run is not read (spec 0050 S11.2; 001 §L).
        member this.studyUse : StudyRunUse =
            match this with
            | ScienceRun
            | CalibrationRun
            | DriftCheckRun -> ResidualRunUse
            | DarkRun -> DarkLevelRunUse
            | DiagnosticRun
            | IgnoredRun
            | PlannedRun -> UnreadRunUse

    type Study with

        /// The study's enabled experiments whose role gives residual rows, as `experiments` holds them, in study order
        /// (spec 0050 S11.2).
        member this.fittedRunsIn (experiments : Experiment list) : Experiment list =
            this.enabledExperiments
            |> List.choose (fun run -> experiments |> List.tryFind (fun experiment -> experiment.name = run))
            |> List.filter (fun experiment -> experiment.role.studyUse = ResidualRunUse)

    /// One value of a study's parameter vector: the unknown and the owner holding that value (spec 0050 S11.2).
    type StudyVectorEntry =
        {
            unknown : StudyUnknown
            owner : UnknownOwner
        }

    /// A place in a study's parameter vector, counted from 0; elevated because ELEV010 refuses an `int` beside
    /// `ParameterValue` (spec 0050 S11.2).
    type StudyVectorIndex =
        | StudyVectorIndex of int

        member this.value : int = let (StudyVectorIndex index) = this in index

    /// The values a fit searches, one per entry of the study's layout, in its order (spec 0050 S11.2).
    [<Softellect.Sys.NumericCarrier("the study's parameter vector: built whole for every residual evaluation and handed whole to the fit")>]
    type StudyParameterVector =
        | StudyParameterVector of float[]

        member this.value : float[] = let (StudyParameterVector values) = this in values

        /// The vector of these values, in order (spec 0050 S11.2).
        static member ofParameterValues (values : ParameterValue list) : StudyParameterVector =
            values |> List.map (fun value -> value.value) |> Array.ofList |> StudyParameterVector

        /// The value at that place; the caller has checked the vector holds one value per entry (spec 0050 S11.2).
        member this.valueAt (index : StudyVectorIndex) : ParameterValue = ParameterValue this.value.[index.value]

    /// How a study's parameter vector is laid out: one entry per owner of each unknown, in the unknowns' order, and the
    /// sessions a session owner names (spec 0050 S11.2; 001 §L, §N).
    type StudyParameterLayout =
        {
            entries : StudyVectorEntry list
            sessions : StudySession list
        }

        /// The vector a fit starts from: each entry's unknown's start, in order (spec 0050 S11.2).
        member this.startVector : StudyParameterVector =
            this.entries |> List.map (fun entry -> entry.unknown.unknown.start) |> StudyParameterVector.ofParameterValues

    type Study with

        /// The layout of the vector a fit of the study searches: for each of its unknowns over the items its experiments
        /// use, one entry per owner, the users being the fitted runs whose slots hold the unknown's item, or an item whose
        /// sample is made of the unknown's opened material; no bound, scaling or option (spec 0050 S11.2, S11.3).
        member this.parameterLayoutWith (context : StudyForwardContext) : StudyParameterLayout =
            let kit = context.kit
            let sessions = this.sessionsIn kit.experiments
            let fitted = this.fittedRunsIn kit.experiments

            // A run whose setup does not resolve holds nothing (spec 0050 S8.2).
            let heldIds (run : Experiment) : ItemId list =
                match run.slotsIn kit.setups with
                | Ok slots -> slots |> List.choose (fun slot -> slot.content.itemOpt)
                | Error _ -> []

            let madeOf (version : MaterialVersionId) (item : Item) : bool =
                match context.choiceOf item with
                | Some (StackSampleChoice (_, _, sample)) -> sample.openedMaterialsIn this |> List.exists (fun material -> material.version = version)
                | Some (SourcePresetChoice _)
                | Some (PolarizerPresetChoice _)
                | Some (LibraryModelChoice _)
                | None -> false

            let usedBy (run : Experiment) (parameter : StudyParameterRef) : bool =
                let held = heldIds run

                match parameter with
                | ItemParameterRef (item, _) -> List.contains item held
                | MaterialParameterRef (version, _) ->
                    kit.items |> List.exists (fun item -> List.contains item.id held && madeOf version item)

            let entries : StudyVectorEntry list =
                this.unknownsWith (kit.itemsUsedBy this)
                |> List.collect (fun unknown ->
                    let users = fitted |> List.filter (fun run -> usedBy run unknown.parameter) |> List.map (fun run -> run.name)
                    unknown.owners sessions users |> List.map (fun owner -> ({ unknown = unknown; owner = owner } : StudyVectorEntry)))

            { entries = entries; sessions = sessions }

    /// The numbers one experiment is evaluated with at one parameter vector: each item's values and each opened
    /// material's crystal, which every sample made of it shares (spec 0050 S11.2; 001 §I, §L).
    type ExperimentAssignment =
        {
            itemValues : Map<ItemId, ParameterValues>
            crystals : Map<MaterialVersionId, CrystalParameters>
        }

        /// The item's assigned values, or its own for an item the assignment lacks (spec 0050 S11.2).
        member this.valuesOf (item : Item) : ParameterValues =
            match this.itemValues |> Map.tryFind item.id with
            | Some values -> values
            | None -> ParameterValues.ofParameters item.parameters

        /// The opened material's assigned crystal, or its working copy's current one (spec 0050 S11.2).
        member this.crystalOf (material : OpenedMaterial) : CrystalParameters =
            match this.crystals |> Map.tryFind material.version with
            | Some crystal -> crystal
            | None -> material.currentCrystal

        /// `materials` with an assigned version resolving to the store's entry over the assigned crystal, so a stack sample
        /// made of an opened material is solved with the study's working copy; all else passes through (spec 0050 S11.2).
        member this.materialProxyOver (materials : MaterialProxy) : MaterialProxy =
            let crystals = this.crystals

            {
                materials with
                    resolveVersion =
                        fun version ->
                            match crystals |> Map.tryFind version with
                            | Some crystal ->
                                materials.resolveVersion version
                                |> Result.map (Option.map (fun (entry : MaterialEntry) -> { entry with physics = CrystalParameterPhysics crystal }))
                            | None -> materials.resolveVersion version
            }

    type StudyParameterLayout with

        /// The experiment's assignment at `vector`: each of `items` and each opened material takes the value of every entry
        /// whose owner holds the experiment, else its own, then each link's follower its leader's value as assigned; refused
        /// when the vector does not hold one value per entry (spec 0050 S11.2; 001 §L).
        member this.assignmentFor
            (study : Study)
            (items : Item list)
            (experiment : ExperimentName)
            (vector : StudyParameterVector)
            : Result<ExperimentAssignment, StudyForwardError> =
            let length = vector.value.Length
            let count = this.entries.Length

            if length <> count then
                Error (VectorLengthErr $"it holds {length} value(s), and the layout has {count} entry(ies)")
            else
                let holds (owner : UnknownOwner) : bool =
                    match owner with
                    | ItemOwner _
                    | MaterialOwner _ -> true
                    | SessionOwner session ->
                        this.sessions |> List.exists (fun block -> block.name = session && List.contains experiment block.experiments)
                    | ExperimentOwner run -> run = experiment

                let held : (StudyParameterRef * ParameterValue) list =
                    this.entries
                    |> List.mapi (fun k entry -> entry, vector.valueAt (StudyVectorIndex k))
                    |> List.filter (fun (entry, _) -> holds entry.owner)
                    |> List.map (fun (entry, value) -> entry.unknown.parameter, value)

                let writtenComponent (name : ParameterName) (value : ParameterValue) (crystal : CrystalParameters) : CrystalParameters =
                    match crystal.parameterAxes |> List.tryFind (fun axis -> axis.name = name) with
                    | Some axis -> axis.writeValue value crystal
                    | None -> crystal

                let itemValuesOf (item : Item) : ParameterValues =
                    held
                    |> List.fold (fun (values : ParameterValues) (parameter, value) ->
                        match parameter with
                        | ItemParameterRef (id, name) when id = item.id -> values.withValue name value
                        | ItemParameterRef _
                        | MaterialParameterRef _ -> values) (ParameterValues.ofParameters item.parameters)

                let crystalOf (material : OpenedMaterial) : CrystalParameters =
                    held
                    |> List.fold (fun (crystal : CrystalParameters) (parameter, value) ->
                        match parameter with
                        | MaterialParameterRef (version, name) when version = material.version -> writtenComponent name value crystal
                        | MaterialParameterRef _
                        | ItemParameterRef _ -> crystal) material.currentCrystal

                let assigned : ExperimentAssignment =
                    {
                        itemValues = items |> List.map (fun item -> item.id, itemValuesOf item) |> Map.ofList
                        crystals = study.materials |> List.map (fun material -> material.version, crystalOf material) |> Map.ofList
                    }

                // A leader never follows (`withLink` refuses a chain), so each follower reads its leader once; a link whose
                // leader or follower the assignment does not hold changes nothing (spec 0050 S11.1, S11.2).
                let leaderValueOpt (assignment : ExperimentAssignment) (leader : StudyParameterRef) : ParameterValue option =
                    match leader with
                    | ItemParameterRef (item, name) ->
                        assignment.itemValues |> Map.tryFind item |> Option.bind (fun values -> values.valueOf name |> Result.toOption)
                    | MaterialParameterRef (version, name) ->
                        assignment.crystals
                        |> Map.tryFind version
                        |> Option.bind (fun crystal ->
                            crystal.parameterAxes |> List.tryFind (fun axis -> axis.name = name) |> Option.map (fun axis -> axis.parameterValueOf crystal))

                let followed (assignment : ExperimentAssignment) (link : ParameterLink) : ExperimentAssignment =
                    match leaderValueOpt assignment link.leader, link.follower with
                    | Some value, ItemParameterRef (item, name) ->
                        match assignment.itemValues |> Map.tryFind item with
                        | Some values -> { assignment with itemValues = assignment.itemValues |> Map.add item (values.withValue name value) }
                        | None -> assignment
                    | Some value, MaterialParameterRef (version, name) ->
                        match assignment.crystals |> Map.tryFind version with
                        | Some crystal -> { assignment with crystals = assignment.crystals |> Map.add version (writtenComponent name value crystal) }
                        | None -> assignment
                    | None, _ -> assignment

                Ok (study.links |> List.fold followed assigned)
