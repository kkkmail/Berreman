namespace OpticalConstructor.Optimization

open Berreman.Geometry
open OpticalConstructor.Domain
open OpticalConstructor.Domain.ItemModels
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.Experiments
open OpticalConstructor.Domain.Setups
open OpticalConstructor.Domain.MuellerReconstruction
open OpticalConstructor.Domain.ImportProfiles
open OpticalConstructor.Domain.Studies
open OpticalConstructor.Domain.StudyForward
open OpticalConstructor.Domain.FitRuns

/// The study engine: its automatic choice between the linear path (S10.3's staged Mueller path) and the nonlinear one
/// (the multi-start study residual), and the fit (spec 0050 S11.3; 001 §L).
module StudyEngine =

    /// Why a study fit refused (spec 0050 S11.3).
    type StudyFitError =
        /// The study's forward model, its pre-fit or its data refused (spec 0050 S11.3).
        | StudyForwardFitErr of error : StudyForwardError
        /// The study could not be staged for the fit (spec 0050 S11.3).
        | FitStageErr of error : FitRunError

        member this.describe : string =
            match this with
            | StudyForwardFitErr error -> $"the study's model or data refused the fit: {error.describe}"
            | FitStageErr error -> $"the stage refused the fit: {error.describe}"

    /// The reference families' two polarizers, linear or LP-then-retarder circular; derived, never stored, so it carries
    /// no tag (spec 0050 S11.3; `MuellerReconstruction.Family`).
    type CalibrationPolarizer =
        | LinearCalibrationPolarizer
        | CircularCalibrationPolarizer

        /// The family this preparing polarizer makes with `analyzer` (spec 0050 S11.3).
        member this.familyWith (analyzer : CalibrationPolarizer) : Family =
            match this, analyzer with
            | LinearCalibrationPolarizer, LinearCalibrationPolarizer -> LpLp
            | LinearCalibrationPolarizer, CircularCalibrationPolarizer -> LpCpl
            | CircularCalibrationPolarizer, LinearCalibrationPolarizer -> CplLp
            | CircularCalibrationPolarizer, CircularCalibrationPolarizer -> CplCpl

    // The built-in circular polarizer's retarder: its second component, at 45° within it (spec 0050 S8.1, step 210).
    let private retarderComponent : ParameterNameIndex = ParameterNameIndex 2

    let private retarderComponentOffset : Angle = Angle.degree 45.0<Berreman.Constants.deg>

    type ItemModelChoice with

        /// The reference-family polarizer the choice is: the ideal-LP preset or a real linear polarizer is linear, the
        /// built-in LP-then-retarder compound circular, anything else none; for calibration only (spec 0050 S11.3).
        member this.calibrationPolarizerOpt : CalibrationPolarizer option =
            match this with
            | PolarizerPresetChoice (_, polarizer) ->
                match polarizer.behavior with
                | Library.ComputedIdeal Library.IdealLinear -> Some LinearCalibrationPolarizer
                | Library.ComputedIdeal (Library.IdealCircularLeft | Library.IdealCircularRight)
                | Library.ConstantMueller _ -> None
            | LibraryModelChoice model ->
                match model.entry.model with
                | OpticalElementModel (RealPolarizerModel _) -> Some LinearCalibrationPolarizer
                | OpticalElementModel (CompoundModel [ { CompoundComponent.form = RealPolarizerModel _; offset = polarizerOffset }
                                                       { CompoundComponent.form = RetarderPlateModel _; offset = retarderOffset } ])
                    when polarizerOffset = Angle.zero && retarderOffset = retarderComponentOffset -> Some CircularCalibrationPolarizer
                | OpticalElementModel (RetarderPlateModel _ | BlackBoxMuellerModel _ | CompoundModel _)
                | SourcePresetItemModel
                | IdealPolarizerItemModel
                | StackSampleItemModel
                | EmptySlotModel
                | IntensityDetectorModel _
                | MuellerPolarimeterModel _
                | EllipsometerModel _ -> None
            | SourcePresetChoice _
            | StackSampleChoice _ -> None

    /// A Calibration run's reference family and the items preparing and analyzing it (spec 0050 S11.3).
    type private CalibrationRunFamily =
        {
            runFamily : Family
            prepItem : ItemId
            analyzerItem : ItemId
        }

        /// The parameters stage one fits on a run of this family, the reference stage-1 constants (spec 0050 S11.3;
        /// `StagedMuellerPath.calibrateStageOne`).
        member this.stageOneConstants : StudyParameterRef list =
            let retardance = ParameterName.retardance.inComponent retarderComponent

            match this.runFamily with
            | LpLp -> [ ItemParameterRef (this.analyzerItem, ParameterName.dialZero) ]
            | LpCpl -> [ ItemParameterRef (this.analyzerItem, ParameterName.dialZero); ItemParameterRef (this.analyzerItem, retardance) ]
            | CplLp ->
                [
                    ItemParameterRef (this.prepItem, ParameterName.fastAxisOffset.inComponent retarderComponent)
                    ItemParameterRef (this.prepItem, retardance)
                ]
            | CplCpl -> []

    /// The sixteen-element black box's coefficients m00 … m33, its form's own axes (spec 0050 S8.1).
    let private muellerCoefficients : ParameterName list =
        (BlackBoxMuellerModel SixteenElementMueller).parameterAxes |> List.map (fun axis -> axis.name)

    type StagedStudy with

        /// Linear for the reference AIR calibration under stage 1, or for sixteen-element black boxes, one per fitted path,
        /// behind fixed optics under stage 2 or joint; nonlinear otherwise. Structural: no score, threshold or rank is read
        /// (spec 0050 S11.3; 001 §L).
        member this.engineWith (context : StudyForwardContext) (prefits : ExperimentPrefit list) : StudyEngineKind =
            let kit = { context.kit with items = this.items }
            let entries = (this.study.parameterLayoutWith { context with kit = kit }).entries
            let fittedParameters = entries |> List.map (fun entry -> entry.unknown.parameter)

            // A link's follower takes its leader's value, so following a fitted leader makes it fitted too (spec 0050 S11.1).
            let fittedFollowers : StudyParameterRef list =
                this.study.links
                |> List.filter (fun link -> List.contains link.leader fittedParameters)
                |> List.map (fun link -> link.follower)

            let bearsUnknown (item : ItemId) : bool =
                fittedParameters @ fittedFollowers
                |> List.exists (fun parameter ->
                    match parameter with
                    | ItemParameterRef (owner, _) -> owner = item
                    | MaterialParameterRef _ -> false)

            let itemOwned (entry : StudyVectorEntry) : bool =
                match entry.owner, entry.unknown.parameter with
                | ItemOwner owner, ItemParameterRef (item, _) -> owner = item
                | (ItemOwner _ | MaterialOwner _ | SessionOwner _ | ExperimentOwner _), _ -> false

            // A fitted run's held items in light order with their models; `None` when its setup, an item or a model does
            // not resolve.
            let heldIn (run : Experiment) : (SlotRole * Item * ItemModelChoice) list option =
                match run.slotsIn kit.setups with
                | Error _ -> None
                | Ok slots ->
                    let roledItems = SetupSlot.inLightOrder slots |> List.choose (fun roled -> roled.slot.content.itemOpt |> Option.map (fun id -> roled.role, id))

                    let resolved =
                        roledItems
                        |> List.choose (fun (role, id) ->
                            this.items
                            |> List.tryFind (fun item -> item.id = id)
                            |> Option.bind (fun item -> context.choiceOf item |> Option.map (fun model -> role, item, model)))

                    if resolved.Length = roledItems.Length then Some resolved else None

            let fitted : (ExperimentName * (SlotRole * Item * ItemModelChoice) list) list =
                this.fittedRuns
                |> List.choose (fun run ->
                    kit.experiments |> List.tryFind (fun experiment -> experiment.name = run) |> Option.bind heldIn |> Option.map (fun held -> run, held))

            let everyRunResolves = fitted.Length = this.fittedRuns.Length

            // The rows of a Phone ROI CSV, step 300's capture rows (spec 0050 S9.2).
            let readsCaptureRows (run : ExperimentName) : bool =
                match prefits |> List.tryFind (fun prefit -> prefit.experiment.name = run) |> Option.map (fun prefit -> prefit.check) with
                | Some (Ok (CapturePoints _)) -> true
                | Some (Ok (SeriesPoints _ | ObservationPoints _ | ColumnPoints _))
                | Some (Error _)
                | None -> false

            // The detector that reads the run, as `boundWith` finds it: the first detector-slot item of a detector form.
            let readByIntensityDetector (held : (SlotRole * Item * ItemModelChoice) list) : bool =
                let detectorForms : ItemModelForm list =
                    held
                    |> List.choose (fun (role, _, model) ->
                        match role, model with
                        | DetectorSlotRole _, LibraryModelChoice version ->
                            match version.entry.model with
                            | IntensityDetectorModel _
                            | MuellerPolarimeterModel _
                            | EllipsometerModel _ -> Some version.entry.model
                            | SourcePresetItemModel
                            | IdealPolarizerItemModel
                            | StackSampleItemModel
                            | OpticalElementModel _
                            | EmptySlotModel -> None
                        | DetectorSlotRole _, (SourcePresetChoice _ | PolarizerPresetChoice _ | StackSampleChoice _)
                        | (SourceSlotRole | PrepSlotRole _ | ObjectSlotRole _ | AnalyzerSlotRole _), _ -> None)

                match detectorForms with
                | IntensityDetectorModel _ :: _ -> true
                | _ -> false

            let readsCapturesByIntensity (run : ExperimentName, held : (SlotRole * Item * ItemModelChoice) list) : bool =
                readsCaptureRows run && readByIntensityDetector held

            // An AIR run: no object but an empty-slot item, one reference polarizer preparing and one analyzing.
            let calibrationRunOf (held : (SlotRole * Item * ItemModelChoice) list) : CalibrationRunFamily option =
                let objectsEmpty =
                    held
                    |> List.forall (fun (role, _, model) ->
                        match role, model with
                        | ObjectSlotRole _, LibraryModelChoice version ->
                            match version.entry.model with
                            | EmptySlotModel -> true
                            | _ -> false
                        | ObjectSlotRole _, _ -> false
                        | (SourceSlotRole | PrepSlotRole _ | AnalyzerSlotRole _ | DetectorSlotRole _), _ -> true)

                let preps = held |> List.filter (fun (role, _, _) -> match role with PrepSlotRole _ -> true | _ -> false)
                let analyzers = held |> List.filter (fun (role, _, _) -> match role with AnalyzerSlotRole _ -> true | _ -> false)

                match objectsEmpty, preps, analyzers with
                | true, [ (_, prep, prepModel) ], [ (_, analyzer, analyzerModel) ] ->
                    match prepModel.calibrationPolarizerOpt, analyzerModel.calibrationPolarizerOpt with
                    | Some prepPolarizer, Some analyzerPolarizer ->
                        Some { runFamily = prepPolarizer.familyWith analyzerPolarizer; prepItem = prep.id; analyzerItem = analyzer.id }
                    | _ -> None
                | _ -> None

            // (a) Stage 1 of the reference: LP-LP, LP-CPL and CPL-LP AIR runs over capture rows, fitting only their
            // stage-1 constants.
            let calibrationShape () : bool =
                let families =
                    fitted
                    |> List.choose (fun (run, held) -> if readsCapturesByIntensity (run, held) then calibrationRunOf held else None)

                let constants = families |> List.collect (fun family -> family.stageOneConstants)
                let present = families |> List.map (fun family -> family.runFamily)

                everyRunResolves
                && families.Length = fitted.Length
                && [ LpLp; LpCpl; CplLp ] |> List.forall (fun family -> List.contains family present)
                && entries |> List.forall (fun entry -> itemOwned entry && List.contains entry.unknown.parameter constants)
                && List.isEmpty fittedFollowers

            let isCoefficient (parameter : StudyParameterRef) : bool =
                match parameter with
                | ItemParameterRef (id, name) when List.contains name muellerCoefficients ->
                    match this.items |> List.tryFind (fun item -> item.id = id) |> Option.bind context.choiceOf with
                    | Some (LibraryModelChoice model) ->
                        match model.entry.model with
                        | OpticalElementModel (BlackBoxMuellerModel SixteenElementMueller) -> true
                        | _ -> false
                    | Some (SourcePresetChoice _ | PolarizerPresetChoice _ | StackSampleChoice _)
                    | None -> false
                | ItemParameterRef _
                | MaterialParameterRef _ -> false

            // (b) Black-box coefficients, linked or not, each fitted path bearing at most one unknown matrix: every other
            // element on it, its gain included, is fixed.
            let blackBoxShape () : bool =
                // Counted per slot: one free item held in two slots is a double pass, quadratic in its coefficients.
                let oneUnknownMatrix (held : (SlotRole * Item * ItemModelChoice) list) : bool =
                    let bearing = held |> List.filter (fun (_, item, _) -> bearsUnknown item.id)
                    bearing.Length <= 1

                everyRunResolves
                && entries |> List.forall (fun entry -> itemOwned entry && isCoefficient entry.unknown.parameter)
                && fittedFollowers |> List.forall isCoefficient
                && fitted |> List.forall (fun (run, held) -> readsCapturesByIntensity (run, held) && oneUnknownMatrix held)

            let linear =
                match this.stage with
                | CalibrationStage -> calibrationShape ()
                | ObjectStage
                | JointStage -> blackBoxShape ()

            if linear then LinearStudyEngine else NonlinearStudyEngine
