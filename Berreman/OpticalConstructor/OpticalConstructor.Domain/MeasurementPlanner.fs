namespace OpticalConstructor.Domain

open System.Globalization
open Berreman.Geometry
open Berreman.MaterialProperties
open Berreman.Fields
open Berreman.Media
open OpticalConstructor.Domain.Placement
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.MaterialComplexityEditor
open OpticalConstructor.Domain.CrystalPhysics
open OpticalConstructor.Domain.ItemModels
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.ItemEvaluation
open OpticalConstructor.Domain.MuellerInverse
open OpticalConstructor.Domain.Studies
open OpticalConstructor.Domain.PlannerSymmetry

/// The Measurement Planner's inputs from a kit and its structural recoverability report (spec 0050 S14.1).
module MeasurementPlanner =

    /// What a look's detector records (spec 0050 S14.1).
    type DetectorReach =
        /// A full Mueller matrix: a polarimeter, or the HAUP class, an intensity detector between a polarizer and an
        /// analyzer (spec 0050 S14.1).
        | FullMuellerReach
        /// Psi and Delta, the ratio of the diagonal Jones elements (spec 0050 S14.1).
        | EllipsometricReach
        /// S0 alone, its level scaled by the detector's gain (spec 0050 S14.1; 001 §I).
        | IntensityOnlyReach

    /// Whether a look meets the plate along its normal (spec 0050 S14.1).
    type LookTilt =
        | NormalIncidenceLook
        | ObliqueIncidenceLook

    /// Whether a principal axis lies across the refracted wave or along it (spec 0050 S14.1).
    type AxisAspect =
        | TransverseAxis
        | LongitudinalAxis

    /// What one look sees of one tensor component, to first order in its normalized reading (spec 0050 S14.1).
    type LookSight =
        | SeenInLook
        /// Every axis of the component lies along the refracted wave (spec 0050 S14.1).
        | AlongPropagationInLook
        /// An off-diagonal gyration component, which the wave's longitudinal field reaches only off normal incidence
        /// (spec 0050 S14.1).
        | NormalIncidenceInLook
        /// An index the look's reading carries no linear retardance of (spec 0050 S14.1).
        | NoRetardanceInLook
        /// Activity the detector does not record at first order (spec 0050 S14.1; 001 §M).
        | ReachMissesInLook

    /// One measurement a kit or a design makes of a plate: the principal axis on its normal, the external incidence,
    /// T or R, and what its detector records (spec 0050 S14.1).
    type PlannerLook =
        {
            normalAxisOpt : CrystalAxis option
            incidence : IncidenceAngle
            observable : Observable
            reach : DetectorReach
        }

        /// Normal exactly at `IncidenceAngle.normal`, oblique at any other incidence (spec 0050 S14.1).
        member this.tilt : LookTilt =
            if this.incidence.value = IncidenceAngle.normal.value then NormalIncidenceLook else ObliqueIncidenceLook

        /// Longitudinal only for the axis on the normal of a normal-incidence look: off normal incidence the refracted
        /// wave runs at a non-zero internal angle to the normal, so no principal axis lies along it (spec 0050 S14.1).
        member this.aspectOf (axis : CrystalAxis) : AxisAspect =
            match this.tilt, this.normalAxisOpt with
            | NormalIncidenceLook, Some normalAxis when normalAxis = axis -> LongitudinalAxis
            | NormalIncidenceLook, _
            | ObliqueIncidenceLook, _ -> TransverseAxis

        /// What the look sees of a component of `crystal`, in the engine's bi-anisotropic ρ, along whose propagation
        /// the transverse gyration acts and g33 does not (`MuellerInverse.fs` header; spec 0050 S14.1; 001 §M).
        member this.sightOf (crystal : CrystalParameters) (tensorComponent : TensorComponent) : LookSight =
            let transverseOf (axes : CrystalAxis list) : CrystalAxis list =
                axes
                |> List.choose (fun axis ->
                    match this.aspectOf axis with
                    | TransverseAxis -> Some axis
                    | LongitudinalAxis -> None)

            match tensorComponent with
            | PrincipalIndexComponent axes ->
                match transverseOf axes, this.tilt, this.reach with
                | [], _, _ -> AlongPropagationInLook
                // The Fresnel amplitudes and the internal angle move with the index (specs/0044/.manual/004 §3.2).
                | _, ObliqueIncidenceLook, _ -> SeenInLook
                // An intensity alone records no retardance; the gain takes its level.
                | _, NormalIncidenceLook, IntensityOnlyReach -> NoRetardanceInLook
                | _, NormalIncidenceLook, FullMuellerReach
                | _, NormalIncidenceLook, EllipsometricReach ->
                    // Linear retardance needs two distinct indices across the wave; a uniaxial plate along its optic
                    // axis is a pure rotator (`MuellerInverseTests`: C1 alone cannot determine three of the four).
                    let retardingIndices : TensorComponent list =
                        crystal.components
                        |> List.choose (fun (held : CrystalComponent) ->
                            match held.``component`` with
                            | PrincipalIndexComponent indexAxes ->
                                match transverseOf indexAxes with
                                | [] -> None
                                | _ -> Some held.``component``
                            | GyrationDiagonalComponent _
                            | GyrationOffDiagonalComponent _
                            | AbsorptionDiagonalComponent _
                            | AbsorptionOffDiagonalComponent _ -> None)
                        |> List.distinct

                    match retardingIndices with
                    | _ :: _ :: _ -> SeenInLook
                    | _ -> NoRetardanceInLook
            | GyrationDiagonalComponent axes ->
                match transverseOf axes, this.reach with
                | [], _ -> AlongPropagationInLook
                | _, FullMuellerReach -> SeenInLook
                | _, EllipsometricReach
                | _, IntensityOnlyReach -> ReachMissesInLook
            | GyrationOffDiagonalComponent _ ->
                // ρ couples the off-diagonal components through the longitudinal field (`BiaxialInverseTests`: only the
                // tilt buys the gyration OFF-DIAGONAL).
                match this.tilt, this.normalAxisOpt, this.reach with
                | NormalIncidenceLook, Some _, _ -> NormalIncidenceInLook
                | _, _, FullMuellerReach -> SeenInLook
                | _, _, EllipsometricReach
                | _, _, IntensityOnlyReach -> ReachMissesInLook
            | AbsorptionDiagonalComponent axis ->
                match this.aspectOf axis with
                | TransverseAxis -> SeenInLook
                | LongitudinalAxis -> AlongPropagationInLook
            | AbsorptionOffDiagonalComponent pair ->
                // A component coupling the propagation axis reaches the transverse response only at second order.
                match this.aspectOf pair.firstAxis, this.aspectOf pair.secondAxis with
                | TransverseAxis, TransverseAxis -> SeenInLook
                | LongitudinalAxis, _
                | _, LongitudinalAxis -> AlongPropagationInLook

    type MeasurementConfiguration with

        /// A spec 0044 design's configuration as a look with `reach`: its cut's normal axis, incidence and observable,
        /// nothing transcribed (spec 0050 S14.1).
        member this.plannerLookWith (reach : DetectorReach) : PlannerLook =
            {
                normalAxisOpt = this.cut.normalAxisOpt
                incidence = this.incidenceAngle
                observable = this.observable
                reach = reach
            }

    /// Why no look recovers a target; derived, never stored, so it carries no wire tag (spec 0050 S14.1).
    type UnseenReason =
        /// There is no look: the kit holds no plate of the material, or no detector (spec 0050 S14.1).
        | NoLookReason
        | AlongEveryPropagationReason
        | NormalIncidenceOnlyReason
        | NoRetardanceReason
        | FullMuellerNeededReason

        /// The reason as the report states it (spec 0050 S14.1).
        member this.text : string =
            match this with
            | NoLookReason -> "no look of the kit measures a plate of it"
            | AlongEveryPropagationReason -> "every look propagates along its axes"
            | NormalIncidenceOnlyReason -> "it is seen only off normal incidence"
            | NoRetardanceReason ->
                "no look carries its linear retardance or an oblique change of the Fresnel amplitudes"
            | FullMuellerNeededReason ->
                "it needs a full Mueller polarimeter, or a polarizer and an analyzer around the plate"

    /// Whether a set of looks recovers a target, or why not (spec 0050 S14.1; 001 §M).
    type TargetSight =
        | ReachableTarget
        | UnreachableTarget of UnseenReason

    type CrystalParameters with

        /// Reachable when some look sees the component; otherwise the reason of the look closest to seeing it, what
        /// that look lacks (spec 0050 S14.1; 001 §M "this kit can never recover X").
        member this.targetSightOf (looks : PlannerLook list) (tensorComponent : TensorComponent) : TargetSight =
            let closestFirst : (LookSight * TargetSight) list =
                [
                    SeenInLook, ReachableTarget
                    ReachMissesInLook, UnreachableTarget FullMuellerNeededReason
                    NoRetardanceInLook, UnreachableTarget NoRetardanceReason
                    NormalIncidenceInLook, UnreachableTarget NormalIncidenceOnlyReason
                ]

            match looks with
            | [] -> UnreachableTarget NoLookReason
            | _ ->
                let sights : LookSight list = looks |> List.map (fun look -> look.sightOf this tensorComponent)

                closestFirst
                |> List.tryPick (fun (sight, targetSight) -> if List.contains sight sights then Some targetSight else None)
                |> Option.defaultValue (UnreachableTarget AlongEveryPropagationReason)

        /// The largest internal angle any tilt of a plate of it reaches, Snell's law for its smallest principal index
        /// at the steepest incidence the engine takes: no tilt carries the wave further off the plate normal
        /// (spec 0050 S14.1; 001 §M).
        member this.snellCap : Angle =
            let principalIndices : RefractionIndex list =
                match this with
                | UniaxialCrystal parameters -> [ parameters.ordinaryIndex; parameters.extraordinaryIndex ]
                | TriclinicCrystal parameters -> [ parameters.index1; parameters.index2; parameters.index3 ]
                | AbsorbingTriclinicCrystal parameters -> [ parameters.index1; parameters.index2; parameters.index3 ]

            internalAngle (List.min principalIndices) IncidenceAngle.maxValue

    type Angle with

        /// Degrees with one decimal under the invariant culture, as every planner sentence writes an angle
        /// (spec 0050 S14.1).
        member private this.plannerDegreesText : string = this.degrees.ToString("F1", CultureInfo.InvariantCulture)

    type SampleCut with

        /// The plate a remedy asks for, a tilted cut by its tilt (spec 0050 S14.1).
        member this.plateText : string =
            match this with
            | ZCut -> "z-cut plate"
            | XCut -> "x-cut plate"
            | YCut -> "y-cut plate"
            | TiltedCut tilt -> $"{tilt.plannerDegreesText}° tilted-cut plate"

    /// What would recover a target: one more plate, a tilt capped at the Snell angle, or a fuller detector; derived,
    /// never stored (spec 0050 S14.1; specs/0044/.manual/004 §3.2).
    type PlannerRemedy =
        | AddPlateRemedy of SampleCut
        | ObliqueIncidenceRemedy of cap : Angle
        | FullMuellerRemedy

        /// The remedy as the report states it (spec 0050 S14.1).
        member this.text : string =
            match this with
            | AddPlateRemedy cut -> $"add one {cut.plateText}"
            | ObliqueIncidenceRemedy cap ->
                $"tilt a plate (its internal angle reaches at most {cap.plannerDegreesText}°)"
            | FullMuellerRemedy -> "use a full Mueller polarimeter, or a polarizer and an analyzer around the plate"

    /// A target no look recovers, why, and what would recover it (spec 0050 S14.1; 001 §M "this kit can never
    /// recover X").
    type TargetFinding =
        {
            target : CrystalComponent
            reason : UnseenReason
            remedies : PlannerRemedy list
        }

    /// Targets every plate puts on its normal, which only a tilt reaches, at most `capAngle` off it, and the cuts
    /// that would lay them across the wave (spec 0050 S14.1; specs/0044/.manual/004 §3.2).
    type OffAxisCutNeed =
        {
            targets : CrystalComponent list
            capAngle : Angle
            cuts : SampleCut list
        }

    /// One finding of the recoverability report; derived, never stored (spec 0050 S14.1; 001 §M).
    type PlannerFinding =
        | UnrecoverableTargetFinding of TargetFinding
        | OffAxisCutFinding of OffAxisCutNeed

        /// The finding as one sentence naming the parameter, the reason and the remedy (spec 0050 S14.1).
        member this.text : string =
            match this with
            | UnrecoverableTargetFinding finding ->
                $"This kit can never recover {finding.target.name.value}: {finding.reason.text}."
            | OffAxisCutFinding need ->
                let names : string = need.targets |> List.map (fun held -> held.name.value) |> String.concat ", "
                let plates : string = need.cuts |> List.map (fun cut -> $"one {cut.plateText}") |> String.concat " or "

                $"You need an off-axis cut: {names} lie along the normal of every plate, "
                + $"and a tilt carries the wave at most {need.capAngle.plannerDegreesText}° off it; add {plates}."

    /// The structural recoverability report: the crystal's symmetry, the targets the looks recover, and a finding for
    /// each that they cannot, then the off-axis cut they call for (spec 0050 S14.1; 001 §M).
    type RecoverabilityReport =
        {
            symmetry : CrystalSymmetry
            reachable : CrystalComponent list
            findings : PlannerFinding list
        }

        /// The targets of its unrecoverable-target findings, in order (spec 0050 S14.1).
        member this.unrecoverable : CrystalComponent list =
            this.findings
            |> List.choose (fun finding ->
                match finding with
                | UnrecoverableTargetFinding targetFinding -> Some targetFinding.target
                | OffAxisCutFinding _ -> None)

        /// Its off-axis-cut finding, none when the plates do not all share one normal axis (spec 0050 S14.1).
        member this.offAxisCutOpt : OffAxisCutNeed option =
            this.findings
            |> List.tryPick (fun finding ->
                match finding with
                | OffAxisCutFinding need -> Some need
                | UnrecoverableTargetFinding _ -> None)

    /// Why the planner refuses a report, a lab profile or its inputs (spec 0050 S14.1).
    type PlannerError =
        | UnknownPlannerTargetErr of reason : string
        | UnresolvedPlannerItemErr of reason : string
        /// The study holds no working copy of the material version the inputs are asked for (spec 0050 S14.1).
        | UnopenedPlannerMaterialErr of reason : string
        /// The opened copy is a tabulated table, which has no crystal to plan for (spec 0050 S14.1).
        | NonCrystalPlannerMaterialErr of reason : string

        /// The refusal as the planner states it (spec 0050 S14.1).
        member this.describe : string =
            match this with
            | UnknownPlannerTargetErr reason -> $"the crystal has no such parameter: {reason}"
            | UnresolvedPlannerItemErr reason -> $"the item's model is not in the library: {reason}"
            | UnopenedPlannerMaterialErr reason -> $"the study opens no such material version: {reason}"
            | NonCrystalPlannerMaterialErr reason -> $"the opened material is not a crystal: {reason}"

    type CrystalParameters with

        /// Each target reachable through `looks`, or a finding of why not with its remedies over the crystal's
        /// distinct principal plates, then an off-axis cut when every look shares one normal axis; the first unknown
        /// target refuses (spec 0050 S14.1; 001 §M; specs/0044/.manual/004 §3.2).
        member this.structuralReport
            (targets : ParameterName list)
            (looks : PlannerLook list)
            : Result<RecoverabilityReport, PlannerError> =
            let held : CrystalComponent list = this.components

            let heldNames : string =
                held |> List.map (fun (candidate : CrystalComponent) -> candidate.name.value) |> String.concat ", "

            // A y-cut looks at a uniaxial crystal as an x-cut does (`MuellerInverse.fs` `SampleCut`).
            let candidates : SampleCut list =
                match this.symmetry.anisotropy with
                | Uniaxial -> [ ZCut; XCut ]
                | Isotropic
                | Biaxial -> [ ZCut; XCut; YCut ]

            let missing : SampleCut list =
                candidates
                |> List.filter (fun cut -> looks |> List.forall (fun look -> look.normalAxisOpt <> cut.normalAxisOpt))

            let addPlatesSeeing (plates : SampleCut list) (target : CrystalComponent) : PlannerRemedy list =
                plates
                |> List.choose (fun cut ->
                    let plateLook : PlannerLook =
                        {
                            normalAxisOpt = cut.normalAxisOpt
                            incidence = IncidenceAngle.normal
                            observable = TransmittedMueller
                            reach = FullMuellerReach
                        }

                    match plateLook.sightOf this target.``component`` with
                    | SeenInLook -> Some (AddPlateRemedy cut)
                    | AlongPropagationInLook
                    | NormalIncidenceInLook
                    | NoRetardanceInLook
                    | ReachMissesInLook -> None)

            let remediesOf (target : CrystalComponent) (reason : UnseenReason) : PlannerRemedy list =
                match reason with
                | NoLookReason -> addPlatesSeeing candidates target @ [ FullMuellerRemedy ]
                | AlongEveryPropagationReason
                | NoRetardanceReason -> addPlatesSeeing missing target @ [ ObliqueIncidenceRemedy this.snellCap ]
                // More plates never substitute for a tilt on an off-diagonal component (`BiaxialInverseTests`).
                | NormalIncidenceOnlyReason -> [ ObliqueIncidenceRemedy this.snellCap ]
                | FullMuellerNeededReason -> [ FullMuellerRemedy ]

            let reportOf (targetComponents : CrystalComponent list) : RecoverabilityReport =
                let sights : (CrystalComponent * TargetSight) list =
                    targetComponents |> List.map (fun target -> target, this.targetSightOf looks target.``component``)

                let reachable : CrystalComponent list =
                    sights
                    |> List.choose (fun (target, sight) ->
                        match sight with
                        | ReachableTarget -> Some target
                        | UnreachableTarget _ -> None)

                let targetFindings : PlannerFinding list =
                    sights
                    |> List.choose (fun (target, sight) ->
                        match sight with
                        | ReachableTarget -> None
                        | UnreachableTarget reason ->
                            let finding : TargetFinding =
                                {
                                    target = target
                                    reason = reason
                                    remedies = remediesOf target reason
                                }

                            Some (UnrecoverableTargetFinding finding))

                let offAxisFindings : PlannerFinding list =
                    match looks |> List.map (fun look -> look.normalAxisOpt) |> List.distinct with
                    | [ (Some sharedAxis as sharedNormal) ] ->
                        let onNormal : CrystalComponent list =
                            targetComponents |> List.filter (fun target -> target.``component``.axes = [ sharedAxis ])

                        match onNormal with
                        | [] -> []
                        | _ :: _ ->
                            let need : OffAxisCutNeed =
                                {
                                    targets = onNormal
                                    capAngle = this.snellCap
                                    cuts = candidates |> List.filter (fun cut -> cut.normalAxisOpt <> sharedNormal)
                                }

                            [ OffAxisCutFinding need ]
                    | _ -> []

                { symmetry = this.symmetry; reachable = reachable; findings = targetFindings @ offAxisFindings }

            targets
            |> List.map (fun name ->
                match held |> List.tryFind (fun (candidate : CrystalComponent) -> candidate.name = name) with
                | Some found -> Ok found
                | None -> Error (UnknownPlannerTargetErr $"{name.value} (it has {heldNames})"))
            |> ItemEvaluation.sequenced
            |> Result.map reportOf

    /// A light source of the kit: its preset's line, and its limits when it is tunable (spec 0050 S14.1; 001 §M).
    type PlannerSource =
        {
            item : ItemId
            label : ItemLabel
            waveLength : WaveLength
            tunableLimitsOpt : Berreman.Dispersion.WaveLengthInterval option
        }

    /// What an optic of the kit does around a plate; derived, never stored, so it carries no wire tag
    /// (spec 0050 S14.1).
    type OpticRole =
        | PolarizerOptic
        | CompensatorOptic
        /// A compound holding a polarizer at some depth, as a circular polarizer does (spec 0050 S14.1).
        | CompoundOptic

    /// A polarizer, compensator or compound of the kit (spec 0050 S14.1; 001 §M).
    type PlannerOptic =
        {
            item : ItemId
            label : ItemLabel
            role : OpticRole
        }

    /// What a detector of the kit records; derived, never stored, so it carries no wire tag (spec 0050 S14.1).
    type DetectorRecording =
        | RecordsIntensity
        | RecordsMuellerMatrix
        | RecordsPsiDelta

    /// A detector of the kit with its noise model: the item's own values, checked against its model's noise axes
    /// (spec 0050 S14.1; 001 §M).
    type PlannerDetector =
        {
            item : ItemId
            label : ItemLabel
            recording : DetectorRecording
            noise : ParameterValues
        }

    /// The lab profile: the kit's sources, optics and detectors read from their models in item order; nothing is
    /// typed in (spec 0050 S14.1; 001 §M).
    type LabProfile =
        {
            sources : PlannerSource list
            optics : PlannerOptic list
            detectors : PlannerDetector list
        }

        /// Each item read through `choiceOf`, as the study's forward model reads it; the first item it does not
        /// resolve, or a detector its noise axes refuse, refuses the whole profile (spec 0050 S14.1; 001 §M).
        static member ofItems
            (items : Item list)
            (choiceOf : Item -> ItemModelChoice option)
            : Result<LabProfile, PlannerError> =
            let noProfile : LabProfile = { sources = []; optics = []; detectors = [] }

            // A compound's component may itself be a compound; its leaves are what the light meets.
            let rec leafForms (components : CompoundComponent list) : OpticalForm list =
                components
                |> List.collect (fun part ->
                    match part.form with
                    | CompoundModel inner -> leafForms inner
                    | RealPolarizerModel _
                    | RetarderPlateModel _
                    | BlackBoxMuellerModel _ -> [ part.form ])

            let compoundRoleOf (components : CompoundComponent list) : OpticRole =
                leafForms components
                |> List.tryPick (fun form ->
                    match form with
                    | RealPolarizerModel _ -> Some CompoundOptic
                    | RetarderPlateModel _
                    | BlackBoxMuellerModel _
                    | CompoundModel _ -> None)
                |> Option.defaultValue CompensatorOptic

            let profileOf (item : Item) : Result<LabProfile, PlannerError> =
                let opticOf (role : OpticRole) : LabProfile =
                    let optic : PlannerOptic = { item = item.id; label = item.label; role = role }
                    { noProfile with optics = [ optic ] }

                let detectorOf
                    (recording : DetectorRecording)
                    (noiseAxes : ItemParameterAxis list)
                    : Result<LabProfile, PlannerError> =
                    (ParameterValues.ofParameters item.parameters).checkedFor noiseAxes
                    |> Result.mapError (fun error -> UnresolvedPlannerItemErr $"{item.label.value}: {error.describe}")
                    |> Result.map (fun noise ->
                        let detector : PlannerDetector =
                            {
                                item = item.id
                                label = item.label
                                recording = recording
                                noise = noise
                            }

                        { noProfile with detectors = [ detector ] })

                match choiceOf item with
                | None -> Error (UnresolvedPlannerItemErr item.label.value)
                | Some (SourcePresetChoice (_, preset)) ->
                    let source : PlannerSource =
                        {
                            item = item.id
                            label = item.label
                            waveLength = preset.waveLength
                            tunableLimitsOpt = preset.tunableLimitsOpt
                        }

                    Ok { noProfile with sources = [ source ] }
                | Some (PolarizerPresetChoice (_, polarizer)) ->
                    match polarizer.behavior with
                    | Library.ComputedIdeal _ -> Ok (opticOf PolarizerOptic)
                    | Library.ConstantMueller _ -> Ok (opticOf CompoundOptic)
                // A plate is the planner's sample (`PlannerPlate`), not a part of the lab.
                | Some (StackSampleChoice _) -> Ok noProfile
                | Some (LibraryModelChoice model) ->
                    match model.entry.model with
                    | OpticalElementModel (RealPolarizerModel _) -> Ok (opticOf PolarizerOptic)
                    | OpticalElementModel (RetarderPlateModel _) -> Ok (opticOf CompensatorOptic)
                    | OpticalElementModel (CompoundModel components) -> Ok (opticOf (compoundRoleOf components))
                    // An object, and the calibration reference.
                    | OpticalElementModel (BlackBoxMuellerModel _)
                    | EmptySlotModel -> Ok noProfile
                    | IntensityDetectorModel noiseAxes -> detectorOf RecordsIntensity noiseAxes
                    | MuellerPolarimeterModel noiseAxes -> detectorOf RecordsMuellerMatrix noiseAxes
                    | EllipsometerModel noiseAxes -> detectorOf RecordsPsiDelta noiseAxes
                    // Only a bound choice carries the preset or sample a binding form takes its physics from
                    // (step 216).
                    | SourcePresetItemModel
                    | IdealPolarizerItemModel
                    | StackSampleItemModel ->
                        let modelName : string = model.entry.name.value
                        let reason : string = $"{item.label.value}: '{modelName}' takes a binding, and the choice binds none"
                        Error (UnresolvedPlannerItemErr reason)

            items
            |> List.map profileOf
            |> sequenced
            |> Result.map (fun (profiles : LabProfile list) ->
                let profile : LabProfile =
                    {
                        sources = profiles |> List.collect (fun (one : LabProfile) -> one.sources)
                        optics = profiles |> List.collect (fun (one : LabProfile) -> one.optics)
                        detectors = profiles |> List.collect (fun (one : LabProfile) -> one.detectors)
                    }

                profile)

        /// A polarimeter's full Mueller matrix, an ellipsometer's Psi and Delta, and an intensity detector's full
        /// Mueller matrix only beside two polarizing optics, a polarizer and an analyzer around the plate: the HAUP
        /// class (spec 0050 S14.1; 001 §M).
        member this.reachOf (detector : PlannerDetector) : DetectorReach =
            match detector.recording with
            | RecordsMuellerMatrix -> FullMuellerReach
            | RecordsPsiDelta -> EllipsometricReach
            | RecordsIntensity ->
                let polarizing : PlannerOptic list =
                    this.optics
                    |> List.choose (fun (optic : PlannerOptic) ->
                        match optic.role with
                        | PolarizerOptic
                        | CompoundOptic -> Some optic
                        | CompensatorOptic -> None)

                match polarizing with
                | _ :: _ :: _ -> FullMuellerReach
                | _ -> IntensityOnlyReach

        /// Each reach some detector of the profile has, once, the fullest first (spec 0050 S14.1).
        member this.reaches : DetectorReach list =
            let reached : DetectorReach list = this.detectors |> List.map this.reachOf
            [ FullMuellerReach; EllipsometricReach; IntensityOnlyReach ]
            |> List.filter (fun reach -> List.contains reach reached)

    /// A free-standing plate of the material in the kit, the shape spec 0044's forward model takes: its sample's
    /// substrate layer, finite, with the principal axis that layer's orientation puts on the normal (spec 0050 S14.1).
    type PlannerPlate =
        {
            item : ItemId
            label : ItemLabel
            sample : Library.SampleVersionId
            thickness : Thickness
            substrate : Library.SubstrateKind
            emission : Emission
            normalAxisOpt : CrystalAxis option
        }

    /// The prior: the study's opened copy, whose parameter states carry the known, calibrated and prior values, and its
    /// crystal at their current values (spec 0050 S14.1; 001 §M).
    type PlannerPrior =
        {
            material : OpenedMaterial
            crystal : CrystalParameters
        }

    /// The planner's four inputs from a kit and one of its studies (spec 0050 S14.1; 001 §M).
    type PlannerInputs =
        {
            lab : LabProfile
            plates : PlannerPlate list
            prior : PlannerPrior
            targets : ParameterName list
        }

    type Study with

        /// The inputs for `material`: the kit's lab profile, its finite plates of the material in item order, the opened
        /// copy at its current crystal, and the study's unknown components of it, a link's follower left out; refused when
        /// the study does not open the version, then when the copy is no crystal, then by the lab profile (spec 0050 S14.1).
        member this.plannerInputsWith
            (context : StudyContext)
            (choiceOf : Item -> ItemModelChoice option)
            (material : MaterialVersionId)
            : Result<PlannerInputs, PlannerError> =
            let versionText : string = $"material {material.materialId.value} v{material.version.value}"

            let plateOf (item : Item) : PlannerPlate option =
                match choiceOf item with
                | Some (StackSampleChoice (_, version, sample)) ->
                    match sample.structure.substrate with
                    | Some layer when layer.materialId = material ->
                        match layer.thickness.parameterValueOpt with
                        | Some _ ->
                            let plate : PlannerPlate =
                                {
                                    item = item.id
                                    label = item.label
                                    sample = version
                                    thickness = layer.thickness
                                    substrate = sample.substrate
                                    emission = sample.supportedEmission
                                    normalAxisOpt = layer.orientation.normalAxisOpt
                                }

                            Some plate
                        | None -> None
                    | Some _
                    | None -> None
                | Some (SourcePresetChoice _)
                | Some (PolarizerPresetChoice _)
                | Some (LibraryModelChoice _)
                | None -> None

            // A known or calibrated component is prior, and an item's unknown is no crystal component (S14.2 weighs it).
            let targets : ParameterName list =
                this.unknownsWith (context.itemsUsedBy this)
                |> List.choose (fun (unknown : StudyUnknown) ->
                    match unknown.parameter with
                    | MaterialParameterRef (version, name) when version = material -> Some name
                    | MaterialParameterRef _
                    | ItemParameterRef _ -> None)

            let priorResult : Result<PlannerPrior, PlannerError> =
                match this.materials |> List.tryFind (fun (opened : OpenedMaterial) -> opened.version = material) with
                | None ->
                    Error (UnopenedPlannerMaterialErr $"{versionText} (the study '{this.name.value}' opens {this.materials.Length})")
                | Some opened ->
                    match opened.currentPhysics.crystalOpt with
                    | Some crystal -> Ok { material = opened; crystal = crystal }
                    | None -> Error (NonCrystalPlannerMaterialErr $"'{opened.name.value}' ({versionText}) opens as a table of n and k")

            priorResult
            |> Result.bind (fun prior ->
                LabProfile.ofItems context.items choiceOf
                |> Result.map (fun lab ->
                    let inputs : PlannerInputs =
                        {
                            lab = lab
                            plates = context.items |> List.choose plateOf
                            prior = prior
                            targets = targets
                        }

                    inputs))
