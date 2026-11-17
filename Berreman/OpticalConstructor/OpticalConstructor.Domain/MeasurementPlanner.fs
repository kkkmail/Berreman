namespace OpticalConstructor.Domain

open Berreman.Geometry
open Berreman.MaterialProperties
open Berreman.Fields
open OpticalConstructor.Domain.CrystalPhysics
open OpticalConstructor.Domain.MuellerInverse
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
