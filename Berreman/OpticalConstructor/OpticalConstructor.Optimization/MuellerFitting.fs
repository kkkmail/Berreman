namespace OpticalConstructor.Optimization

open Berreman.Constants
open Berreman.Geometry
open Berreman.Fields
open Berreman.Media
open OpticalConstructor.Domain.MuellerInverse

/// The Mueller-matrix fit machinery the inverse-problem suites, the study engine and the Planner share, moved from
/// `BerremanTests/InverseFitHarness.fs` by spec 0050 S10.1 (spec 0044 manual tasks 010-016).
module MuellerFitting =

    /// A typed failure of generating observations or fitting them, never a throw.
    type MuellerFitError =
        | ObservationForwardErr of ForwardModelError

    /// One measurement configuration, its incidence and sample azimuth given in degrees.
    /// `IncidenceAngle.create` folds the incidence into [0, 90) deg, so a negative one is not a tilt the other way.
    let configurationOf
        (cut : SampleCut)
        (thickness : Thickness)
        (incidence : float<deg>)
        (azimuth : float<deg>)
        (observable : Observable)
        (waveLength : WaveLength)
        : MeasurementConfiguration =
        {
            cut = cut
            thickness = thickness
            incidenceAngle = Angle.degree incidence |> IncidenceAngle.create
            azimuth = SampleAzimuth.degree azimuth
            observable = observable
            waveLength = waveLength
        }

    /// The measured data a configuration set records from known truth, in configuration order. The first forward
    /// failure is the error and nothing after it is evaluated: a failure on the ground truth is not a datum.
    let observeWith
        (forward : ForwardModelProxy<'Parameters>)
        (truth : 'Parameters)
        (configurations : MeasurementConfiguration list)
        : Result<MuellerObservation list, MuellerFitError> =
        (Ok [], configurations)
        ||> List.fold (fun acc c ->
            acc |> Result.bind (fun os -> forward.muellerOf c truth |> Result.map (fun m -> { configuration = c; measured = m } :: os)))
        |> Result.map List.rev
        |> Result.mapError ObservationForwardErr
