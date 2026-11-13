namespace OpticalConstructor.Domain

open Berreman.Fields
open Analytics.AnalysisFunctions
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.Experiments
open OpticalConstructor.Domain.Propagation
open OpticalConstructor.Domain.MeasuredData
open OpticalConstructor.Domain.MuellerInverse
open OpticalConstructor.Domain.ImportProfiles

/// The forward model of an experiment and the study residual (spec 0050 S11.2).
module StudyForward =

    /// Why an experiment's forward model or the study residual refused (spec 0050 S11.2).
    type StudyForwardError =
        /// A mapped point the experiment's scan cannot place (spec 0050 S11.2).
        | UnpairedPointsErr of reason : string

        member this.describe : string =
            match this with
            | UnpairedPointsErr reason -> $"the data cannot be placed on the scan: {reason}"

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
