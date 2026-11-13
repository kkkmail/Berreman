namespace OpticalConstructor.Domain

open OpticalConstructor.Domain.Experiments
open OpticalConstructor.Domain.Setups
open OpticalConstructor.Domain.MeasuredData
open OpticalConstructor.Domain.ImportProfiles
open OpticalConstructor.Domain.ExperimentData

/// An experiment's checks before a fit (spec 0050 S11.2; spec 0038 Part L).
module PrefitChecks =

    /// Whether a slot says what it holds, as a fit reads it (spec 0050 S11.2).
    type SlotSpecification =
        | SpecifiedSlot
        | UnspecifiedSlot

    type SetupSlot with

        /// Specified when the slot holds an item or its placement is bound (spec 0050 S8.2, S11.2).
        member this.specification : SlotSpecification =
            match this.content, this.placement.valueId with
            | ItemInSlot _, _
            | EmptySlotContent, Some _ -> SpecifiedSlot
            | EmptySlotContent, None -> UnspecifiedSlot

    /// Why an experiment is not ready for a fit: its slots, its missing data, a file that does not read, or points that
    /// do not fit its scan (spec 0050 S11.2).
    type PrefitError =
        | UnspecifiedSlotsErr of reason : string
        | MissingDataErr of reason : string
        | UnreadableDataErr of error : ExperimentDataError
        | InconsistentDataErr of error : ExperimentDataError

        member this.describe : string =
            match this with
            | UnspecifiedSlotsErr reason -> $"a slot is not specified: {reason}"
            | MissingDataErr reason -> $"no data is attached: {reason}"
            | UnreadableDataErr (MalformedDataFileErr reason | EmptyDataFileErr reason | DataRangeMismatchErr reason | DataUnitsMismatchErr reason) ->
                $"the data file does not read: {reason}"
            | InconsistentDataErr (MalformedDataFileErr reason | EmptyDataFileErr reason | DataRangeMismatchErr reason | DataUnitsMismatchErr reason) ->
                $"the data does not fit the scan: {reason}"

    type Experiment with

        /// In order: the setup revision resolves among `setups` and every slot but a sample's is specified, data is
        /// attached, and its kept points read and fit the scan; the kept, validated points a fit reads (spec 0050 S11.2).
        member this.prefitCheckWith
            (library : Library.LibraryProxy)
            (dataProxy : ExperimentDataProxy)
            (setups : Setup list)
            : Result<MappedPoints, PrefitError> =
            match this.slotsIn setups with
            | Error error -> Error (UnspecifiedSlotsErr $"its setup is not in the collection: {error.describe}")
            | Ok slots ->
                let unspecified : string list =
                    SetupSlot.inLightOrder slots
                    |> List.choose (fun roled ->
                        match roled.slot.placement.catalogueKind, roled.slot.specification with
                        | Placement.Sample, _
                        | _, SpecifiedSlot -> None
                        | _, UnspecifiedSlot -> Some roled.role.text)

                match unspecified, this.dataOpt with
                | _ :: _, _ ->
                    let names = unspecified |> String.concat ", "
                    Error (UnspecifiedSlotsErr $"every element except the sample must be specified; still unbound: {names}")
                | [], None -> Error (MissingDataErr "no measured-data file is attached to this experiment")
                | [], Some data ->
                    ExperimentDataLoad.loadAndValidate library dataProxy slots this data
                    |> Result.mapError (fun error ->
                        match error with
                        | MalformedDataFileErr _
                        | EmptyDataFileErr _ -> UnreadableDataErr error
                        | DataRangeMismatchErr _
                        | DataUnitsMismatchErr _ -> InconsistentDataErr error)
