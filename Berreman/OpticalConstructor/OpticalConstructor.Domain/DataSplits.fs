namespace OpticalConstructor.Domain

open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.MeasuredData
open OpticalConstructor.Domain.ImportProfiles

/// One lab file split into experiments by its key column, as pure Domain (001 §K "One file → many experiments";
/// spec 0050 S9.3).
module DataSplits =

    /// The column a file is split by, as the split names it (spec 0050 S9.3).
    type SplitKeyColumn =
        | SplitKeyColumn of string

        member this.value : string = let (SplitKeyColumn text) = this in text

    type ChosenImportProfile with

        /// The key column of a file the profile reads: the Phone ROI label, the observation file's sample key or a kit
        /// profile's key column; `None` for an X,Y built-in or a kit profile without a key column (spec 0050 S9.3).
        member this.splitKeyOpt : SplitKeyColumn option =
            match this with
            | BuiltInProfileChoice PhoneRoiCsvProfile -> Some (SplitKeyColumn "experiment")
            | BuiltInProfileChoice MuellerObservationProfile -> Some (SplitKeyColumn "sample")
            | BuiltInProfileChoice IntensitySeriesProfile
            | BuiltInProfileChoice EllipsometricSeriesProfile -> None
            | KitProfileChoice profile -> profile.roles.keyOpt |> Option.map (fun column -> SplitKeyColumn $"column {column.value}")

    /// One group of a split file: its key and how many points hold it (spec 0050 S9.3).
    type KeyedGroup =
        {
            key : DataRowKey
            points : MeasuredPointCount
        }

    type MappedPoints with

        /// The distinct keys of `pointKeys` in order of first appearance, each with its point count, so a group's points
        /// are those `selectedBy` keeps; a point without a key is in no group (spec 0050 S9.3).
        member this.splitGroups : Result<KeyedGroup list, ExperimentDataError> =
            let keys = this.pointKeys |> List.choose id

            let groupOf (key : DataRowKey) : KeyedGroup =
                {
                    key = key
                    points = keys |> List.filter (fun held -> held = key) |> List.length |> MeasuredPointCount
                }

            match this, keys with
            | SeriesPoints _, _ -> Error (MalformedDataFileErr "an X,Y data file has no key column to split by")
            | ColumnPoints _, [] -> Error (MalformedDataFileErr "the import profile maps no key column to split by")
            | CapturePoints _, _
            | ObservationPoints _, _
            | ColumnPoints _, _ -> keys |> List.distinct |> List.map groupOf |> Ok
