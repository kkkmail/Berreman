namespace OpticalConstructor.Domain

open System
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.MeasuredData
open OpticalConstructor.Domain.ImportProfiles
open OpticalConstructor.Domain.Experiments
open OpticalConstructor.Domain.Setups
open OpticalConstructor.Domain.NamingTemplates
open OpticalConstructor.Domain.SetupGrids

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

    type DataRowKey with

        /// The key as a name the naming template reads, as `MuellerExperimentLabel.nameText` is (spec 0050 S9.3, S8.3).
        member this.nameText : NameText = let (DataRowKey text) = this in NameText text

    /// Why a group of a split file matches no experiment: the error that stopped it, or the earlier group that holds
    /// its experiment (spec 0050 S9.3).
    type DataSplitError =
        | SplitKeyTextErr of ExperimentError
        | SplitKeyNameErr of NamingTemplateError
        | SplitKeySetupErr of SetupError
        | SplitKeyTwiceErr of reason : string

        member this.describe : string =
            match this with
            | SplitKeyTextErr error -> $"the key is no experiment name: {error.describe}"
            | SplitKeyNameErr error -> $"the key does not read through the naming template: {error.describe}"
            | SplitKeySetupErr error -> $"the table cannot hold the key's setup: {error.describe}"
            | SplitKeyTwiceErr reason -> $"two groups match one experiment: {reason}"

    /// The setup, held or new, and the new experiment a group would be attached to (spec 0050 S9.3; 001 §K).
    type SplitSetupOffer =
        {
            setup : Setup
            experiment : ExperimentName
        }

    /// How a group is matched: the kit's experiment of its name, a setup and experiment its name describes, or none and
    /// why. A split is never stored, so it carries no wire tag (spec 0050 S9.3; 001 §K, §P `MatchByTemplate`).
    type SplitGroupMatch =
        | ExperimentForGroup of experiment : ExperimentName
        | SetupOfferForGroup of offer : SplitSetupOffer
        | GroupWithoutMatch of problem : DataSplitError

    /// One group of a split file and how it is matched (spec 0050 S9.3).
    type SplitGroup =
        {
            key : DataRowKey
            points : MeasuredPointCount
            matching : SplitGroupMatch
        }

    /// What a split is matched against: the kit's template, experiments, setups and items, the table, and how an item
    /// resolves to its model (the window's `tryItemModelChoice`) (spec 0050 S9.3).
    [<ReferenceEquality>]
    type DataSplitParam =
        {
            template : NamingTemplate
            experiments : Experiment list
            setups : Setup list
            table : SetupSlot list
            items : Item list
            choiceOf : Item -> ItemModelChoice option
        }

    /// A split file's groups in file order, each with its match (spec 0050 S9.3; 001 §K).
    type DataSplit =
        {
            groups : SplitGroup list
        }

        /// The groups matched to one of the kit's experiments, in order (spec 0050 S9.3).
        member this.matched : SplitGroup list =
            this.groups
            |> List.filter (fun group ->
                match group.matching with
                | ExperimentForGroup _ -> true
                | SetupOfferForGroup _
                | GroupWithoutMatch _ -> false)

        /// The groups offered a setup and experiment, in order (spec 0050 S9.3).
        member this.offered : SplitGroup list =
            this.groups
            |> List.filter (fun group ->
                match group.matching with
                | SetupOfferForGroup _ -> true
                | ExperimentForGroup _
                | GroupWithoutMatch _ -> false)

        /// The groups matched to nothing, in order (spec 0050 S9.3).
        member this.unmatched : SplitGroup list =
            this.groups
            |> List.filter (fun group ->
                match group.matching with
                | GroupWithoutMatch _ -> true
                | ExperimentForGroup _
                | SetupOfferForGroup _ -> false)

        /// The one matching rule, group by group in order: the kit's experiment the key names, as written or as the
        /// template writes it back, else the setup and experiment the template reads in it over the table's slots, else
        /// the reason; an experiment an earlier group matched is refused (spec 0050 S9.3; 001 §K). It writes nothing.
        static member ofGroups (param : DataSplitParam) (groups : KeyedGroup list) : DataSplit =
            // The kit's name identity: one file stem, ignoring case (step 286).
            let tryFindNamed (name : ExperimentName) : ExperimentName option =
                param.experiments
                |> List.tryFind (fun experiment ->
                    String.Equals(experiment.name.fileStem.value, name.fileStem.value, StringComparison.OrdinalIgnoreCase))
                |> Option.map (fun experiment -> experiment.name)

            let offersIn (earlier : SplitGroup list) : SplitSetupOffer list =
                earlier
                |> List.choose (fun group ->
                    match group.matching with
                    | SetupOfferForGroup offer -> Some offer
                    | ExperimentForGroup _
                    | GroupWithoutMatch _ -> None)

            let matchedOnce (earlier : SplitGroup list) (key : DataRowKey) (experiment : ExperimentName) : SplitGroupMatch =
                let earlierKeyOpt =
                    earlier
                    |> List.tryPick (fun group ->
                        match group.matching with
                        | ExperimentForGroup held when held = experiment -> Some group.key
                        | ExperimentForGroup _
                        | SetupOfferForGroup _
                        | GroupWithoutMatch _ -> None)

                match earlierKeyOpt with
                | Some earlierKey ->
                    GroupWithoutMatch (SplitKeyTwiceErr $"'{earlierKey.value}' and '{key.value}' both match '{experiment.value}'")
                | None -> ExperimentForGroup experiment

            let offerFor
                (earlier : SplitGroup list)
                (key : DataRowKey)
                (keyName : ExperimentName)
                (reading : NameReading)
                : Result<SplitSetupOffer, DataSplitError> =
                let lightOrder = SetupSlot.inLightOrder param.table

                let choiceIn (slotItems : SlotItem list) (roled : RoledSlot) : SlotChoice =
                    let itemOpt =
                        slotItems
                        |> List.tryFind (fun slotItem -> slotItem.role = roled.role)
                        |> Option.bind (fun slotItem -> param.items |> List.tryFind (fun item -> item.id = slotItem.item))

                    // The name records a dial-mounted item's raw reading, so R1 is the physical angle of it (steps 255, 258).
                    let r1 =
                        match roled.role, itemOpt with
                        | ObjectSlotRole position, Some item ->
                            match reading.tryFindAngle position, item.dial with
                            | Some angle, FixedMounting -> angle
                            | Some angle, DialMounted sign -> sign.physicalAngleOf item.dialZeroAngle angle
                            | None, _ -> roled.slot.placement.r1
                        | ObjectSlotRole _, None
                        | SourceSlotRole, _
                        | PrepSlotRole _, _
                        | AnalyzerSlotRole _, _
                        | DetectorSlotRole _, _ -> roled.slot.placement.r1

                    {
                        itemOpt = itemOpt |> Option.map (fun item -> item.id)
                        r1 = r1
                    }

                let setupOf (slotItems : SlotItem list) (pieces : TemplatePiece list) : Result<Setup, DataSplitError> =
                    let named : SlotRole list =
                        pieces
                        |> List.choose (fun piece ->
                            match piece with
                            | ValuePiece { placeholder = SlotPlaceholder role } -> Some role
                            | ValuePiece _
                            | LiteralPiece _ -> None)

                    let held = param.setups @ (offersIn earlier |> List.map (fun offer -> offer.setup)) |> List.distinct

                    let axisOf (roled : RoledSlot) : SlotOptions =
                        {
                            slot = roled.slot.id
                            choices = [ choiceIn slotItems roled ]
                        }

                    let grid : SetupGrid =
                        {
                            axes = lightOrder |> List.filter (fun roled -> List.contains roled.role named) |> List.map axisOf
                            unticked = []
                        }

                    // One-choice axes make one combination, so `generate` answers one setup or its refusal.
                    match grid.generate param.table param.items param.choiceOf param.template held with
                    | Ok (setup :: _) -> Ok setup
                    | Ok [] -> Error (SplitKeySetupErr (UnknownSetupSlotErr $"'{key.value}': the grid of its slots made no setup"))
                    | Error error -> Error (SplitKeySetupErr error)

                let offerOf (setup : Setup) : SplitSetupOffer =
                    let kitNames = param.experiments |> List.map (fun experiment -> experiment.name)
                    let offeredNames = offersIn earlier |> List.map (fun offer -> offer.experiment)

                    {
                        setup = setup
                        experiment =
                            param.template.experimentNameAmong setup.slots param.items reading.noteOpt (kitNames @ offeredNames) keyName
                    }

                match param.template.itemsOf reading param.items, param.template.pieces with
                | Error error, _
                | _, Error error -> Error (SplitKeyNameErr error)
                | Ok slotItems, Ok pieces ->
                    let isOnTable (role : SlotRole) : bool = lightOrder |> List.exists (fun roled -> roled.role = role)

                    match slotItems |> List.tryFind (fun slotItem -> not (isOnTable slotItem.role)) with
                    | Some missing ->
                        let reason = $"'{key.value}' names '{missing.role.text}', which the table has no slot of"
                        Error (SplitKeySetupErr (UnknownSetupSlotErr reason))
                    | None -> setupOf slotItems pieces |> Result.map offerOf

            let matchingOf (earlier : SplitGroup list) (key : DataRowKey) : SplitGroupMatch =
                let byTemplate (keyName : ExperimentName) : Result<SplitGroupMatch, DataSplitError> =
                    param.template.readName key.nameText
                    |> Result.mapError SplitKeyNameErr
                    |> Result.bind (fun reading ->
                        param.template.writeName reading
                        |> Result.mapError SplitKeyNameErr
                        |> Result.bind (fun written -> written.toExperimentName |> Result.mapError SplitKeyTextErr)
                        |> Result.bind (fun writtenName ->
                            match tryFindNamed writtenName with
                            | Some experiment -> Ok (matchedOnce earlier key experiment)
                            | None -> offerFor earlier key keyName reading |> Result.map SetupOfferForGroup))

                let outcome =
                    key.nameText.toExperimentName
                    |> Result.mapError SplitKeyTextErr
                    |> Result.bind (fun keyName ->
                        match tryFindNamed keyName with
                        | Some experiment -> Ok (matchedOnce earlier key experiment)
                        | None -> byTemplate keyName)

                match outcome with
                | Ok matching -> matching
                | Error problem -> GroupWithoutMatch problem

            let made =
                groups
                |> List.fold (fun (earlier : SplitGroup list) group ->
                    let matched : SplitGroup =
                        {
                            key = group.key
                            points = group.points
                            matching = matchingOf earlier group.key
                        }

                    earlier @ [ matched ]) []

            { groups = made }
