namespace OpticalConstructor.Domain

open OpticalConstructor.Domain.ItemModels
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.Studies
open OpticalConstructor.Domain.FitRuns

/// What a fit run publishes back into the kit and the library: its item-owned values as the items' next revisions
/// (spec 0050 S11.5).
module StudyPublishing =

    /// Why a fit run's calibration cannot be published into the kit's items (spec 0050 S11.5).
    type PublishError =
        /// The fit run is another study's (spec 0050 S11.5).
        | ForeignFitRunErr of reason : string
        /// The run fitted an item the kit no longer holds (spec 0050 S11.5).
        | UnknownPublishedItemErr of reason : string
        /// A fitted value whose sigma the run leaves undetermined (spec 0050 S11.5).
        | UndeterminedSigmaErr of reason : string
        /// The run fitted no item-owned value (spec 0050 S11.5).
        | NoCalibrationErr of reason : string
        /// Every item already holds the run's calibration (spec 0050 S11.5).
        | CalibrationAlreadyPublishedErr of reason : string
        /// An item refused its next revision (spec 0050 S11.5).
        | PublishItemErr of error : ItemError

        member this.describe : string =
            match this with
            | ForeignFitRunErr reason -> $"the fit run belongs to another study: {reason}"
            | UnknownPublishedItemErr reason -> $"the kit has no such item: {reason}"
            | UndeterminedSigmaErr reason -> $"the fit leaves a sigma undetermined: {reason}"
            | NoCalibrationErr reason -> $"the fit run calibrates no item: {reason}"
            | CalibrationAlreadyPublishedErr reason -> $"the calibration is already published: {reason}"
            | PublishItemErr error -> $"the item refused the calibration: {error.describe}"

    type Study with

        /// Publish calibration's pure half: each of `items` the run calibrates, in order, at its next revision, every fitted
        /// value a `CalibratedParameter` of its sigma and the run, a link's follower its leader's; session- and run-owned
        /// values stay in the run. It writes nothing (spec 0050 S11.5; 001 §I, §L, §P).
        member this.calibratedItemsWith (run : FitRun) (items : Item list) : Result<Item list, PublishError> =
            let named = $"'{run.name.value}'"

            // A session or experiment owner holds one value per block or run, so its entry is no item's (001 §L).
            let itemOwned : (FitRunEntry * ItemId * ParameterName) list =
                run.entries
                |> List.choose (fun entry ->
                    match entry.parameter, entry.owner with
                    | ItemParameterRef (item, name), ItemOwner owner when item = owner -> Some (entry, item, name)
                    | _ -> None)

            let located : Result<(FitRunEntry * Item * ParameterName) list, PublishError> =
                itemOwned
                |> List.map (fun (entry, itemId, name) ->
                    match items |> List.tryFind (fun item -> item.id = itemId) with
                    | Some item -> Ok (entry, item, name)
                    | None -> Error (UnknownPublishedItemErr $"{itemId.value}, which {named} fits, is not among the kit's {items.Length} items"))
                |> ItemEvaluation.sequenced

            // A sigma the run does not record is as undetermined as step 396's `sigmaOpt = None`.
            let calibratedOf (entry : FitRunEntry, item : Item, name : ParameterName) : Result<ItemId * Parameter, PublishError> =
                let reference : FitEntryRef = { parameter = entry.parameter; owner = entry.owner }

                match run.results.sigmas |> List.tryFind (fun held -> held.entry = reference) |> Option.bind (fun held -> held.sigmaOpt) with
                | Some sigma ->
                    let calibration : CalibratedParameterData = { value = entry.fitted; sigma = sigma; fitRun = run.name }
                    Ok (item.id, ({ name = name; state = CalibratedParameter calibration } : Parameter))
                | None -> Error (UndeterminedSigmaErr $"'{item.label.value}' {name.value} in {named}")

            let calibrations : Result<(ItemId * Parameter) list, PublishError> =
                if run.study <> this.name then
                    Error (ForeignFitRunErr $"{named} is a fit run of '{run.study.value}', not of '{this.name.value}'")
                else
                    located |> Result.bind (fun entries -> entries |> List.map calibratedOf |> ItemEvaluation.sequenced)

            calibrations
            |> Result.bind (fun calibrated ->
                match calibrated with
                | [] -> Error (NoCalibrationErr $"{named} fits {run.entries.Length} values, and a session or an experiment owns each")
                | _ ->
                    // A follower takes its leader's state, so stage 2 finds it calibrated (`stageWith`, step 385).
                    let followers : (ItemId * Parameter) list =
                        this.links
                        |> List.choose (fun link ->
                            match link.follower, link.leader with
                            | ItemParameterRef (followerItem, followerName), ItemParameterRef (leaderItem, leaderName)
                                when items |> List.exists (fun item -> item.id = followerItem) ->
                                calibrated
                                |> List.tryFind (fun (owner, parameter) -> owner = leaderItem && parameter.name = leaderName)
                                |> Option.map (fun (_, parameter) -> followerItem, { parameter with name = followerName })
                            | _ -> None)

                    // A state the item already holds is dropped, so publishing one run twice writes no second revision.
                    let revisions : (Item * Parameter list) list =
                        items
                        |> List.choose (fun item ->
                            let ownOf (published : (ItemId * Parameter) list) : Parameter list =
                                published |> List.filter (fun (owner, _) -> owner = item.id) |> List.map snd

                            let fresh =
                                ownOf calibrated @ ownOf followers
                                |> List.filter (fun parameter ->
                                    match item.tryFindParameter parameter.name with
                                    | Some held -> held.state <> parameter.state
                                    | None -> true)

                            match fresh with
                            | [] -> None
                            | _ -> Some (item, fresh))

                    match revisions with
                    | [] -> Error (CalibrationAlreadyPublishedErr $"every item {named} calibrates already holds its values")
                    | _ ->
                        revisions
                        |> List.map (fun (item, fresh) -> item.publishRevision fresh |> Result.mapError PublishItemErr)
                        |> ItemEvaluation.sequenced)
