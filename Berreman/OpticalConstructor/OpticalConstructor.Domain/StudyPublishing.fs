namespace OpticalConstructor.Domain

open Berreman.Geometry
open OpticalConstructor.Domain.Placement
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.ItemModels
open OpticalConstructor.Domain.ItemEvaluation
open OpticalConstructor.Domain.ExperimentRun
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.Studies
open OpticalConstructor.Domain.FitRuns

/// A fit run's calibration revisions and library promotion values, without writing either store (spec 0050 S11.5).
module StudyPublishing =

    /// Why a fit run's calibration or library promotion is refused (spec 0050 S11.5).
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
        /// The material was not opened, or the run fitted none of the subject's values (spec 0050 S11.5).
        | UnfittedPromotionErr of reason : string
        /// A fitted component has no single value shared by its subject (spec 0050 S11.5).
        | ScopedPromotionErr of reason : string
        /// The item's model is not a black-box Mueller matrix (spec 0050 S11.5).
        | NotBlackBoxErr of reason : string
        /// The study refused a promoted component's state (spec 0050 S11.5).
        | PromotionStudyErr of error : StudyError
        /// The fitted black-box matrix could not be evaluated (spec 0050 S11.5).
        | PromotionEvaluationErr of error : ItemEvaluationError

        member this.describe : string =
            match this with
            | ForeignFitRunErr reason -> $"the fit run belongs to another study: {reason}"
            | UnknownPublishedItemErr reason -> $"the kit has no such item: {reason}"
            | UndeterminedSigmaErr reason -> $"the fit leaves a sigma undetermined: {reason}"
            | NoCalibrationErr reason -> $"the fit run calibrates no item: {reason}"
            | CalibrationAlreadyPublishedErr reason -> $"the calibration is already published: {reason}"
            | PublishItemErr error -> $"the item refused the calibration: {error.describe}"
            | UnfittedPromotionErr reason -> $"the fit run fitted none of its values: {reason}"
            | ScopedPromotionErr reason -> $"the fit gives it no single value: {reason}"
            | NotBlackBoxErr reason -> $"the item is not a black-box Mueller matrix: {reason}"
            | PromotionStudyErr error -> $"the study refused the promoted value: {error.describe}"
            | PromotionEvaluationErr error -> $"the fitted matrix could not be evaluated: {error.describe}"

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

    type ItemId with

        /// The item's library identity keeps its GUID across promotions (spec 0050 S11.5).
        member this.toLibraryEntryId : LibraryEntryId = LibraryEntryId this.value

        /// The item's preset identity is its GUID's canonical text (spec 0050 S11.5).
        member this.toElementValueId : ElementValueId = ElementValueId (this.value.ToString("D"))

    type ItemLabel with

        /// A promoted preset keeps the item's label as its entry name (spec 0050 S11.5).
        member this.entryName : EntryName = EntryName this.value

    type FitRun with

        /// A promoted value's provenance names the kit and the study and fit-run files (spec 0050 S11.5).
        member this.provenanceIn (kit : KitId) : EntryProvenance =
            FitProvenance
                {
                    kit = kit
                    studyStem = this.study.fileStem
                    fitRunStem = this.name.fileStem
                }

    /// What PromoteToLibrary names: an opened material version or a black-box item; derived, never stored
    /// (spec 0050 S11.5).
    type PromotionSubject =
        | MaterialPromotion of MaterialVersionId
        | BlackBoxPromotion of ItemId

    type Study with

        /// The opened entry with the crystal the run produced, preserving all its metadata; unchanged components
        /// come from the fit's snapshot, and values owned per session or run are refused (spec 0050 S11.5).
        member this.promotedMaterialWith (run : FitRun) (version : MaterialVersionId) (entry : MaterialEntry) : Result<MaterialEntry, PublishError> =
            if run.study <> this.name then
                Error (ForeignFitRunErr $"'{run.name.value}' is a fit run of '{run.study.value}', not of '{this.name.value}'")
            else
                match this.materials |> List.tryFind (fun material -> material.version = version) with
                | None ->
                    Error (UnfittedPromotionErr $"'{this.name.value}' did not open material {version.materialId.value} v{version.version.value}")
                | Some material ->
                    let fitted =
                        run.entries
                        |> List.filter (fun fitted ->
                            match fitted.parameter with
                            | MaterialParameterRef (held, _) -> held = version
                            | ItemParameterRef _ -> false)

                    match fitted |> List.tryFind (fun fitted -> fitted.owner <> MaterialOwner version) with
                    | Some scoped -> Error (ScopedPromotionErr $"{scoped.parameter.describe} in '{run.name.value}'")
                    | None ->
                        match fitted with
                        | [] -> Error (UnfittedPromotionErr $"'{material.name.value}' in '{run.name.value}'")
                        | _ ->
                            material.parameters
                            |> List.fold (fun (held : Result<OpenedMaterial, StudyError>) (parameter : Parameter) ->
                                held
                                |> Result.bind (fun copy ->
                                    let reference = MaterialParameterRef (version, parameter.name)
                                    let state =
                                        match fitted |> List.tryFind (fun fitted -> fitted.parameter = reference) with
                                        | Some fitted -> Ok (KnownParameter fitted.fitted)
                                        | None ->
                                            match run.parameters |> List.tryFind (fun recorded -> recorded.parameter = reference) with
                                            | Some recorded -> Ok recorded.state
                                            | None -> Error (UnknownStudyParameterErr $"{reference.describe} is absent from '{run.name.value}' parameter snapshot")
                                    state |> Result.bind (copy.withParameterState parameter.name))) (Ok material)
                            |> Result.mapError PromotionStudyErr
                            |> Result.map (fun copy ->
                                {
                                    entry with
                                        physics = CrystalParameterPhysics copy.currentCrystal
                                })

    type FitRun with

        /// A fitted black box as one constant-Mueller preset component at reference orientation, under the item's
        /// identity and label; unchanged values come from the fit's snapshot, and scoped fits are refused (spec 0050 S11.5).
        member this.promotedBlackBoxWith (item : Item) (choice : ItemModelChoice) : Result<PolarizerPreset, PublishError> =
            match choice with
            | LibraryModelChoice { entry = { model = OpticalElementModel (BlackBoxMuellerModel _ as form) } } ->
                let fitted =
                    this.entries
                    |> List.filter (fun fitted ->
                        match fitted.parameter with
                        | ItemParameterRef (held, _) -> held = item.id
                        | MaterialParameterRef _ -> false)

                match fitted |> List.tryFind (fun fitted -> fitted.owner <> ItemOwner item.id) with
                | Some scoped -> Error (ScopedPromotionErr $"'{item.label.value}' {scoped.parameter.describe} in '{this.name.value}'")
                | None ->
                    match fitted with
                    | [] -> Error (UnfittedPromotionErr $"'{item.label.value}' in '{this.name.value}'")
                    | _ ->
                        item.parameters
                        |> List.map (fun parameter ->
                            let reference = ItemParameterRef (item.id, parameter.name)
                            match fitted |> List.tryFind (fun fitted -> fitted.parameter = reference) with
                            | Some fitted ->
                                Ok
                                    {
                                        parameter with
                                            state = KnownParameter fitted.fitted
                                    }
                            | None ->
                                match this.parameters |> List.tryFind (fun recorded -> recorded.parameter = reference) with
                                | Some recorded ->
                                    Ok
                                        {
                                            parameter with
                                                state = recorded.state
                                        }
                                | None -> Error (MissingParameterValueErr $"{reference.describe} is absent from '{this.name.value}' parameter snapshot"))
                        |> ItemEvaluation.sequenced
                        |> Result.map ParameterValues.ofParameters
                        |> Result.bind (fun values -> BoundOpticalElement.ofOpticalForm values form)
                        |> Result.bind (fun bound ->
                            let reference = ElementOrientation.transmittingAt Angle.zero
                            let waveLength = SourcePreset.operatingWaveLengthOf None None
                            bound.muellerAt reference waveLength)
                        |> Result.mapError PromotionEvaluationErr
                        |> Result.map (fun matrix ->
                            {
                                id = item.id.toElementValueId
                                name = item.label.entryName
                                behavior =
                                    ConstantMueller
                                        [
                                            {
                                                matrix = matrix
                                                offset = Angle.zero
                                            }
                                        ]
                                category = CustomMueller
                                protection = UserManaged
                            })
            | LibraryModelChoice _
            | SourcePresetChoice _
            | PolarizerPresetChoice _
            | StackSampleChoice _ -> Error (NotBlackBoxErr item.label.value)

        /// The run's fitted opened materials in study order, then its fitted black-box items in item order;
        /// promotion performs the ownership checks (spec 0050 S11.5).
        member this.promotionSubjectsWith (study : Study) (items : Item list) (choiceOf : Item -> ItemModelChoice option) : PromotionSubject list =
            let materials =
                study.materials
                |> List.choose (fun material ->
                    this.entries
                    |> List.tryFind (fun fitted ->
                        match fitted.parameter with
                        | MaterialParameterRef (version, _) -> version = material.version
                        | ItemParameterRef _ -> false)
                    |> Option.map (fun _ -> MaterialPromotion material.version))

            let blackBoxes =
                items
                |> List.choose (fun item ->
                    match choiceOf item with
                    | Some (LibraryModelChoice { entry = { model = OpticalElementModel (BlackBoxMuellerModel _) } }) ->
                        this.entries
                        |> List.tryFind (fun fitted ->
                            match fitted.parameter with
                            | ItemParameterRef (owner, _) -> owner = item.id
                            | MaterialParameterRef _ -> false)
                        |> Option.map (fun _ -> BlackBoxPromotion item.id)
                    | _ -> None)

            materials @ blackBoxes
