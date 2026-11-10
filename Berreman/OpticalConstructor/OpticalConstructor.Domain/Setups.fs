namespace OpticalConstructor.Domain

open Berreman.Geometry
open OpticalConstructor.Domain.Placement
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.ItemModels
open OpticalConstructor.Domain.Lifecycle
open OpticalConstructor.Domain.LibraryEntryFiles

/// The setup: an ordered optical path of slots that the table draws, kept in the revisions experiments reference
/// (spec 0050 S8.2; 001 §J).
module Setups =

    /// How a slot's item turns: its R1 is the placement's own, or the item sits on a dial set to this raw reading and
    /// its R1 is the item's `DialSign.physicalAngleOf` of it (spec 0050 S8.2; 001 §I).
    type SlotTurn =
        | PlacementTurn
        | DialTurn of reading : Angle

    /// What a slot holds: nothing, or one of the kit's items and how it turns (spec 0050 S8.2; 001 §J).
    type SlotContent =
        | EmptySlotContent
        | ItemInSlot of item : ItemId * turn : SlotTurn

        member this.itemOpt : ItemId option =
            match this with
            | EmptySlotContent -> None
            | ItemInSlot (item, _) -> Some item

    /// One slot of a setup and the one table element that draws it; an experiment's varied element names its `id`
    /// (spec 0050 S8.2).
    type SetupSlot =
        {
            id : Library.ElementId
            placement : ElementPlacement
            zoom : BeamLayout.ElementZoom
            beam : BeamLayout.BeamHost
            content : SlotContent
        }

        /// The slot as the one ray law lays it out (spec 0050 S8.2; AGENTS.md "One ray law").
        member this.layoutElement : BeamLayout.LayoutElement =
            {
                id = this.id
                placement = this.placement
                host = this.beam
                zoom = this.zoom
            }

    /// An earlier revision's slots, kept for the experiments that reference it (spec 0050 S8.2).
    type SetupRevisionRecord =
        {
            revision : SetupRevision
            slots : SetupSlot list
        }

    /// A named setup: its slots in the table's order, the light source first, and its earlier revisions oldest first,
    /// as an item keeps its history (spec 0050 S8.2; 001 §J).
    type Setup =
        {
            name : SetupName
            revision : SetupRevision
            slots : SetupSlot list
            history : SetupRevisionRecord list
        }

        /// The setup's name at its current revision (spec 0050 S8.2).
        member this.reference : SetupReference =
            {
                name = this.name
                revision = this.revision
            }

        /// The slots the setup held at that revision; a revision it never had is refused (spec 0050 S8.2).
        member this.slotsAt (revision : SetupRevision) : Result<SetupSlot list, SetupError> =
            if revision = this.revision then
                Ok this.slots
            else
                match this.history |> List.tryFind (fun record -> record.revision = revision) with
                | Some record -> Ok record.slots
                | None ->
                    let missing : SetupReference = { name = this.name; revision = revision }
                    Error (UnknownSetupRevisionErr missing.describe)

        member private this.slotsOfEveryRevision : SetupSlot list =
            this.slots :: (this.history |> List.map (fun record -> record.slots)) |> List.concat

        /// Each item the current slots and then each earlier revision's slots hold, once, in that order (spec 0050
        /// S8.2).
        member this.heldItems : ItemId list =
            this.slotsOfEveryRevision
            |> List.choose (fun slot -> slot.content.itemOpt)
            |> List.distinct

        /// Each library entry the current slots and then each earlier revision's slots bind, once, at its latest: every
        /// revision an experiment may reference needs its entries in the kit (spec 0050 S8.2; step 147's scene rule).
        member this.libraryReferences : EntryReference list =
            this.slotsOfEveryRevision
            |> List.choose (fun slot -> slot.placement.valueId |> Option.bind (fun valueId -> valueId.tryToLibraryEntryId))
            |> List.distinct
            |> List.map EntryLatestReference

        /// A new setup at its first revision, refused when a held setup's file shares its name's, as every
        /// `<name>.setup-json` shares the kit root: CreateSetup's pure half (spec 0050 S8.2; 001 §P).
        static member create (name : SetupName) (slots : SetupSlot list) (held : Setup list) : Result<Setup, SetupError> =
            // A name clashes exactly when the shared sequence would not keep it, so the stem test stays in one place.
            let sameFile (other : Setup) : string option =
                if FileStem.firstFreeText name.value [ other.name.value ] = name.value then
                    None
                else
                    let file = SetupKitFile.fileNameOf other.name.fileStem
                    Some $"'{name.value}' beside '{other.name.value}': both are stored as {file.value}, ignoring case"

            match held |> List.tryPick sameFile with
            | Some reason -> Error (DuplicateSetupNameErr reason)
            | None ->
                let created : Setup =
                    {
                        name = name
                        revision = SetupRevision.first
                        slots = slots
                        history = []
                    }

                Ok created

        /// The setup once the table's slots are written over it, by the one version rule: only a pinned revision whose
        /// slots change mints the next, keeping its slots in `history` (spec 0050 S8.2; .spec-md §6).
        member this.revisedWith (slots : SetupSlot list) (pin : PinState) : Setup =
            let stored : VersionPayload<SetupSlot list, unit> = { physics = this.slots; metadata = () }
            let incoming : VersionPayload<SetupSlot list, unit> = { physics = slots; metadata = () }

            match decideVersioning stored incoming pin with
            | KeepCurrent -> this
            | MutateInPlace -> { this with slots = slots }
            | MintNextVersion ->
                let replaced : SetupRevisionRecord =
                    {
                        revision = this.revision
                        slots = this.slots
                    }

                {
                    this with
                        revision = this.revision.next
                        slots = slots
                        history = this.history @ [ replaced ]
                }

    /// One setup's versioned `<name>.setup-json` content, replacing S4.3's scene file; a file is read back as its
    /// `setup` (spec 0050 S8.2; 001 §A).
    type SetupFile =
        {
            schemaVersion : FileSchemaVersion
            setup : Setup
        }

        /// The only supported setup file schema version (spec 0050 S8.2).
        static member currentSchemaVersion : FileSchemaVersion = FileSchemaVersion 1

    type Setup with

        /// The setup in the current setup file schema (spec 0050 S8.2).
        member this.setupFile : SetupFile =
            {
                schemaVersion = SetupFile.currentSchemaVersion
                setup = this
            }

    /// A slot's 1-based place within its role on the light path (spec 0050 S8.2; 001 §J).
    type SlotPosition =
        | SlotPosition of int

        member this.value : int = let (SlotPosition position) = this in position

    /// A slot's role on the light path: derived from its kind and where the light reaches it, never stored, so it
    /// carries no wire tag (spec 0050 S8.2; 001 §J).
    type SlotRole =
        | SourceSlotRole
        | PrepSlotRole of position : SlotPosition
        | ObjectSlotRole of position : SlotPosition
        | AnalyzerSlotRole of position : SlotPosition
        | DetectorSlotRole of position : SlotPosition

        /// The role's chip name on the slot bar (spec 0050 S8.2; 001 §J).
        member this.text : string =
            match this with
            | SourceSlotRole -> "Source"
            | PrepSlotRole position -> $"Prep {position.value}"
            | ObjectSlotRole position -> $"Obj {position.value}"
            | AnalyzerSlotRole position -> $"Analyzer {position.value}"
            | DetectorSlotRole position -> $"Detector {position.value}"

    /// A setup slot and the role the light gives it (spec 0050 S8.2).
    type RoledSlot =
        {
            role : SlotRole
            slot : SetupSlot
        }

    type SetupSlot with

        /// The slots' roles by the one ray law, the first slot its root: the source, the preps (no sample yet on their
        /// path), the objects, the analyzers (behind a sample, on its reflected ray too) and the detectors, each by
        /// light depth, then table order, an unreached slot last (spec 0050 S8.2; 001 §J; AGENTS.md "One ray law").
        static member inLightOrder (slots : SetupSlot list) : RoledSlot list =
            let layout = slots |> List.map (fun slot -> slot.layoutElement) |> BeamLayout.layout

            // The layout keeps the input order; an unreached slot's path is empty, as only the root's may be.
            let inLight : (SetupSlot * BeamLayout.PlacedElement) list =
                match List.zip slots layout.elements with
                | [] -> []
                | root :: others ->
                    let reached =
                        others
                        |> List.filter (fun (_, placed) -> not (List.isEmpty placed.path))
                        |> List.sortBy (fun (_, placed) -> placed.path.Length)

                    let unreached = others |> List.filter (fun (_, placed) -> List.isEmpty placed.path)
                    root :: reached @ unreached

            let objects : Set<Library.ElementId> =
                slots |> List.filter (fun slot -> slot.placement.catalogueKind = Sample) |> List.map (fun slot -> slot.id) |> Set.ofList

            let behindObject (placed : BeamLayout.PlacedElement) : bool =
                placed.path |> List.exists (fun step -> Set.contains step.element objects)

            let isOptic (slot : SetupSlot) : bool =
                match slot.placement.catalogueKind with
                | LightSource | Sample | Detector -> false
                | LinearPolarizer | CircularPolarizer | FlatMirror | CurvedMirror | Lens -> true

            let ofKind (kind : CatalogueKind) (slot : SetupSlot) (_ : BeamLayout.PlacedElement) : bool =
                slot.placement.catalogueKind = kind

            let slotsWhere (inRole : SetupSlot -> BeamLayout.PlacedElement -> bool) : SetupSlot list =
                inLight |> List.filter (fun (slot, placed) -> inRole slot placed) |> List.map fst

            let numbered (role : SlotPosition -> SlotRole) (members : SetupSlot list) : RoledSlot list =
                members
                |> List.mapi (fun index slot ->
                    let position = SlotPosition (index + 1)
                    { role = role position; slot = slot })

            [
                slotsWhere (ofKind LightSource) |> List.map (fun slot -> { role = SourceSlotRole; slot = slot })
                slotsWhere (fun slot placed -> isOptic slot && not (behindObject placed)) |> numbered PrepSlotRole
                slotsWhere (ofKind Sample) |> numbered ObjectSlotRole
                slotsWhere (fun slot placed -> isOptic slot && behindObject placed) |> numbered AnalyzerSlotRole
                slotsWhere (ofKind Detector) |> numbered DetectorSlotRole
            ]
            |> List.concat

    type Setup with

        /// The setup's current slots with their roles in light order (spec 0050 S8.2).
        member this.lightOrder : RoledSlot list = SetupSlot.inLightOrder this.slots

    type ItemModelChoice with

        /// The library entry a table element is bound to while the item sits on it; a library model binds none (spec
        /// 0050 S8.2; `Library.LibraryEntry`).
        member this.libraryEntryOpt : Library.LibraryEntry option =
            match this with
            | SourcePresetChoice (_, source) -> Some (Library.SourceItem source)
            | PolarizerPresetChoice (_, polarizer) -> Some (Library.PolarizerItem polarizer)
            | StackSampleChoice (_, _, sample) -> Some (Library.SampleItem sample)
            | LibraryModelChoice _ -> None

        /// The slot kinds the item may fill: its library entry's `forKinds`, the rule the table's binding applies, else
        /// its model form's (spec 0050 S8.2; 001 §J "each dropdown lists compatible items only").
        member this.slotKinds : CatalogueKind list =
            let opticalKinds (form : OpticalForm) : CatalogueKind list =
                match form with
                | RealPolarizerModel _ -> [ LinearPolarizer ]
                | RetarderPlateModel _ -> [ LinearPolarizer; CircularPolarizer; Sample ]
                | BlackBoxMuellerModel _ -> [ Sample ]
                // The compound polarizer presets fit both roles, so a compound model does too.
                | CompoundModel _ -> [ LinearPolarizer; CircularPolarizer ]

            match this with
            | LibraryModelChoice model ->
                match model.entry.model with
                | OpticalElementModel form -> opticalKinds form
                | EmptySlotModel -> [ LinearPolarizer; CircularPolarizer; Sample ]
                | IntensityDetectorModel _
                | MuellerPolarimeterModel _
                | EllipsometerModel _ -> [ Detector ]
                // A binding form takes its kinds from its binding; chosen without one, `ItemDraft.addTo` refuses it.
                | SourcePresetItemModel
                | IdealPolarizerItemModel
                | StackSampleItemModel -> []
            | SourcePresetChoice _
            | PolarizerPresetChoice _
            | StackSampleChoice _ -> this.libraryEntryOpt |> Option.toList |> List.collect (fun entry -> entry.forKinds)

    /// The dial-mounted item of `items` whose raw reading the slot holds, its dial's sign and that reading (spec 0050
    /// S8.2; 001 §I).
    let private tryDialReadingIn (items : Item list) (content : SlotContent) : (Item * DialSign * Angle) option =
        match content with
        | ItemInSlot (id, DialTurn reading) ->
            match items |> List.tryFind (fun item -> item.id = id) with
            | Some item ->
                match item.dial with
                | DialMounted sign -> Some (item, sign, reading)
                | FixedMounting -> None
            | None -> None
        | ItemInSlot (_, PlacementTurn)
        | EmptySlotContent -> None

    type SetupSlot with

        /// The slot holding the item, refused when its model fits no slot of this kind. A dial-mounted item keeps the
        /// drawn R1 as its reading, and an item over a library entry binds the slot as the table binds that entry
        /// (`withBoundSampleEmission`), so the drawn element and the item agree (spec 0050 S8.2; 001 §P `AssignSlot`).
        member this.withItem (item : Item) (model : ItemModelChoice) : Result<SetupSlot, SetupError> =
            let placement = this.placement

            if model.slotKinds |> List.contains placement.catalogueKind then
                let turn : SlotTurn =
                    match item.dial with
                    | DialMounted sign -> DialTurn (sign.readingOf item.dialZeroAngle placement.r1)
                    | FixedMounting -> PlacementTurn

                let bound : ElementPlacement =
                    match model.libraryEntryOpt with
                    | Some entry ->
                        {
                            placement with
                                valueId = Some entry.entryId
                                emission = Library.placementEmissionForEntry placement.emission entry
                        }
                    | None -> placement

                Ok { this with placement = bound; content = ItemInSlot (item.id, turn) }
            else
                let slotKind : string =
                    match placement.catalogueKind with
                    | LightSource -> "light source"
                    | LinearPolarizer -> "linear polarizer"
                    | CircularPolarizer -> "circular polarizer"
                    | Sample -> "sample"
                    | Lens -> "lens"
                    | FlatMirror -> "flat mirror"
                    | CurvedMirror -> "curved mirror"
                    | Detector -> "detector"

                Error (IncompatibleSlotItemErr $"'{item.label.value}' in the {slotKind} slot '{this.id.value}'")

        /// The slot emptied, its placement kept (spec 0050 S8.2).
        member this.withoutItem : SetupSlot = { this with content = EmptySlotContent }

        /// The slot as the table draws it: a dial-mounted item's reading turns R1 to its physical angle through the item's
        /// current zero; every other slot is unchanged (spec 0050 S8.2; 001 §I).
        member this.drawnWith (items : Item list) : SetupSlot =
            match tryDialReadingIn items this.content with
            | Some (item, sign, reading) ->
                { this with placement = { this.placement with r1 = sign.physicalAngleOf item.dialZeroAngle reading } }
            | None -> this

        /// `drawnWith`'s inverse: the table turns a slot in physical angles, and the setup records a dial-mounted item's
        /// reading of its R1 (spec 0050 S8.2; 001 §I "Experiments record raw dial readings").
        member this.recordedWith (items : Item list) : SetupSlot =
            match tryDialReadingIn items this.content with
            | Some (item, sign, _) ->
                { this with content = ItemInSlot (item.id, DialTurn (sign.readingOf item.dialZeroAngle this.placement.r1)) }
            | None -> this

    /// Where a swap lands: the held setup already holding the swapped content, or a new one (spec 0050 S8.2; 001 §J
    /// "choosing one is either a new setup or an existing one").
    type SetupSwap =
        | SwitchToHeldSetup of Setup
        | CreateSwappedSetup of Setup

    type Setup with

        /// The first held setup whose current slots hold, slot by slot in order, the kinds and content of `slots`, else a
        /// new setup `fresh` at its first revision. It writes nothing; the host records (spec 0050 S8.2; 001 §P `SwapSlot`).
        static member swapTarget (slots : SetupSlot list) (held : Setup list) (fresh : SetupName) : SetupSwap =
            let contentOf (setupSlots : SetupSlot list) : (CatalogueKind * SlotContent) list =
                setupSlots |> List.map (fun slot -> slot.placement.catalogueKind, slot.content)

            let swapped = contentOf slots

            match held |> List.tryFind (fun setup -> contentOf setup.slots = swapped) with
            | Some setup -> SwitchToHeldSetup setup
            | None ->
                let created : Setup =
                    {
                        name = fresh
                        revision = SetupRevision.first
                        slots = slots
                        history = []
                    }

                CreateSwappedSetup created

    type Setup with

        /// Pinned while an experiment references the current revision, so a changed table then mints the next by
        /// `Lifecycle.decideVersioning` (spec 0050 S8.2).
        member this.pinFor (experiments : Experiments.Experiment list) : PinState =
            if experiments |> List.exists (fun experiment -> experiment.setup = this.reference) then
                VersionPinned
            else
                VersionUnpinned

    type Experiments.Experiment with

        /// The slots of the setup revision the experiment references; a setup the list lacks is refused, naming the
        /// reference (spec 0050 S8.2).
        member this.slotsIn (setups : Setup list) : Result<SetupSlot list, SetupError> =
            match setups |> List.tryFind (fun setup -> setup.name = this.setup.name) with
            | Some setup -> setup.slotsAt this.setup.revision
            | None -> Error (UnknownSetupErr this.setup.describe)

        /// The items the slots of the experiments' revisions hold, once each in the supplied order. An unresolved revision
        /// answers its `slotsIn` error and a held id the items lack is refused, naming the slot by its role, never matched
        /// by preset or model (spec 0050 S7.2, S8.2).
        static member itemsUsedBy
            (experiments : Experiments.Experiment list)
            (setups : Setup list)
            (items : Item list)
            : Result<Item list, SetupError> =
            let held : Result<(Experiments.Experiment * RoledSlot * ItemId) list, SetupError> =
                experiments
                |> List.fold (fun result experiment ->
                    result |> Result.bind (fun found ->
                        experiment.slotsIn setups
                        |> Result.map (fun slots ->
                            let holding =
                                SetupSlot.inLightOrder slots
                                |> List.choose (fun roled -> roled.slot.content.itemOpt |> Option.map (fun id -> experiment, roled, id))
                            found @ holding))) (Ok [])

            let isHeld (id : ItemId) : bool = items |> List.exists (fun item -> item.id = id)

            held
            |> Result.bind (fun assigned ->
                match assigned |> List.tryFind (fun (_, _, id) -> not (isHeld id)) with
                | Some (experiment, roled, id) ->
                    let slot = $"'{roled.role.text}' ({roled.slot.id.value})"
                    let place = $"the slot {slot} of {experiment.setup.describe}, which {experiment.id.fileStem.value} references"
                    Error (SetupItemReferenceErr (UnknownKitItemErr $"{id.value}, held in {place}"))
                | None ->
                    let used = assigned |> List.map (fun (_, _, id) -> id)
                    items |> List.filter (fun item -> List.contains item.id used) |> List.distinctBy (fun item -> item.id) |> Ok)
