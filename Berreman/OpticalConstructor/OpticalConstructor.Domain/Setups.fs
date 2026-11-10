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

        /// Each item the current slots and then each earlier revision's slots hold, once, in that order (spec 0050
        /// S8.2).
        member this.heldItems : ItemId list =
            this.slots :: (this.history |> List.map (fun record -> record.slots))
            |> List.concat
            |> List.choose (fun slot -> slot.content.itemOpt)
            |> List.distinct

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
