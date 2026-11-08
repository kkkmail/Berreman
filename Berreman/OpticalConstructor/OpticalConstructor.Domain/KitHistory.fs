namespace OpticalConstructor.Domain

open System
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.WorkbenchSettings

/// The kits the user has opened, with their stars, eviction and display order (spec 0050 S5.1).
module KitHistory =

    /// Whether the user starred a kit in the history; its disk tag is an int (spec 0050 S5.1).
    type KitStar =
        | StarredKit
        | UnstarredKit

        member this.value : int =
            match this with
            | StarredKit -> 1
            | UnstarredKit -> 2

        static member tryOfWire (tag : int) : KitStar option =
            match tag with
            | 1 -> Some StarredKit
            | 2 -> Some UnstarredKit
            | _ -> None

    /// One kit in the history. `name` is the kit's name at its last open, so a row renders without
    /// opening the kit, and `lastOpened` comes from the injected clock (spec 0050 S5.1).
    type KitHistoryEntry =
        {
            folder : KitFolder
            kitId : KitId
            name : KitName
            lastOpened : ClockTime
            star : KitStar
        }

    /// The whole history, `entries` held newest open first; a kit is identified by its folder (spec 0050 S5.1).
    type KitHistory =
        {
            schemaVersion : FileSchemaVersion
            entries : KitHistoryEntry list
        }

        static member currentSchemaVersion : FileSchemaVersion = FileSchemaVersion 1

        static member empty : KitHistory =
            {
                schemaVersion = KitHistory.currentSchemaVersion
                entries = []
            }

        /// The one folder comparison, ordinal ignoring case: `Some entry` when `entry` is the kit in `folder`. It
        /// answers no `bool` so no primitive sits beside our types (spec 0050 S5.1).
        static member private tryMatchFolder (folder : KitFolder) (entry : KitHistoryEntry) : KitHistoryEntry option =
            match String.Equals(entry.folder.value.value, folder.value.value, StringComparison.OrdinalIgnoreCase) with
            | true -> Some entry
            | false -> None

        /// `change` maps the kit in `folder`, `None` drops it; every other entry is kept (spec 0050 S5.1).
        member private this.changeEntry (folder : KitFolder) (change : KitHistoryEntry -> KitHistoryEntry option) : KitHistory =
            let changed =
                this.entries
                |> List.choose (fun entry ->
                    match KitHistory.tryMatchFolder folder entry with
                    | Some matched -> change matched
                    | None -> Some entry)

            { this with entries = changed }

        /// Stars the kit in `folder`, keeping its place, time and other fields (spec 0050 S5.1).
        member this.star (folder : KitFolder) : KitHistory =
            this.changeEntry folder (fun entry -> Some { entry with star = StarredKit })

        /// Unstars the kit in `folder`, keeping its place, time and other fields (spec 0050 S5.1).
        member this.unstar (folder : KitFolder) : KitHistory =
            this.changeEntry folder (fun entry -> Some { entry with star = UnstarredKit })

        /// Removes the kit in `folder`, starred or not; the folder itself is untouched (spec 0050 S5.1, S5.2).
        member this.forget (folder : KitFolder) : KitHistory =
            this.changeEntry folder (fun _ -> None)

        /// Drops unstarred kits, the earliest open first and the later of equal times first, until at most `limit`
        /// remain; a starred kit is never dropped and the survivors keep their order (spec 0050 S5.1).
        member this.evict (limit : KitHistoryLimit) : KitHistory =
            let dropped =
                this.entries
                |> List.indexed
                |> List.filter (fun (_, entry) -> entry.star = UnstarredKit)
                |> List.sortBy (fun (index, entry) -> entry.lastOpened, -index)
                |> List.truncate (max 0 (this.entries.Length - limit.value))
                |> List.map fst
                |> Set.ofList

            let kept =
                this.entries
                |> List.indexed
                |> List.filter (fun (index, _) -> not (dropped.Contains index))
                |> List.map snd

            { this with entries = kept }

        /// Puts the kit opened from `folder` first, with `kitFile`'s id and name, one reading of `clock` and the star of
        /// the entry it replaces (unstarred when new), then evicts down to `limit`, which can drop it (spec 0050 S5.1).
        member this.bump (limit : KitHistoryLimit) (clock : ClockProvider) (folder : KitFolder) (kitFile : KitFile) : KitHistory =
            let star =
                this.entries
                |> List.tryPick (KitHistory.tryMatchFolder folder)
                |> Option.map _.star
                |> Option.defaultValue UnstarredKit

            let opened : KitHistoryEntry =
                {
                    folder = folder
                    kitId = kitFile.id
                    name = kitFile.name
                    lastOpened = clock.now ()
                    star = star
                }

            let others = this.forget folder
            let reopened = { others with entries = opened :: others.entries }
            reopened.evict limit

        /// The starred kits, then the rest, each group newest open first and equal times in stored order (spec 0050 S5.1).
        member this.displayOrder : KitHistoryEntry list =
            let newestFirst (star : KitStar) : KitHistoryEntry list =
                this.entries
                |> List.filter (fun entry -> entry.star = star)
                |> List.sortByDescending _.lastOpened

            newestFirst StarredKit @ newestFirst UnstarredKit

    /// Whether a history entry's folder is on disk now; it is probed, never stored, so it has no tag (spec 0050 S5.1).
    type KitFolderPresence =
        | KitFolderPresent
        | KitFolderMissing

    /// Why the kit history failed; the reason names the file or folder and the cause (spec 0050 S5.1).
    type KitHistoryError =
        | KitHistoryWriteErr of reason : string

        member this.describe : string =
            match this with
            | KitHistoryWriteErr reason -> $"the kit history could not be written: {reason}"

    /// The kit history's seam: Storage's `KitHistoryProxy.create` at the composition root, `createInMemory` the double (spec 0050 S5.1).
    [<ReferenceEquality>]
    type KitHistoryProxy =
        {
            /// Answers the stored history, and the empty history when none is stored or it cannot be read, so it never fails.
            readHistory : unit -> KitHistory
            /// Replaces the stored history whole.
            writeHistory : KitHistory -> Result<unit, KitHistoryError>
            /// Answers whether a folder exists now, which a history row shows as its missing state.
            probeFolder : KitFolder -> KitFolderPresence
        }

    type KitHistoryProxy with

        /// The double: it starts empty, keeps the last history written and sees only `presentFolders` on disk.
        /// Its store is mutated only inside the closure, as `SourceProxy.createInMemory` keeps its own (spec 0050 S5.1).
        static member createInMemory (presentFolders : KitFolder list) : KitHistoryProxy =
            let store = ref KitHistory.empty

            {
                readHistory = fun () -> store.Value
                writeHistory =
                    fun history ->
                        store.Value <- history
                        Ok ()
                probeFolder =
                    fun folder ->
                        match presentFolders |> List.contains folder with
                        | true -> KitFolderPresent
                        | false -> KitFolderMissing
            }
