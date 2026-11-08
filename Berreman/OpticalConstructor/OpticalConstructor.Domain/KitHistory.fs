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

        /// The one folder comparison, ordinal ignoring case as `RecentFiles.bump` compares paths: `change` maps the
        /// matching entry, `None` drops it. It answers no `bool` so no primitive sits beside our types (spec 0050 S5.1).
        member private this.changeEntry (folder : KitFolder) (change : KitHistoryEntry -> KitHistoryEntry option) : KitHistory =
            let changed =
                this.entries
                |> List.choose (fun entry ->
                    match String.Equals(entry.folder.value.value, folder.value.value, StringComparison.OrdinalIgnoreCase) with
                    | true -> change entry
                    | false -> Some entry)

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

        /// The starred kits, then the rest, each group newest open first and equal times in stored order (spec 0050 S5.1).
        member this.displayOrder : KitHistoryEntry list =
            let newestFirst (star : KitStar) : KitHistoryEntry list =
                this.entries
                |> List.filter (fun entry -> entry.star = star)
                |> List.sortByDescending _.lastOpened

            newestFirst StarredKit @ newestFirst UnstarredKit
