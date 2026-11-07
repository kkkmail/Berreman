namespace OpticalConstructor.Storage

open System
open System.Globalization
open System.IO
open System.Text.RegularExpressions
open OpticalConstructor.Domain.LibraryBackup
open OpticalConstructor.Storage.LibraryDatabaseStore
open Softellect.Sys
open Softellect.Sys.Primitives

module LibraryBackupStore =

    type LibraryBackupTime =
        | LibraryBackupTime of DateTime

        member this.value : DateTime = let (LibraryBackupTime time) = this in time

        static member private stampFormat : string = "yyyyMMdd-HHmmss"

        member this.fileStamp : string = this.value.ToString(LibraryBackupTime.stampFormat, CultureInfo.InvariantCulture)

        static member tryCreate (stamp : string) : LibraryBackupTime option =
            match DateTime.TryParseExact(stamp, LibraryBackupTime.stampFormat, CultureInfo.InvariantCulture, DateTimeStyles.None) with
            | true, time -> Some (LibraryBackupTime time)
            | false, _ -> None

    type private LibraryBackupSequence =
        | LibraryBackupSequence of int

        member this.value : int = let (LibraryBackupSequence number) = this in number
        static member first : LibraryBackupSequence = LibraryBackupSequence 1
        member this.next : LibraryBackupSequence = LibraryBackupSequence (this.value + 1)

        member this.fileSuffix : string =
            if this = LibraryBackupSequence.first then String.Empty
            else "-" + this.value.ToString(CultureInfo.InvariantCulture)

        static member tryCreate (suffix : string) : LibraryBackupSequence option =
            if String.IsNullOrEmpty suffix then Some LibraryBackupSequence.first
            else
                match Int32.TryParse(suffix, NumberStyles.None, CultureInfo.InvariantCulture) with
                | true, number when number >= 2 -> Some (LibraryBackupSequence number)
                | _ -> None

    type private LibraryBackupFileInfo =
        {
            time : LibraryBackupTime
            sequence : LibraryBackupSequence
        }

        member this.name : LibraryBackupName = LibraryBackupName $"library-{this.time.fileStamp}{this.sequence.fileSuffix}.db"

        static member tryOfFile (file : FileName) : LibraryBackupFileInfo option =
            let matched = Regex.Match(Path.GetFileName(file.value), @"\Alibrary-([0-9]{8}-[0-9]{6})(?:-([2-9]|[1-9][0-9]+))?\.db\z")
            if matched.Success then
                match LibraryBackupTime.tryCreate matched.Groups[1].Value, LibraryBackupSequence.tryCreate matched.Groups[2].Value with
                | Some time, Some sequence ->
                    Some
                        {
                            time = time
                            sequence = sequence
                        }
                | _ -> None
            else None

    type LibraryBackupProxy with

        [<ElevationBoundary(nameof StorageBoundary, "the library backup store (001 §C): VACUUM INTO and the online restore over the backups folder")>]
        static member createInDatabase (database : LibraryDatabase) (backups : FolderName) (now : unit -> LibraryBackupTime) : LibraryBackupProxy =
            let backupFile (name : LibraryBackupName) : FileName = FileName (Path.Combine(backups.value, name.value))

            let nextBackupName () : LibraryBackupName =
                let time = now ()
                let rec firstAvailable (sequence : LibraryBackupSequence) : LibraryBackupName =
                    let info : LibraryBackupFileInfo =
                        {
                            time = time
                            sequence = sequence
                        }
                    let name = info.name
                    let file = backupFile name
                    if File.Exists(file.value) || Directory.Exists(file.value) then firstAvailable sequence.next
                    else name
                firstAvailable LibraryBackupSequence.first

            let listBackups () : Result<LibraryBackupName list, LibraryBackupError> =
                try
                    Directory.GetFiles(backups.value)
                    |> Array.choose (fun path ->
                        let file = FileName path
                        LibraryBackupFileInfo.tryOfFile file)
                    |> Array.sortByDescending (fun info -> info.time, info.sequence)
                    |> Array.map (fun info -> info.name)
                    |> Array.toList
                    |> Ok
                with
                | :? DirectoryNotFoundException -> Ok []
                | error -> Error (LibraryBackupFailedErr $"'{backups.value}': {error.Message}")

            {
                backUpLibrary =
                    fun () ->
                        try
                            Directory.CreateDirectory(backups.value) |> ignore
                            let name = nextBackupName ()
                            database.vacuumInto (backupFile name)
                            |> Result.mapError (fun error -> LibraryBackupFailedErr error.describe)
                            |> Result.map (fun () -> name)
                        with error -> Error (LibraryBackupFailedErr $"'{backups.value}': {error.Message}")
                listBackups = listBackups
                restoreLibrary =
                    fun name ->
                        listBackups ()
                        |> Result.bind (fun listed ->
                            if not (List.contains name listed) then Error (UnknownLibraryBackupErr $"'{name.value}'")
                            else
                                try
                                    let before = nextBackupName ()
                                    database.restoreFrom (backupFile name) (backupFile before)
                                    |> Result.mapError (fun error -> LibraryRestoreFailedErr error.describe)
                                    |> Result.map (fun () -> before)
                                with error -> Error (LibraryRestoreFailedErr $"'{backups.value}': {error.Message}"))
            }
