namespace OpticalConstructor.Domain

module LibraryBackup =

    /// One backup's file name within the backups folder, without the folder.
    type LibraryBackupName =
        | LibraryBackupName of string

        member this.value : string = let (LibraryBackupName name) = this in name

    type LibraryBackupError =
        /// Taking a backup or listing the available backups failed.
        | LibraryBackupFailedErr of reason : string
        /// Restoring failed or was refused; the current library is unchanged.
        | LibraryRestoreFailedErr of reason : string
        /// The requested backup name is not among the listed backups.
        | UnknownLibraryBackupErr of reason : string

        member this.describe : string =
            match this with
            | LibraryBackupFailedErr reason -> $"library backup or listing failed: {reason}"
            | LibraryRestoreFailedErr reason -> $"library restore failed or was refused: {reason}"
            | UnknownLibraryBackupErr reason -> $"the library backup is not listed: {reason}"

    /// The Library window's backup and restore seam (spec 0050 001 §C; .spec-md §10.5).
    /// A successful restore preserves the current library as a backup that can undo it.
    /// Use createInDatabase for SQLite storage or createInMemory for the name-only double.
    [<ReferenceEquality>]
    type LibraryBackupProxy =
        {
            /// Takes a backup of the whole library and answers its name.
            backUpLibrary : unit -> Result<LibraryBackupName, LibraryBackupError>
            /// Answers every backup, newest first.
            listBackups : unit -> Result<LibraryBackupName list, LibraryBackupError>
            /// Refuses an unlisted name; otherwise backs up the current library, replaces it with the named backup and answers the pre-restore backup's name.
            restoreLibrary : LibraryBackupName -> Result<LibraryBackupName, LibraryBackupError>
        }

        /// Holds minted names only; library content belongs to the database store (spec 0050 step 0085).
        static member createInMemory () : LibraryBackupProxy =
            let backups = ResizeArray<LibraryBackupName> ()

            let backUp () : Result<LibraryBackupName, LibraryBackupError> =
                let name = LibraryBackupName $"backup-{backups.Count + 1}"
                backups.Add name
                Ok name

            {
                backUpLibrary = backUp
                listBackups = fun () -> backups |> Seq.rev |> Seq.toList |> Ok
                restoreLibrary =
                    fun name ->
                        if backups.Contains name then backUp ()
                        else Error (UnknownLibraryBackupErr $"'{name.value}'")
            }
