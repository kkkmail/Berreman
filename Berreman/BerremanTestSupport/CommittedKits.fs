namespace BerremanTestSupport

open System
open System.IO
open Softellect.Sys.Primitives
open OpticalConstructor.Domain.Kit

/// The committed reference kits under the repository's `data/` folder, found from the folder holding
/// `Berreman/Berreman.slnx` and read read-only (spec 0050 §3.21, S9.4); both test suites share them through
/// `BerremanTestSupport` (S10.2).
type CommittedKit =
    | OpticsMuellerKit

    /// The kit's folder under the repository root, the first folder upward of the running assembly that holds
    /// `Berreman/Berreman.slnx`; None without that root or without the kit folder. Writes nothing.
    member this.tryFindFolder : KitFolder option =
        let kitFolderName =
            match this with
            | OpticsMuellerKit -> "optics-mueller"

        let rec tryFindRoot (folder : DirectoryInfo) : DirectoryInfo option =
            if File.Exists(Path.Combine(folder.FullName, "Berreman", "Berreman.slnx")) then Some folder
            else folder.Parent |> Option.ofObj |> Option.bind tryFindRoot

        tryFindRoot (DirectoryInfo AppContext.BaseDirectory)
        |> Option.map (fun root -> Path.Combine(root.FullName, "data", kitFolderName))
        |> Option.filter Directory.Exists
        |> Option.map (fun kitFolder -> KitFolder (FolderName kitFolder))
