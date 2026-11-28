namespace BerremanTestSupport

open System
open System.IO
open Softellect.Sys.Primitives
open OpticalConstructor.Domain.Kit

/// The read-only reference kits under the repository's `data/` folder, shared by both test suites: AT-1's optics-mueller,
/// AT-2's absorbing-biaxial-noisy, AT-3's multilayer-thin-film and AT-4's langasite-on-silicon (spec 0050 §3.21, S9.4,
/// S10.2, S12.2; spec 0051 `.manual/007` 9.1).
type CommittedKit =
    | OpticsMuellerKit
    | AbsorbingBiaxialNoisyKit
    | MultilayerThinFilmKit
    | LangasiteOnSiliconKit

    /// The kit's folder under the repository root, the first folder upward of the running assembly that holds
    /// `Berreman/Berreman.slnx`; None without that root or kit folder. Writes nothing (spec 0050 S12.2).
    member this.tryFindFolder : KitFolder option =
        let kitFolderName =
            match this with
            | OpticsMuellerKit -> "optics-mueller"
            | AbsorbingBiaxialNoisyKit -> "absorbing-biaxial-noisy"
            | MultilayerThinFilmKit -> "multilayer-thin-film"
            | LangasiteOnSiliconKit -> "langasite-on-silicon"

        let rec tryFindRoot (folder : DirectoryInfo) : DirectoryInfo option =
            if File.Exists(Path.Combine(folder.FullName, "Berreman", "Berreman.slnx")) then Some folder
            else folder.Parent |> Option.ofObj |> Option.bind tryFindRoot

        tryFindRoot (DirectoryInfo AppContext.BaseDirectory)
        |> Option.map (fun root -> Path.Combine(root.FullName, "data", kitFolderName))
        |> Option.filter Directory.Exists
        |> Option.map (fun kitFolder -> KitFolder (FolderName kitFolder))
