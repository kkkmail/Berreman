namespace OpticalConstructor.Tests

open Softellect.Sys.Primitives
open Xunit
open OpticalConstructor.Domain.KitFolders

/// The seam that shows a folder in Explorer, and its in-memory double (spec 0050 S5.3).
module ExplorerProxyTests =

    [<Fact>]
    let ``the in-memory explorer hands every folder it is asked to show to its recorder`` () : unit =
        let shown = ResizeArray<FolderName> ()
        let explorer = ExplorerProxy.createInMemory shown.Add
        let kitFolder = FolderName "C:/kits/a"
        let dataFolder = FolderName "D:/data"

        Assert.Equal<Result<unit, ExplorerError>>(Ok (), explorer.showFolder kitFolder)
        Assert.Equal<Result<unit, ExplorerError>>(Ok (), explorer.showFolder dataFolder)
        Assert.Equal<FolderName list>([ kitFolder; dataFolder ], List.ofSeq shown)

    [<Fact>]
    let ``every explorer error renders its condition and its payload`` () : unit =
        let missingReason = "C:/kits/gone"
        let launchReason = "explorer.exe was not found"
        let missingText = (MissingFolderErr missingReason).describe
        let launchText = (ExplorerLaunchErr launchReason).describe

        Assert.Contains(missingReason, missingText)
        Assert.Contains(launchReason, launchText)
        Assert.Contains("does not exist", missingText)
        Assert.Contains("could not be shown", launchText)
        Assert.NotEqual<string>(missingText, launchText)
