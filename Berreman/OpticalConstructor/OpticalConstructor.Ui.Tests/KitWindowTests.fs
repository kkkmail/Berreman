namespace OpticalConstructor.Ui.Tests

open System.IO
open Avalonia.Controls
open Avalonia.Threading
open Xunit
open Softellect.Sys.Primitives
open OpticalConstructor.Ui.KitOpening
open OpticalConstructor.App

module KitWindowTests =

    let private owner : FolderName = FolderName "KitWindowTests"

    /// Shows the window headless, asserts its kit title, then closes it and checks its kit is released (spec 0050 S5.3).
    let private assertTitledByKit (window : #Window) (opened : OpenedKit) (constructorName : string) : unit =
        let lockFile = Path.Combine(opened.folder.value.value, ".oc", "lock")
        let expected = $"{opened.library.kit.kitFile.name.value} — {opened.folder.value.value} — Optical Constructor — {constructorName}"
        try
            window.Show()
            Dispatcher.UIThread.RunJobs()
            Assert.Equal(expected, window.Title)
        finally
            window.Close()
            Dispatcher.UIThread.RunJobs()
        Assert.False(File.Exists(lockFile), "closing the constructor window must release its kit")

    [<Fact>]
    let ``a constructor window is titled by its kit`` () : unit =
        HeadlessSession.run (fun () ->
            let ctx = TestAppScope.freshAppContext owner
            let mainKit = TestAppScope.openedKitIn owner ctx.catalogueImports
            assertTitledByKit (MainConstructorWindow(ctx, mainKit)) mainKit "Main"
            let inverseKit = TestAppScope.openedKitIn owner ctx.catalogueImports
            assertTitledByKit (InverseConstructorWindow(ctx, inverseKit)) inverseKit "Inverse")
