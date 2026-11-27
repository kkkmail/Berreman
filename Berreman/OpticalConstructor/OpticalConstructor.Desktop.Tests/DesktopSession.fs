namespace OpticalConstructor.Desktop.Tests

open System
open System.Diagnostics
open System.IO
open System.Runtime.InteropServices
open System.Threading
open FlaUI.Core
open FlaUI.Core.AutomationElements
open FlaUI.Core.Conditions
open FlaUI.Core.Definitions
open FlaUI.UIA3
open Xunit

open OpticalConstructor.Controls

/// One run of the real Optical Constructor on the real desktop, driven through Windows UI Automation (spec 0051
/// `.manual/003` 1.6). Every location is under the test's output directory; the committed kits are copied, never opened.
module DesktopSession =

    /// The one xUnit collection of every desktop fact: they share the desktop, its focus and the app copy's
    /// `appsettings.json`, so no two may run at once.
    [<Literal>]
    let RealDesktop = "Real desktop"

    module private Native =
        [<DllImport("user32.dll", CharSet = CharSet.Unicode)>]
        extern IntPtr SendMessage(IntPtr window, uint32 message, IntPtr wParam, string lParam)

        [<DllImport("user32.dll")>]
        extern IntPtr SendMessageW(IntPtr window, uint32 message, IntPtr wParam, IntPtr lParam)

        [<Literal>]
        let SetText = 0x000Cu

        [<Literal>]
        let ButtonClick = 0x00F5u

    /// A FlaUI search condition from an F# function.
    let condition (build : ConditionFactory -> ConditionBase) : Func<ConditionFactory, ConditionBase> = Func<ConditionFactory, ConditionBase> build

    /// Skips the calling fact where no interactive desktop is present: Windows session 0, where the arc's gates run.
    let requireDesktop () : unit =
        if Process.GetCurrentProcess().SessionId = 0 || not Environment.UserInteractive then
            Assert.Skip "No interactive desktop: the desktop suite runs in a user's session (spec 0051 .manual/003 1.6)."

    /// The committed optics-mueller kit, located by the shared test support; the repository root is two folders up.
    let private committedOpticsMueller : string =
        match BerremanTestSupport.OpticsMuellerKit.tryFindFolder with
        | Some folder -> folder.value.value
        | None -> failwith "the committed optics-mueller kit was not found under the repository's data folder"

    let repositoryRoot : string = Path.GetFullPath(Path.Combine(committedOpticsMueller, "..", ".."))

    let private builtApp : string =
        Path.Combine(repositoryRoot, "Berreman", "OpticalConstructor", "OpticalConstructor.App", "bin", "x64", "Release", "net10.0")

    let private copyFolder (source : string) (target : string) (skip : string -> bool) : unit =
        for directory in Directory.GetDirectories(source, "*", SearchOption.AllDirectories) do
            if not (skip directory) then
                Directory.CreateDirectory(directory.Replace(source, target)) |> ignore
        Directory.CreateDirectory target |> ignore
        for file in Directory.GetFiles(source, "*", SearchOption.AllDirectories) do
            if not (skip file) then
                File.Copy(file, file.Replace(source, target), true)

    /// The app's copy for this test process, refreshed from the build once, so its `appsettings.json` can point at a
    /// sandbox without touching the build's own.
    let private appCopy : Lazy<string> =
        lazy (
            let target = Path.Combine(AppContext.BaseDirectory, "DesktopRun", "app")
            copyFolder builtApp target (fun path -> path.EndsWith "appsettings.json")
            target)

    /// A sandbox of one run: its data folder, its kits folder (holding a copy of `data/optics-mueller`) and the app.
    type DesktopSandbox =
        {
            root : string
            kits : string
            opticsMueller : string
            app : string
        }

    let createSandbox (name : string) : DesktopSandbox =
        let root = Path.Combine(AppContext.BaseDirectory, "DesktopRun", $"{name}-{DateTime.UtcNow:yyyyMMddHHmmss}")
        let data = Path.Combine(root, "data")
        let kits = Path.Combine(root, "kits")
        let opticsMueller = Path.Combine(kits, "optics-mueller")
        copyFolder committedOpticsMueller opticsMueller (fun path -> path.Contains(Path.DirectorySeparatorChar.ToString() + ".oc"))
        let app = appCopy.Force()
        let forward (path : string) : string = path.Replace('\\', '/')
        let settings =
            $$"""{
  "connectionStrings": { "OpticalConstructorSqlite": "Data Source={{forward data}}/library.db" },
  "appSettings": {
    "LibraryBackupsFolder": "{{forward data}}/Backups",
    "PreferencesFolder": "{{forward data}}/Preferences",
    "KitsFolder": "{{forward kits}}",
    "ExportsFolder": "{{forward data}}/Exports",
    "CataloguesFolder": "{{forward data}}/Catalogues",
    "MaterialSourcesFolder": "{{forward data}}/Sources"
  }
}
"""
        Directory.CreateDirectory data |> ignore
        // The previous fact's app, or a scanner reading the fresh copy, can hold the file for a moment.
        let deadline = DateTime.UtcNow.AddSeconds 15.0
        let rec write () : unit =
            try File.WriteAllText(Path.Combine(app, "appsettings.json"), settings)
            with :? IOException when DateTime.UtcNow < deadline ->
                Thread.Sleep 200
                write ()
        write ()
        { root = root; kits = kits; opticsMueller = opticsMueller; app = app }

    /// Polls `probe` until it answers, or fails after `seconds`; never a fixed sleep (AGENTS.md).
    let waitFor (what : string) (seconds : float) (probe : unit -> 'Found option) : 'Found =
        let deadline = DateTime.UtcNow.AddSeconds seconds
        let rec poll () =
            match probe () with
            | Some found -> found
            | None when DateTime.UtcNow < deadline ->
                Thread.Sleep 150
                poll ()
            | None -> failwith $"timed out waiting for {what}"
        poll ()

    /// The running app and its automation.
    type DesktopApp =
        {
            application : Application
            automation : UIA3Automation
        }

        member this.windows : Window list = this.application.GetAllTopLevelWindows(this.automation) |> List.ofArray

        /// The app's top-level window whose title contains `title`.
        member this.window (title : string) : Window =
            waitFor $"the window '{title}'" 30.0 (fun () -> this.windows |> List.tryFind (fun window -> window.Title.Contains title))

        /// The element answering to `id` under `root`, waited for.
        member this.find (root : AutomationElement) (id : ConstructorUiId) : AutomationElement =
            waitFor $"the control '{id.value}'" 15.0 (fun () -> root.FindFirstDescendant(condition (fun factory -> factory.ByAutomationId(id.value))) |> Option.ofObj)

        member this.tryFind (root : AutomationElement) (id : ConstructorUiId) : AutomationElement option =
            root.FindFirstDescendant(condition (fun factory -> factory.ByAutomationId(id.value))) |> Option.ofObj

        /// Drives the control answering to `id` through its pattern, as a user's assistive tool would: Invoke for a
        /// button, Toggle for a choice.
        member this.invoke (root : AutomationElement) (id : ConstructorUiId) : unit =
            let element = this.find root id
            if element.Patterns.Invoke.IsSupported then element.Patterns.Invoke.Pattern.Invoke()
            elif element.Patterns.Toggle.IsSupported then element.Patterns.Toggle.Pattern.Toggle()
            else failwith $"{id.value} offers neither Invoke nor Toggle ({element.ControlType})"

        /// The native file or folder dialog the app opened, owned by one of its windows.
        member this.nativeDialog : AutomationElement =
            waitFor "a native file dialog" 20.0 (fun () ->
                let pid = this.application.ProcessId
                this.automation.GetDesktop().FindAllDescendants(condition (fun factory -> factory.ByClassName("#32770").And(factory.ByProcessId(pid))))
                |> Array.tryHead)

        /// Types `path` into the native dialog's name box and presses its default button; answers the dialog's title.
        member this.answerNativeDialog (path : string) : string =
            let dialog = this.nativeDialog
            let title = dialog.Name
            let nameBox =
                dialog.FindAllDescendants(condition (fun factory -> factory.ByClassName("Edit")))
                |> Array.tryFind (fun (edit : AutomationElement) -> Array.contains edit.AutomationId [| "1152"; "1001"; "1148" |])
                |> Option.defaultWith (fun () -> failwith "the dialog has no name box")
            Native.SendMessage(nameBox.Properties.NativeWindowHandle.Value, Native.SetText, IntPtr.Zero, path) |> ignore
            let accept =
                dialog.FindAllDescendants(condition (fun factory -> factory.ByAutomationId("1").And(factory.ByClassName("Button"))))
                |> Array.head
            Native.SendMessageW(accept.Properties.NativeWindowHandle.Value, Native.ButtonClick, IntPtr.Zero, IntPtr.Zero) |> ignore
            title

        /// Types `path` into the native file dialog's name box and presses Enter. A file dialog reads its name box
        /// through the combo box's edit-change notice, which only typing raises, never WM_SETTEXT.
        member this.typeIntoNativeDialog (path : string) : unit =
            let dialog = this.nativeDialog
            let nameBox =
                waitFor "the dialog's name box" 10.0 (fun () ->
                    dialog.FindAllDescendants(condition (fun factory -> factory.ByControlType(ControlType.Edit)))
                    |> Array.tryFind (fun (edit : AutomationElement) -> Array.contains edit.AutomationId [| "1001"; "1148"; "1152" |]))
            dialog.SetForeground()
            nameBox.Focus()
            FlaUI.Core.Input.Keyboard.TypeSimultaneously(FlaUI.Core.WindowsAPI.VirtualKeyShort.CONTROL, FlaUI.Core.WindowsAPI.VirtualKeyShort.KEY_A)
            FlaUI.Core.Input.Keyboard.Type path
            FlaUI.Core.Input.Keyboard.Type(FlaUI.Core.WindowsAPI.VirtualKeyShort.ENTER)

        /// The folder the native dialog shows, from its address bar.
        member this.nativeDialogFolder : string =
            let dialog = this.nativeDialog
            dialog.FindAllDescendants()
            |> Array.tryPick (fun (element : AutomationElement) ->
                match element.Name with
                | null -> None
                | name when name.StartsWith "Address: " -> Some (name.Substring "Address: ".Length)
                | _ -> None)
            |> Option.defaultValue ""

        /// Cancels the native dialog.
        member this.cancelNativeDialog () : unit =
            let dialog = this.nativeDialog
            let cancel = dialog.FindAllDescendants(condition (fun factory -> factory.ByAutomationId("2").And(factory.ByClassName("Button")))) |> Array.head
            Native.SendMessageW(cancel.Properties.NativeWindowHandle.Value, Native.ButtonClick, IntPtr.Zero, IntPtr.Zero) |> ignore

        interface IDisposable with
            member this.Dispose() =
                try this.application.Close() |> ignore with _ -> ()
                try this.application.Kill() with _ -> ()
                // The next fact rewrites the app copy's settings, so this one's process must be gone first.
                try
                    use exiting = Process.GetProcessById this.application.ProcessId
                    exiting.WaitForExit 15000 |> ignore
                with :? ArgumentException -> ()
                this.automation.Dispose()

    /// Starts the app over `sandbox` and applies the first start's import decisions through their dialog.
    let launch (sandbox : DesktopSandbox) : DesktopApp =
        let start = ProcessStartInfo(Path.Combine(sandbox.app, "Softellect.Berreman.OpticalConstructor.App.exe"), WorkingDirectory = sandbox.app)
        let app = { application = Application.Launch start; automation = new UIA3Automation() }
        let launcher = app.window "Launcher"
        // A first start may hold catalogue rows for a decision; their dialog opens over the Launcher once it shows.
        let deadline = DateTime.UtcNow.AddSeconds 5.0
        let rec applyPending () : unit =
            // The dialog is a window of its own over the Launcher.
            match app.windows |> List.tryPick (fun window -> app.tryFind window ConstructorUiId.importApplyButton |> Option.map (fun _ -> window)) with
            | Some dialog -> app.invoke dialog ConstructorUiId.importApplyButton
            | None when DateTime.UtcNow < deadline ->
                Thread.Sleep 150
                applyPending ()
            | None -> ()
        applyPending ()
        app

    /// An import dialog a kit's opening answered: the rows it asked, and its count of the rows of one choice.
    type KitOpenQuestion =
        {
            askedRows : int
            unaskedSummaryOpt : string option
        }

    /// Opens `kitFolder` from the Launcher's Main or Inverse start screen through the real folder dialog, applies any
    /// import dialog on the way, and answers the kit window with the dialogs it applied.
    let openKitSeeing (app : DesktopApp) (openButton : ConstructorUiId) (kitFolder : string) : Window * KitOpenQuestion list =
        let launcher = app.window "Launcher"
        app.invoke launcher openButton
        let start = app.window "Open a kit"
        app.invoke start ConstructorUiId.openKitButton
        app.answerNativeDialog kitFolder |> ignore
        // Opening a kit may ask the snapshot decisions of its library first.
        let deadline = DateTime.UtcNow.AddSeconds 10.0
        let rec settle (seen : KitOpenQuestion list) : Window * KitOpenQuestion list =
            match app.windows |> List.tryFind (fun window -> window.Title.Contains "Optical Constructor —" && window.Title.Contains (Path.GetFileName kitFolder)) with
            | Some kitWindow -> kitWindow, List.rev seen
            | None ->
                match app.windows |> List.tryPick (fun window -> app.tryFind window ConstructorUiId.importApplyButton |> Option.map (fun _ -> window)) with
                | Some owner ->
                    let question =
                        {
                            askedRows =
                                owner.FindAllDescendants()
                                |> Array.filter (fun (element : AutomationElement) -> (element.Properties.AutomationId.ValueOrDefault |> Option.ofObj |> Option.defaultValue "").StartsWith "ImportDecisionRow_")
                                |> Array.length
                            unaskedSummaryOpt =
                                app.tryFind owner ConstructorUiId.importUnaskedSummary
                                |> Option.map (fun (summary : AutomationElement) -> summary.Name)
                        }
                    app.invoke owner ConstructorUiId.importApplyButton
                    settle (question :: seen)
                | None when DateTime.UtcNow < deadline ->
                    Threading.Thread.Sleep 150
                    settle seen
                | None -> failwith $"the kit window over {kitFolder} did not open"
        settle []

    /// Opens `kitFolder` as `openKitSeeing` does, and answers the kit window.
    let openKit (app : DesktopApp) (openButton : ConstructorUiId) (kitFolder : string) : Window =
        fst (openKitSeeing app openButton kitFolder)

    /// The text a control shows to UI Automation: its value, else its name.
    let shownText (element : AutomationElement) : string =
        if element.Patterns.Value.IsSupported then element.Patterns.Value.Pattern.Value.Value else element.Name

    /// A folder path without its trailing separator.
    let trimmed (folder : string) : string = folder.TrimEnd([| '\\'; '/' |])
