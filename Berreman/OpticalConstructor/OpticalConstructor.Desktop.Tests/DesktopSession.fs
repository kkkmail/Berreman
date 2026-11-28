namespace OpticalConstructor.Desktop.Tests

open System
open System.Diagnostics
open System.IO
open System.Runtime.InteropServices
open System.Security.Cryptography
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

        [<DllImport("user32.dll", SetLastError = true)>]
        extern bool OpenClipboard(IntPtr owner)

        [<DllImport("user32.dll")>]
        extern bool CloseClipboard()

        [<DllImport("user32.dll")>]
        extern IntPtr GetClipboardData(uint32 format)

        [<DllImport("kernel32.dll")>]
        extern IntPtr GlobalLock(IntPtr memory)

        [<DllImport("kernel32.dll")>]
        extern bool GlobalUnlock(IntPtr memory)

        [<Literal>]
        let UnicodeText = 13u

        [<Struct; StructLayout(LayoutKind.Sequential)>]
        type Rect =
            val mutable left : int
            val mutable top : int
            val mutable right : int
            val mutable bottom : int

        [<Struct; StructLayout(LayoutKind.Sequential)>]
        type MonitorInfo =
            val mutable size : uint32
            val mutable monitor : Rect
            val mutable work : Rect
            val mutable flags : uint32

        [<DllImport("user32.dll")>]
        extern IntPtr MonitorFromWindow(IntPtr window, uint32 flags)

        [<DllImport("user32.dll")>]
        extern bool GetMonitorInfo(IntPtr monitor, MonitorInfo& info)

        [<Literal>]
        let NearestMonitor = 2u

    /// The screen's pixels, read through GDI: what only the real renderer draws (spec 0051 `.manual/003` Phase 6).
    module Screen =
        [<Struct; StructLayout(LayoutKind.Sequential)>]
        type BitmapInfoHeader =
            val mutable size : uint32
            val mutable width : int
            val mutable height : int
            val mutable planes : uint16
            val mutable bitCount : uint16
            val mutable compression : uint32
            val mutable sizeImage : uint32
            val mutable xPelsPerMeter : int
            val mutable yPelsPerMeter : int
            val mutable colorsUsed : uint32
            val mutable colorsImportant : uint32

        [<DllImport("user32.dll")>]
        extern bool SetProcessDPIAware()

        [<DllImport("user32.dll")>]
        extern IntPtr GetDC(IntPtr window)

        [<DllImport("user32.dll")>]
        extern int ReleaseDC(IntPtr window, IntPtr context)

        [<DllImport("gdi32.dll")>]
        extern IntPtr CreateCompatibleDC(IntPtr context)

        [<DllImport("gdi32.dll")>]
        extern IntPtr CreateCompatibleBitmap(IntPtr context, int width, int height)

        [<DllImport("gdi32.dll")>]
        extern IntPtr SelectObject(IntPtr context, IntPtr gdiObject)

        [<DllImport("gdi32.dll")>]
        extern bool BitBlt(IntPtr target, int x, int y, int width, int height, IntPtr source, int sourceX, int sourceY, uint32 operation)

        [<DllImport("gdi32.dll")>]
        extern int GetDIBits(IntPtr context, IntPtr bitmap, uint32 start, uint32 lines, int[] bits, BitmapInfoHeader& info, uint32 usage)

        [<DllImport("gdi32.dll")>]
        extern bool DeleteObject(IntPtr gdiObject)

        [<DllImport("gdi32.dll")>]
        extern bool DeleteDC(IntPtr context)

        /// The pixels of `bounds` on screen, top row first, each 0x00RRGGBB.
        let capture (bounds : Drawing.Rectangle) : int[] =
            SetProcessDPIAware() |> ignore
            let screen = GetDC IntPtr.Zero
            let memory = CreateCompatibleDC screen
            let bitmap = CreateCompatibleBitmap(screen, bounds.Width, bounds.Height)
            try
                SelectObject(memory, bitmap) |> ignore
                BitBlt(memory, 0, 0, bounds.Width, bounds.Height, screen, bounds.Left, bounds.Top, 0x00CC0020u) |> ignore
                let mutable header = BitmapInfoHeader()
                header.size <- uint32 (Marshal.SizeOf<BitmapInfoHeader>())
                header.width <- bounds.Width
                header.height <- -bounds.Height
                header.planes <- 1us
                header.bitCount <- 32us
                let pixels = Array.zeroCreate<int> (bounds.Width * bounds.Height)
                GetDIBits(memory, bitmap, 0u, uint32 bounds.Height, pixels, &header, 0u) |> ignore
                pixels
            finally
                DeleteObject bitmap |> ignore
                DeleteDC memory |> ignore
                ReleaseDC(IntPtr.Zero, screen) |> ignore

    /// The text on the clipboard, read through the Win32 clipboard as any other program reads it; the app may hold the
    /// clipboard for a moment after writing it.
    let clipboardText () : string =
        let deadline = DateTime.UtcNow.AddSeconds 5.0
        let rec read () : string =
            if Native.OpenClipboard IntPtr.Zero then
                try
                    match Native.GetClipboardData Native.UnicodeText with
                    | handle when handle = IntPtr.Zero -> ""
                    | handle ->
                        let text = Native.GlobalLock handle
                        try Marshal.PtrToStringUni text finally Native.GlobalUnlock handle |> ignore
                finally Native.CloseClipboard() |> ignore
            elif DateTime.UtcNow < deadline then
                Thread.Sleep 100
                read ()
            else failwith "the clipboard stayed held by another program"
        read ()

    /// The working area of the monitor showing `window`, in screen pixels: its width and height.
    let workAreaOf (window : AutomationElement) : Drawing.Size =
        let monitor = Native.MonitorFromWindow(window.Properties.NativeWindowHandle.Value, Native.NearestMonitor)
        let mutable info = Native.MonitorInfo()
        info.size <- uint32 (Marshal.SizeOf<Native.MonitorInfo>())
        Native.GetMonitorInfo(monitor, &info) |> ignore
        Drawing.Size(info.work.right - info.work.left, info.work.bottom - info.work.top)

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

    /// Copies the committed kit `data/<name>` into `parent`, `.oc/` aside, and answers its folder.
    let addCommittedKitUnder (parent : string) (name : string) : string =
        let target = Path.Combine(parent, name)
        copyFolder (Path.Combine(repositoryRoot, "data", name)) target (fun path -> path.Contains(Path.DirectorySeparatorChar.ToString() + ".oc"))
        target

    /// Copies the committed kit `data/<name>` into the sandbox's kits folder.
    let addCommittedKit (sandbox : DesktopSandbox) (name : string) : string = addCommittedKitUnder sandbox.kits name

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

        /// The app's window whose title contains `title`: a top-level one, or one a top-level window owns, which UI
        /// Automation lists as that window's child.
        member this.window (title : string) : Window =
            waitFor $"the window '{title}'" 30.0 (fun () ->
                this.windows
                |> List.tryPick (fun window ->
                    if window.Title.Contains title then Some window
                    else
                        window.FindAllChildren(condition (fun factory -> factory.ByControlType(ControlType.Window)))
                        |> Array.map (fun owned -> owned.AsWindow())
                        |> Array.tryFind (fun owned -> owned.Title.Contains title)))

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

    /// Opens `kitFolder` from the Launcher's Main or Inverse start screen by typing its `.optics-kit` file into the real
    /// file dialog (spec 0051 `.manual/007` 3.3), applies any import dialog on the way, and answers the kit window with
    /// the dialogs it applied.
    let openKitSeeing (app : DesktopApp) (openButton : ConstructorUiId) (kitFolder : string) : Window * KitOpenQuestion list =
        let launcher = app.window "Launcher"
        app.invoke launcher openButton
        let start = app.window "Open a kit"
        app.invoke start ConstructorUiId.openKitButton
        app.typeIntoNativeDialog (Path.Combine(kitFolder, ".optics-kit"))
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

    /// Every file of a kit outside `.oc/`, by its path in the kit, with its SHA-256.
    let kitContent (folder : string) : Map<string, string> =
        Directory.GetFiles(folder, "*", SearchOption.AllDirectories)
        |> Array.filter (fun file -> not (Path.GetRelativePath(folder, file).StartsWith ".oc"))
        |> Array.map (fun file -> Path.GetRelativePath(folder, file), Convert.ToHexString(SHA256.HashData(File.ReadAllBytes file)))
        |> Map.ofArray

    /// What changed between two `kitContent` reads, for a failure message.
    let changedFiles (before : Map<string, string>) (after : Map<string, string>) : string list =
        Set.union (before.Keys |> Set.ofSeq) (after.Keys |> Set.ofSeq)
        |> Set.filter (fun path -> Map.tryFind path before <> Map.tryFind path after)
        |> Set.toList

    /// The texts UI Automation shows under `root`.
    let textsUnder (root : AutomationElement) : string list =
        root.FindAllDescendants(condition (fun factory -> factory.ByControlType(ControlType.Text)))
        |> Array.map (fun (text : AutomationElement) -> text.Name)
        |> Array.filter (String.IsNullOrEmpty >> not)
        |> List.ofArray

    /// The text a control shows to UI Automation: its value, else its name.
    let shownText (element : AutomationElement) : string =
        if element.Patterns.Value.IsSupported then element.Patterns.Value.Pattern.Value.Value else element.Name

    /// A folder path without its trailing separator.
    let trimmed (folder : string) : string = folder.TrimEnd([| '\\'; '/' |])
