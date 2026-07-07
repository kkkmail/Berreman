// Scratch (slice 026): does Window.WindowOpenedEvent fire for an unowned .Show()
// under the headless platform? Decides the composition-acceptance test mechanism.
#r @"C:\GitHub\Berreman\Berreman\OpticalConstructor\OpticalConstructor.Ui.Tests\bin\x64\Release\net10.0\Avalonia.Base.dll"
#r @"C:\GitHub\Berreman\Berreman\OpticalConstructor\OpticalConstructor.Ui.Tests\bin\x64\Release\net10.0\Avalonia.Controls.dll"
#r @"C:\GitHub\Berreman\Berreman\OpticalConstructor\OpticalConstructor.Ui.Tests\bin\x64\Release\net10.0\Avalonia.Headless.dll"
#r @"C:\GitHub\Berreman\Berreman\OpticalConstructor\OpticalConstructor.Ui.Tests\bin\x64\Release\net10.0\Avalonia.Skia.dll"

open System
open System.IO
open System.Reflection
open Avalonia
open Avalonia.Controls
open Avalonia.Headless
open Avalonia.Threading

let bin = @"C:\GitHub\Berreman\Berreman\OpticalConstructor\OpticalConstructor.Ui.Tests\bin\x64\Release\net10.0"
AppDomain.CurrentDomain.add_AssemblyResolve (ResolveEventHandler (fun _ args ->
    let name = AssemblyName(args.Name).Name
    let candidate = Path.Combine(bin, name + ".dll")
    if File.Exists candidate then Assembly.LoadFrom candidate else null))

AppBuilder
    .Configure<Application>()
    .UseHeadless(AvaloniaHeadlessPlatformOptions())
    .SetupWithoutStarting()
|> ignore

let opened = ResizeArray<string>()
let sub =
    Window.WindowOpenedEvent.Raised
    |> Observable.subscribe (fun (struct (sender, _args)) ->
        match sender with
        | :? Window as w -> opened.Add (sprintf "opened: %s" w.Title)
        | other -> opened.Add (sprintf "opened (non-window): %A" other))

let w = Window(Title = "probe")
w.Show()
Dispatcher.UIThread.RunJobs()
printfn "events after Show: %d" opened.Count
opened |> Seq.iter (printfn "  %s")
w.Close()
Dispatcher.UIThread.RunJobs()
sub.Dispose()
printfn "IsVisible after close: %b" w.IsVisible
