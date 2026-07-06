// Scratch (slice 026): visibility check — can a test subscribe to Window.WindowOpenedEvent?
open System
open System.IO
open System.Reflection

let bin = @"C:\GitHub\Berreman\Berreman\OpticalConstructor\OpticalConstructor.Ui.Tests\bin\x64\Release\net10.0"

AppDomain.CurrentDomain.add_AssemblyResolve (ResolveEventHandler (fun _ args ->
    let name = AssemblyName(args.Name).Name
    let candidate = Path.Combine(bin, name + ".dll")
    if File.Exists candidate then Assembly.LoadFrom candidate else null))

let controls = Assembly.LoadFrom(Path.Combine(bin, "Avalonia.Controls.dll"))
let baseAsm = Assembly.LoadFrom(Path.Combine(bin, "Avalonia.Base.dll"))

let w = controls.GetType("Avalonia.Controls.Window", true)
for name in [ "WindowOpenedEvent"; "WindowClosedEvent" ] do
    let f = w.GetField(name, BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Static)
    printfn "%s : IsPublic=%b IsAssembly=%b type=%s" name f.IsPublic f.IsAssembly (f.FieldType.FullName)

// RoutedEvent.Raised — public observable?
let re = baseAsm.GetType("Avalonia.Interactivity.RoutedEvent", true)
for p in re.GetProperties(BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Instance) do
    printfn "RoutedEvent prop: %s %s (public getter=%b)" p.PropertyType.Name p.Name (p.GetGetMethod() <> null)

// Application.ApplicationLifetime setter public?
let appT = controls.GetType("Avalonia.Application", true)
let alt = appT.GetProperty("ApplicationLifetime", BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Instance)
printfn "Application.ApplicationLifetime: getter public=%b, setter=%A" (alt.GetGetMethod() <> null) (alt.GetSetMethod())
