// Scratch (slice 026): inspect Avalonia 12.0.5 window-tracking internals so the
// composition-acceptance smoke test can decide how to observe windows opened by
// EditorLaunchers.defaults (unowned .Show()).
open System
open System.IO
open System.Reflection

let bin = @"C:\GitHub\Berreman\Berreman\OpticalConstructor\OpticalConstructor.Ui.Tests\bin\x64\Release\net10.0"

AppDomain.CurrentDomain.add_AssemblyResolve (ResolveEventHandler (fun _ args ->
    let name = AssemblyName(args.Name).Name
    let candidate = Path.Combine(bin, name + ".dll")
    if File.Exists candidate then Assembly.LoadFrom candidate else null))

let controls = Assembly.LoadFrom(Path.Combine(bin, "Avalonia.Controls.dll"))

let dump (typeName : string) =
    let t = controls.GetType(typeName, true)
    printfn "=== %s ===" typeName
    printfn "-- fields --"
    for f in t.GetFields(BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Instance ||| BindingFlags.Static) do
        printfn "  %s %s (static=%b)" f.FieldType.Name f.Name f.IsStatic
    printfn "-- methods --"
    for m in t.GetMethods(BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Instance ||| BindingFlags.Static ||| BindingFlags.DeclaredOnly) do
        printfn "  %s" (m.ToString())
    printfn "-- ctors --"
    for c in t.GetConstructors(BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Instance) do
        printfn "  %s" (c.ToString())

dump "Avalonia.Controls.ApplicationLifetimes.ClassicDesktopStyleApplicationLifetime"
dump "Avalonia.Controls.Window"
