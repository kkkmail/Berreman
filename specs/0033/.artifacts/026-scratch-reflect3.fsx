// Scratch (slice 026): exact signature of RoutedEvent.Raised and where WindowOpenedEvent is raised.
open System
open System.IO
open System.Reflection

let bin = @"C:\GitHub\Berreman\Berreman\OpticalConstructor\OpticalConstructor.Ui.Tests\bin\x64\Release\net10.0"

AppDomain.CurrentDomain.add_AssemblyResolve (ResolveEventHandler (fun _ args ->
    let name = AssemblyName(args.Name).Name
    let candidate = Path.Combine(bin, name + ".dll")
    if File.Exists candidate then Assembly.LoadFrom candidate else null))

let baseAsm = Assembly.LoadFrom(Path.Combine(bin, "Avalonia.Base.dll"))
let re = baseAsm.GetType("Avalonia.Interactivity.RoutedEvent", true)
let raised = re.GetProperty("Raised")
printfn "Raised : %s" raised.PropertyType.AssemblyQualifiedName
