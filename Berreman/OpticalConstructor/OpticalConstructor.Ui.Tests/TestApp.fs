/// Headless Avalonia test application + session driver (spec 0024 §U1.7). The
/// `AvaloniaTestApplication` assembly attribute points `Avalonia.Headless` at the
/// REAL composition-root `OpticalConstructor.App.App`, so the shared headless session
/// builds and `Initialize()`s the SAME `App` type the desktop entry point runs. That
/// means `App.Initialize`'s theme seam (`AppShell.themeVariant`, Program.fs:52-54)
/// actually executes inside the session — the smoke test drives the real lifetime
/// rather than a stand-in app that bypasses framework-init (AC-U1.3 / §U1.7). The real
/// `App.Initialize` already loads the Fluent theme, so no separate test `Application`
/// is needed.
///
/// This project (and ONLY this project) carries the headless Avalonia dependency; the
/// pure `OpticalConstructor.Tests` suite stays Avalonia-free so `constructor-unit-tests`
/// keeps its Avalonia-free guarantee (§U1.7).
///
/// `Avalonia.Headless.XUnit` 11.3.4 binds to xUnit **v2** only (`xunit.core` 2.4.0),
/// so its `[<AvaloniaFact>]` is invisible to the xunit.v3 runner. To keep this project
/// on xunit.v3 — matching the sibling `OpticalConstructor.Tests` framework, as §U1.7
/// requires — the tests use plain `[<Fact>]` and marshal their UI work onto the shared
/// headless UI thread through the public `HeadlessUnitTestSession` API directly (this
/// is exactly what `[<AvaloniaFact>]` does internally).
namespace OpticalConstructor.Ui.Tests

open System
open System.Threading
open Avalonia
open Avalonia.Headless
open OpticalConstructor.App

/// Builds the headless app for the shared session against the real `App` so its
/// `Initialize` (and therefore the theme seam) runs under the headless platform.
type TestAppBuilder() =
    static member BuildAvaloniaApp () : AppBuilder =
        AppBuilder
            .Configure<App>()
            .UseHeadless(AvaloniaHeadlessPlatformOptions())

[<assembly: AvaloniaTestApplication(typeof<TestAppBuilder>)>]
do ()

/// Runs UI-thread actions on the shared headless session, discovered from the
/// `AvaloniaTestApplication` attribute above.
module HeadlessSession =
    let private session =
        lazy HeadlessUnitTestSession.GetOrStartForAssembly(typeof<TestAppBuilder>.Assembly)

    /// Run `action` on the headless UI thread, blocking until it completes and
    /// rethrowing any exception or assertion failure it raised.
    let run (action : unit -> unit) : unit =
        session.Value.Dispatch(Action(action), CancellationToken.None).GetAwaiter().GetResult()
