/// Spec 0038 Part B.1 (step 002): the surviving theme seam. The Elmish shell was
/// retired; `AppShell.themeVariant` remains the ONLY place the persisted theme label
/// crosses into Avalonia's styling system (`App.Initialize` applies it at startup —
/// pinned end-to-end by `SmokeTests`). These are pure mapping tests (gate `ui-tests`);
/// they live here rather than in the Avalonia-free `OpticalConstructor.Tests` because
/// the mapped `ThemeVariant` is an Avalonia type.
namespace OpticalConstructor.Ui.Tests

open Avalonia.Styling
open Xunit
open OpticalConstructor.Ui
open OpticalConstructor.Ui.UserEnvironment

module AppShellSeamTests =

    [<Fact>]
    let ``the Light theme maps to Avalonia's Light variant`` () =
        Assert.Equal(ThemeVariant.Light, AppShell.themeVariant Light)

    [<Fact>]
    let ``the Dark theme maps to Avalonia's Dark variant`` () =
        Assert.Equal(ThemeVariant.Dark, AppShell.themeVariant Dark)

    [<Fact>]
    let ``the two persisted themes map to distinct variants`` () =
        // A theme edit in environment.json must actually change the applied variant.
        Assert.NotEqual<ThemeVariant>(AppShell.themeVariant Light, AppShell.themeVariant Dark)
