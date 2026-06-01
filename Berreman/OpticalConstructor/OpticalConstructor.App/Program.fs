/// Minimal runnable entry point (§A.1). Later UI slices wire this to the
/// Avalonia application bootstrap; the scaffold returns 0 so the project is
/// runnable and links `OpticalConstructor.Ui`.
module OpticalConstructor.App.Program

[<EntryPoint>]
let main _argv = 0
