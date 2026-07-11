/// Spec 0038 Part C (step 007, SVC_XDUO_0001 — DECLARED): the window-opening seam.
/// Window policy lives behind ONE function record (the `*Proxy` convention),
/// generalizing the `EditorLaunchers` precedent (`TableAndElementRotationView.fs`):
/// Materials / Library / Solver-handoff / Category-editor windows are
/// single-instance (a second open ACTIVATES the live window), editor windows are
/// multi-instance keyed by the edited entity's id, and NEW entities mint their
/// Guid at Add-window open (carrying `EntryFreshness = NewUnsaved`) so the
/// registry keys uniformly by id from the first moment — the id-mint moves OFF
/// the save path. This step declares the surface, its mock, and the pinning
/// tests only; step 008 (IMPLEMENT_CONTRACT) builds the real launcher over a
/// host-layer `WindowRegistry` (mutable `Map<WindowKey, Window>` OUTSIDE any
/// Elmish model) with real `Activate` / unregister-on-`Closed` / `Show` vs
/// `ShowDialog` behaviour, and step 016 adds the Select-state re-target.
namespace OpticalConstructor.Ui

open Avalonia.Controls
open OpticalConstructor.Domain
open OpticalConstructor.Domain.WorkbenchSettings

module WindowLauncher =

    /// The identity of every window the launcher manages (§5). The four
    /// parameterless cases are the single-instance windows; the editor cases key
    /// by the edited entity's elevated id, so two edits of the SAME entity meet
    /// in one window while different entities each get their own. An Add-opened
    /// editor mints its `MaterialId.create ()` / `SampleId.create ()` AT WINDOW
    /// OPEN, so a brand-new entity is registry-addressable exactly like a
    /// persisted one. Structural equality/comparison (Guid-backed ids) is what
    /// lets the registry `Map` key on this DU.
    type WindowKey =
        | MaterialsWindowKey
        | LibraryWindowKey
        | SolverHandoffWindowKey
        | CategoryEditorKey
        | MaterialEditorKey of MaterialLibrary.MaterialId
        | SampleEditorKey of Library.SampleId

    /// Whether an editor's entity already lives in its store (spec §0.1: the
    /// operator's "is-new flag" is a two-case DU, never a bool). `NewUnsaved` =
    /// the id was minted at Add-window open and nothing is persisted yet;
    /// `Persisted` = the entity came from the store. Step 008 routes Save on
    /// this: `NewUnsaved` → `addMaterial` / `addSample`, `Persisted` →
    /// `updateMaterial` / `updateSample`.
    type EntryFreshness =
        | NewUnsaved
        | Persisted

    /// The typed failures of the seam (errors are values — never a throw across
    /// this boundary; each case carries the offending key and, where an inner
    /// failure exists, its reason).
    type WindowLauncherError =
        /// The injected per-key factory could not build the window for `key`.
        | WindowFactoryFailed of key : WindowKey * reason : string
        /// `forgetWindow` was asked to forget a key with no live window — a
        /// double-unregister or a close raced against an explicit forget.
        | WindowNotRegistered of key : WindowKey

    /// What an open request did, carrying the live window either way (the modal
    /// owner wiring and the tests' distinct-instance proofs both need it).
    type WindowLaunchOutcome =
        /// The key had no live window: the injected factory built one and the
        /// registry now holds it.
        | CreatedWindow of Window
        /// The key already had a live window: it was activated (brought to
        /// front by the real launcher), not re-created.
        | ActivatedWindow of Window
        /// The key's live window is in Select state and was re-pointed at a new
        /// selection target instead of being re-created. Declared now so match
        /// sites are compiler-complete; no launcher returns it before step 016.
        | RetargetedWindow of Window

    /// The window open/activate seam (SVC_XDUO_0001, `declared`): pre-curried,
    /// `Result`-returning functions that bake in the per-key window factory and
    /// the registry at construction — logic holds this record, never a live
    /// window table. Function-valued fields have no structural equality, so the
    /// record compares by reference (the `EditorLaunchers` precedent).
    [<ReferenceEquality>]
    type WindowLauncher =
        {
            /// Open-or-activate the window for `key`: no live window → build it
            /// through the injected per-key factory and register it
            /// (`CreatedWindow`); a live window → activate it
            /// (`ActivatedWindow`); a live Select-state window → re-target it
            /// (`RetargetedWindow`, step 016).
            openOrActivate : WindowKey -> Result<WindowLaunchOutcome, WindowLauncherError>
            /// Forget `key` on window close, so the next open creates afresh
            /// (the real launcher calls this from the window's `Closed`
            /// unregister hook). Forgetting a key with no live window is the
            /// typed `WindowNotRegistered` error.
            forgetWindow : WindowKey -> Result<unit, WindowLauncherError>
            /// Decide how a Select-state window opens, from the step-005
            /// `SelectWindowModality` switch baked in at construction:
            /// `ModalSelectWindows` → `ShowDialog` with the requesting window
            /// as owner, `ModelessSelectWindows` → `Show` (step 008). Browse
            /// windows never consult this — they always `Show`.
            decideSelectModality : unit -> Result<SelectWindowModality, WindowLauncherError>
        }

        /// The MOCK launcher (the contract's test double, the `createInMemory`
        /// ref-Map precedent): full open-or-activate/forget REGISTRY bookkeeping
        /// over a private `ref Map`, with window construction delegated to the
        /// caller's stub `factory` (which records the calls it receives) and the
        /// modality switch injected verbatim. NO real windowing behaviour — the
        /// mock never shows, activates, or hooks `Closed` on a window; that is
        /// step 008's `create`. Tests substitute recording stubs here and
        /// exercise every field through its exact signature.
        static member createMock
            (factory : WindowKey -> Result<Window, WindowLauncherError>)
            (modality : SelectWindowModality)
            : WindowLauncher =
            let registry : Map<WindowKey, Window> ref = ref Map.empty
            {
                openOrActivate =
                    fun (key : WindowKey) ->
                        match Map.tryFind key registry.Value with
                        | Some window -> ActivatedWindow window |> Ok
                        | None ->
                            match factory key with
                            | Ok window ->
                                registry.Value <- Map.add key window registry.Value
                                CreatedWindow window |> Ok
                            | Error e -> Error e
                forgetWindow =
                    fun (key : WindowKey) ->
                        match Map.tryFind key registry.Value with
                        | Some _ ->
                            registry.Value <- Map.remove key registry.Value
                            Ok ()
                        | None -> WindowNotRegistered key |> Error
                decideSelectModality = fun () -> Ok modality
            }
