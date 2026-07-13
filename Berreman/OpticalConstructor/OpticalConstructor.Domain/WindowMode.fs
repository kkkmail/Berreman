namespace OpticalConstructor.Domain

open OpticalConstructor.Domain.Placement
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.SampleStackEditor

/// Spec 0038 Part G (step 016) — the Browse/Select window-mode domain: *Selector = a catalogue
/// window in a Select state* — the SAME window in code, never a copy. ONE mode DU serves BOTH
/// windows (the Materials window over `MaterialEntry`, the Library window over `LibraryEntry`)
/// through the generic `'entry`; the Select case carries a `SelectionContext` — the pre-applied,
/// NON-REMOVABLE kind constraint, the typed selection target, and the `onSelected` /
/// `onCancelled` callbacks the requesting site bakes in. Pure data + callbacks: the windows
/// consume it, the step-017 Selector flow and the step-019 sample-editor picking produce it.
module WindowMode =

    /// The Select-state corpus constraint: the ONE `CatalogueKind` the requesting site needs an
    /// entry for (spec G.0 — the Library window narrows its corpus through
    /// `LibraryEntry.forKinds`; the Materials window's material corpus satisfies a layer pick
    /// structurally). A single-case DU per §0.1; the window pre-applies it as NON-REMOVABLE —
    /// no breadcrumb chip, no remove affordance.
    type KindConstraint =
        | KindConstraint of CatalogueKind

        member this.value = let (KindConstraint kind) = this in kind

    /// WHAT the selection is for — the typed target the requesting surface routes the returned
    /// entry to: a table element (the workbench Selector flow, step 017) by its serializable
    /// `ElementId`, one sample-layer slot (the sample-editor material pick, step 019) by its
    /// `LayerPosition`, or a sample's single substrate plate (the sample-editor substrate pick,
    /// spec 0040 Part D.4 step 011). The receiving surface resolves the target at RETURN time — a
    /// vanished target is a no-op plus a status line, never a throw. The substrate slot has no
    /// positional identity (a sample holds ONE substrate plate), so its case carries no payload:
    /// the return routes through the requesting editor's captured dispatch, and a closed editor
    /// makes it a no-op.
    type SelectionTarget =
        | TableElementTarget of ElementId
        | SampleLayerTarget of LayerPosition
        | SampleSubstrateTarget

    /// One Select session's context (spec G.0): the fixed kind constraint, the typed target,
    /// and the two outcome callbacks — `onSelected` dispatches a TARGETED message carrying the
    /// chosen entry back to the requesting surface; `onCancelled` ends the session without a
    /// choice (the Close verb, the title-bar X, a staleness cancel, or a re-target superseding
    /// it). Exactly one of the two fires per session — the window guarantees it. The spec names
    /// the first field `constraint`; that is an F# reserved word, so it is `kindConstraint`
    /// here. Function-valued fields have no structural equality, so the context compares by
    /// reference (a model holding the mode keeps its Elmish-required equality).
    [<ReferenceEquality>]
    type SelectionContext<'entry> =
        {
            kindConstraint : KindConstraint
            target : SelectionTarget
            onSelected : 'entry -> unit
            onCancelled : unit -> unit
        }

    /// The window mode (spec G.0): `Browse` is the ordinary single-instance window; `Select`
    /// is the SAME window pre-constrained to the context's kind with exactly two extra buttons
    /// (Select / Close) — everything else IS the ordinary window, so add-on-the-fly works
    /// because it is the library. Named for the Library window (the spec's wording); the
    /// Materials window instantiates the same DU over `MaterialEntry`.
    type LibraryWindowMode<'entry> =
        | Browse
        | Select of SelectionContext<'entry>
