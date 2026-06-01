namespace OpticalConstructor.Storage

open OpticalConstructor.Domain.Project

/// Whole-project-snapshot undo/redo (§I.6 / R-2). `EditHistory` is an immutable F#
/// zipper over three project-snapshot lists; every destructive model edit in the UI
/// `push`es the new project onto it, and `undo` is its exact inverse — discharging
/// the Operator-facing UX checklist's opposing-action requirement. Holds whole
/// `OpticalConstructorProject` snapshots in memory; on-disk undo journals,
/// command-pattern diff logs, and bounded-history eviction are OUT OF SCOPE (the
/// I.8 design-history file is the durable record). The history depth bound, if any,
/// is owned by the UI model (Part J). This is the multi-level history Part B §B.10
/// defers here; the construction page's single-level snapshot swap is the
/// degenerate one-deep case.
module History =

    /// Immutable undo/redo state (§I.6): `past` (most-recent first), the `present`
    /// project, and `future` (next-redo first). No mutable field, no `.Value`/
    /// `.IsSome`; `undo`/`redo` pattern-match the lists.
    type EditHistory =
        {
            past : OpticalConstructorProject list
            present : OpticalConstructorProject
            future : OpticalConstructorProject list
        }

    /// Seed a history at `present` with empty past/future (§I.6).
    let ofPresent (present : OpticalConstructorProject) : EditHistory =
        { past = []; present = present; future = [] }

    /// Record a destructive edit (§I.6): the old `present` moves onto `past`, `next`
    /// becomes `present`, and `future` is cleared (a fresh edit invalidates redo).
    let push (next : OpticalConstructorProject) (h : EditHistory) : EditHistory =
        { past = h.present :: h.past; present = next; future = [] }

    /// Step back one edit (§I.6). A no-op (returns the input unchanged) when `past`
    /// is empty; otherwise the head of `past` becomes `present` and the old
    /// `present` moves onto `future` so `redo` can replay it.
    let undo (h : EditHistory) : EditHistory =
        match h.past with
        | [] -> h
        | previous :: rest -> { past = rest; present = previous; future = h.present :: h.future }

    /// Step forward one edit (§I.6). A no-op when `future` is empty; otherwise the
    /// head of `future` becomes `present` and the old `present` moves back onto
    /// `past`. `redo` is the exact inverse of `undo`.
    let redo (h : EditHistory) : EditHistory =
        match h.future with
        | [] -> h
        | next :: rest -> { past = h.present :: h.past; present = next; future = rest }
