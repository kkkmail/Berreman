# Slice 0110 implementation plan

Wire Materials-window drops through the existing CatalogueImportContext and
ImportDialogWindow. Add semantic drop/completion messages and model status, with
the composition root opening the dialog and dispatching its applied summary.
Preserve Select/Browse mode and existing confirmation gates throughout import.

Modify only MaterialsWindowView.fs, MaterialsWindow.fs, Controls/UiIds.fs and
Ui.Tests/MaterialsWindowTests.fs. Reuse entry serialization, in-memory import
proxies, TestAppScope and the existing headless helpers. Add the four specified
UI facts and one smoke acceptance proving no write before Apply, shared-store
visibility afterwards, and the rendered completion status. Cover empty drops,
refusal rendering and preserved model state within those facts.

Run the resolved build, elevation, smoke, selected UI tests and eleven section
invariants under the session-level verification requirement. Keep artifacts in
the supplied arc folder and leave supervisor state, gates and baselines alone.
Main risks are context-record construction, event subscription cleanup, and the
live material projection refreshing after the dialog's completion callback.
