# Implementation plan

Wire catalogue-folder registration into the existing Library MVU model, using `CatalogueImportContext.registerFolder` and `proxy.forgetImport`. Retain every pending decision without applying it. Reuse the existing verb styling for the folder picker, and project registered folders and status through stable automation IDs in both Browse and Select modes.

Thread the app-scoped import context alongside `libraryBackups` through the Library host, workbench launchers/model, default stores, application composition and existing test fixtures. Extend `LibraryWindowTests` with the five specified pure facts and one real-window unregister smoke assertion. Already-registered fixtures explicitly confirm the complete pending list.

Files are limited to the slice's authorized UI, Controls, App and test paths, plus these prescribed worker outputs. Risks are stale launcher signatures, accidentally applying imports before confirmation, and losing the original folder name when unregistering. The supervisor owns gate results and measured baseline counts.
