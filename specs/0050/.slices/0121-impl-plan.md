# Implementation plan

Wire the Details bay to the existing licence-attribution pipeline for spec 0050 S3.3. Add the public `TableAndElementRotationModel.detailsAttributionLines` member beside `detailsState`, resolving the selected binding through `boundEntry`, using `entry.attributedEntryIds`, and returning no lines for missing selections, missing bindings, or read errors. Append each rendered line below the existing title and optional out-of-band warning.

Modify only the two authorized source files: `TableAndElementRotationView.fs` and `LayerBandsControlsTests.fs`. Reuse `BuiltinCatalogue.licensedMaterialContent`, `sampleContentOver`, and `putSealedEntries` with the `initMain` model's own catalogue context. Add exactly the two authorized facts. Cover CC-BY citation, CC0 licence-only credit, built-in/no-binding/no-selection/error cases, and real-view Details-tab input followed by the title's semantic projection.

Risks are preserving title/warning order, sharing the model's live import stores, and refreshing the headless view after input. Reuse the existing model update, ribbon id, and headless mounting patterns. No durable shape changes or migrations are needed. The WIRE_UI protocol reserves builds, test execution, analyzers, invariants, and measured counts for the supervisor; this worker implements and records the handoff without running gates.
