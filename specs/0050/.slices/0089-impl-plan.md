# Step 0089 implementation plan

Implement S24-LAUNCHER-RECREATE within the five authorized source files. Extract the existing database-backed AppContext composition into a private static member, then share it between open/create and LibraryDatabase.recreate so recreated libraries import the Built-in catalogue through the existing seam.

Give LauncherWindow the configured DataLocations and WorkbenchSettings. Build its content from the open outcome, offer the stable, accessible recreate button only for an initial schema mismatch, and replace the content after the action. Preserve the remedy and display the new diagnostic when recreation fails; on success enable all four surface buttons. Update startup and every launcher test call site.

Add the two specified LauncherTests facts: schema-only visibility and click-driven, file-backed preservation of the schema-2 backup plus a schema-1 live library. Reuse TestAppScope locations and existing headless interaction helpers. Retain the existing scrolling and shared window launchers.

Risks are losing the retry action after a non-schema recreation error, leaving SQLite connections open during fixture preparation, and changing the fixed launcher layout. The worker role reserves formal acceptance and checkpoints for the supervisor. The coding environment's higher-priority verification requirement also requires local build, elevation, selected UI tests and section invariants; record those observations separately from supervisor acceptance. No version, schema or gate changes are planned.
