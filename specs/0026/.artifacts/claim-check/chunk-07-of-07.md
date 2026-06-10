{
  "claims": [
    {
      "id": 1,
      "phrase": "A project round-trip fixture exercising placement + table (AC-A6, AC-C1) added under `OpticalConstructor.Tests` reusing the existing `ProjectJsonRoundtripTests` harness.",
      "spec_location": "Test artifacts (chunk lines 1134-1200)",
      "evidence": "Literal 'existing' trigger; class-anchored on `ProjectJsonRoundtripTests`. Grep resolves to exactly one production file: Berreman/OpticalConstructor/OpticalConstructor.Tests/ProjectJsonRoundtripTests.fs (also referenced in OpticalConstructor.Tests.fsproj). Harness exists.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 2,
      "phrase": "`Berreman/OpticalConstructor/OpticalConstructor.Domain/Project.fs` (edit) — extend `OpticalConstructorProject` with table + per-element placement.",
      "spec_location": "Files in scope (chunk lines 1134-1200)",
      "evidence": "Inferred assumption-shape: the '(edit)' marker asserts the file and the named type pre-exist (phrase inferred, not in the default/project trigger list). Berreman/OpticalConstructor/OpticalConstructor.Domain/Project.fs exists; Grep found `type OpticalConstructorProject =` at Project.fs:26.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 3,
      "phrase": "`Berreman/OpticalConstructor/OpticalConstructor.Storage/schema/optical-constructor-project.schema.json` (edit) — add placement + table representation.",
      "spec_location": "Files in scope (chunk lines 1134-1200)",
      "evidence": "Inferred assumption-shape: the '(edit)' marker asserts the file pre-exists (phrase inferred). Glob confirms Berreman/OpticalConstructor/OpticalConstructor.Storage/schema/optical-constructor-project.schema.json exists.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 4,
      "phrase": "`Berreman/OpticalConstructor/OpticalConstructor.Ui/Shell.fs` (edit) — add the Constructor and Legacy pages to `Page`; route through `RootModel`/`RootMsg`; default landing.",
      "spec_location": "Files in scope (chunk lines 1134-1200)",
      "evidence": "Inferred assumption-shape: the '(edit)' marker asserts the file and the named types pre-exist (phrase inferred). Berreman/OpticalConstructor/OpticalConstructor.Ui/Shell.fs exists; Grep found `type Page =` (Shell.fs:73), `type RootModel =` (Shell.fs:94), `type RootMsg =` (Shell.fs:121).",
      "verdict": "CONFIRMED"
    },
    {
      "id": 5,
      "phrase": "`Berreman/OpticalConstructor/OpticalConstructor.Ui/UserEnvironment.fs` (edit) — add `language` and the configurable key map to `EnvironmentSettings`.",
      "spec_location": "Files in scope (chunk lines 1134-1200)",
      "evidence": "Inferred assumption-shape: the '(edit)' marker asserts the file and the named type pre-exist (phrase inferred). Berreman/OpticalConstructor/OpticalConstructor.Ui/UserEnvironment.fs exists; Grep found `type EnvironmentSettings =` at UserEnvironment.fs:141.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 6,
      "phrase": "`Berreman/OpticalConstructor/OpticalConstructor.Ui/optical-constructor-environment.schema.json` (edit) — add `language` + key-map fields.",
      "spec_location": "Files in scope (chunk lines 1134-1200)",
      "evidence": "Inferred assumption-shape: the '(edit)' marker asserts the file pre-exists (phrase inferred). Glob confirms Berreman/OpticalConstructor/OpticalConstructor.Ui/optical-constructor-environment.schema.json exists.",
      "verdict": "CONFIRMED"
    },
    {
      "id": 7,
      "phrase": "`Berreman/OpticalConstructor/OpticalConstructor.App/Program.fs` (edit) — startup localization completeness check + copyable error surface; constructor as default landing.",
      "spec_location": "Files in scope (chunk lines 1134-1200)",
      "evidence": "Inferred assumption-shape: the '(edit)' marker asserts the file pre-exists (phrase inferred). Berreman/OpticalConstructor/OpticalConstructor.App/Program.fs exists.",
      "verdict": "CONFIRMED"
    }
  ]
}
