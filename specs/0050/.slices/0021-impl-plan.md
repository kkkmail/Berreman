# Step 0021 — IMPLEMENT — impl plan

## Approach

This step is a pure MOVE (001 §B, `.spec-md` §5 "Canonical JSON + SHA-256", §6 "JSON + schema"). No
output changes.

1. Create `OpticalConstructor.Storage/CanonicalJson.fs` with `module CanonicalJson` in
   `namespace OpticalConstructor.Storage`. Move `ProjectJson.options` and its five private
   converters into it verbatim: `BeamBranchConverter`, `RotationLockConverter`,
   `BeamPresenceConverter`, `ToggleStateConverter` and `ComplexMatrixConverter`. The module doc and
   the `options` doc say that this is the one options value every JSON store of the app reads.
2. Add one `<Compile Include="CanonicalJson.fs" />` line to `OpticalConstructor.Storage.fsproj`,
   between `SchemaValidation.fs` and `ProjectJson.fs`. It adds no new comment.
3. Delete the moved code from `ProjectJson.fs`. `parseProject` and `serializeProject` then read
   `CanonicalJson.options`. Drop the opens that are no longer used: `System.Numerics`,
   `System.Text.Json.Serialization` and `MathNet.Numerics.LinearAlgebra`.
4. Re-point every reader and change nothing else at those sites: `GroupsLibrary.fs` (3 sites),
   `RecentFiles.fs` (2), `Report.fs` (3), `UserEnvironment.fs` (3) and `DispersionModelsTests.fs` (2).
5. Sweep the comments the slice lists: `ProjectJson.options` becomes `CanonicalJson.options`, and
   "`ProjectJson`'s converter" / "(`ProjectJson`)" name `CanonicalJson` instead.
6. Run `git grep -n -F "ProjectJson.options" -- Berreman/`. It must find nothing.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Storage/CanonicalJson.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/ProjectJson.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/OpticalConstructor.Storage.fsproj`
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/GroupsLibrary.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/RecentFiles.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/Report.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/UserEnvironment.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/Localization.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/Groups.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/Placement.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/DispersionModelsTests.fs`

## Risks

- **The fsproj comment.** The existing comment at `OpticalConstructor.Storage.fsproj:26-27` names
  `ProjectJson.options`. Unless it is re-pointed, the acceptance grep finds it. The fsproj is an
  authorized edit, so that existing comment is re-pointed to `CanonicalJson`. No new comment is
  added.
- **Name uniqueness.** `CanonicalJson` is a new module name and is used nowhere else. `options`
  moves, so the tree still has exactly one public `options` serializer value. The converters
  stay private.
- **Behaviour.** The converter registration order and the `JsonFSharpOptions` chain are copied
  byte-for-byte, so every round-trip fact stays green.

## Round note

The first local ELEV run found ELEV013 on `ProjectJson.serializeProject`. The rule's exemption
(reading a module value of its own module) no longer held once `options` moved. Per the analyzer,
`serializeProject` became an `OpticalConstructorProject` extension member in `ProjectJson`. Its
callers in `Report.fs`, `ProjectFile.fs`, `PlacementTests.fs` and `ProjectJsonRoundtripTests.fs`
take the member form. The deviation is recorded in the impl-log's Decisions.
