# Step 0256 — impl plan

## Approach

The setup as pure Domain data: an ordered optical path of slots the table draws, plus the revisions an experiment
can reference (spec 0050 S8.2; 001 §J). It reuses step 255's identities (`SetupName`, `SetupRevision`,
`SetupReference`, `SetupError` in `Kit.fs`), the one version rule (`Lifecycle.decideVersioning` with `PinState`)
and the layout's element shape (`BeamLayout.LayoutElement`, `BeamHost`, `ElementZoom`).

1. New `OpticalConstructor.Domain/Setups.fs`, `module Setups`, compiled directly after `BeamLayout.fs` (one bare
   `<Compile Include="Setups.fs" />`, no comment). Contents, in order:
   - `SlotTurn` = `PlacementTurn | DialTurn of reading : Angle`;
   - `SlotContent` = `EmptySlotContent | ItemInSlot of item : ItemId * turn : SlotTurn`, with `itemOpt`;
   - `SetupSlot = { id; placement; zoom; beam; content }` with `layoutElement`;
   - `SetupRevisionRecord = { revision; slots }`;
   - `Setup = { name; revision; slots; history }` with `reference`, `slotsAt`, `heldItems`, `create` and
     `revisedWith`.
2. `create` refuses a held setup whose `name.fileStem` equals the new name's ignoring case. Its reason follows
   `Item.relabelledAmong` (step 216): both names and the shared `<stem>.setup-json` file.
3. `revisedWith` delegates to `decideVersioning` over `{ physics = slots; metadata = () }`. It maps `KeepCurrent`,
   `MutateInPlace` and `MintNextVersion` exactly as the slice says.
4. Tests: new `OpticalConstructor.Tests/SetupTests.fs` (`module SetupTests`), appended last in the test compile
   list, with the five census facts named verbatim.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/Setups.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/OpticalConstructor.Domain.fsproj`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/SetupTests.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj`

## Risks

- Record-label resolution: `revision` / `slots` / `id` / `name` labels are shared by several records in scope
  (`Item`, `ItemRevisionRecord`, `SetupReference`, `LayoutElement`, …). Every record construction is annotated.
- `decideVersioning` needs `SetupSlot` to support equality. Every field is structural data (`ElementPlacement`,
  DUs, `Guid`), so this should hold; the compiler will check it.
- ELEV: no new free function, every type and case name is unique (checked against the repo and STL), and all
  error cases are step 255's.
