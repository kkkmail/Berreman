# Step 0267 — impl plan

The kit file gains its naming template as stored text (spec 0050 S8.3, 001 §J / §A).
The grammar, reading and writing are steps 268-271; this step adds only the stored
shape and its default.

## Approach

1. `OpticalConstructor.Domain/Kit.fs`: declare `NamingTemplate of string` (with `.value`
   and `static member standard`) directly before `KitFile`. `KitFile` gains
   `namingTemplate : NamingTemplate` after `sendExclude`; `KitFile.create` sets
   `NamingTemplate.standard`. No validation here.
2. `OpticalConstructor.Storage/schema/kit.schema.json`: under the `schemaVersion = 1`
   branch, `namingTemplate` joins `required` and `properties` as a string. Schema version
   stays 1; `additionalProperties: false` stays. No up-converter or default-on-read.
3. Serialization needs no code: `KitFile.fileText` serializes the record through the
   shared options, which unwrap single-case unions, so the template is a JSON string and
   the canonical writer sorts keys (`namingTemplate` lands between `name` and `notes`).
4. Tests:
   - `KitTests`: new fact `a kit's naming template keeps its text, and a new kit takes the standard one`.
   - `KitFileJsonTests`: the fixture kit carries
     `NamingTemplate "{source:CPL1=CPL,LP1=LP}-({obj1}#{phi1})-{analyzer:CPL2=CPL}"`; the
     round-trip fact lists the nine keys and checks the template's text verbatim; the
     refusal fact also refuses the text without `namingTemplate` and with it set to `7`.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/Kit.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/schema/kit.schema.json`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitFileJsonTests.fs`

## Risks

- Every full `KitFile` record literal must gain the field: only `KitFile.create` builds
  one; the tests use `{ KitFile.create … with … }`, so they compile unchanged.
- No committed `.kit-json` exists (`git ls-files`), so requiring the key breaks no stored kit.
- `NamingTemplate` is a new type and DU-case name; no namesake in Berreman or STL (ELEV014/016).
