# Step 0029 — IMPLEMENT — impl-plan

## Approach

Put the one JSON-Schema evaluation body in `SchemaValidation.validateWith`:

- signature: `validateWith (schema : Lazy<Result<JsonSchema, StorageError>>) (element : JsonElement) : Result<unit, StorageError>`;
- body: force the lazy, evaluate with `EvaluationOptions(OutputFormat = OutputFormat.List)`, flatten the
  messages with `collectMessages`, and fall back to `schema validation failed`.

Then:

- `validate` becomes `validateWith schema element` over the private project-schema lazy, so `ProjectJson`
  is unchanged.
- `GroupsLibrary.validateGroupsLibrary` and `UserEnvironment.validateUserEnvironment` lose their copies
  of the body and call `validateWith` over their own lazies. They keep their names, lazies and schema
  files (so their `$id`s).
- `collectMessages` has no caller left outside `SchemaValidation`, so it becomes private. Its "public
  so the groups store reuses this" comment would otherwise go stale.
- Update the docs that described the old sharing (the GroupsLibrary module doc, both validators' docs)
  to say they reuse `validateWith`.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Storage/SchemaValidation.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/GroupsLibrary.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/UserEnvironment.fs`

No test file changes (the slice authorizes none). `RoundTripTests`, `GroupsRoundTripTests` and
`EnvironmentRoundTripTests` must pass unchanged.

## Risks

- **ELEV.** `validateWith` is a two-parameter public free function over third-party and Storage types,
  the same shape as `validate`, so ELEV003/010/013 do not apply. The name is unique in Berreman and STL
  (grep). Making `collectMessages` private could surface ELEV003P on `string list`; the gate run will
  show it.
- **Fallback text.** The groups and environment fallbacks become the generic `schema validation failed`.
  `GroupsRoundTripTests` asserts only that `groups schema validation failed` is absent, and the
  environment test only that messages exist, so both stay green.
