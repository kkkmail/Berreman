# Step 0362 — impl plan

The study's unknowns, each with its scope, and the owners each scope gives it (spec 0050 S11.1; 001 §I, §L).

## Approach

All production code goes at the end of `OpticalConstructor.Domain/Studies.fs`, after step 361's
`Sample.openedMaterialsIn`. Every name it needs is already open there: `Kit` (`Item`, `ItemId`,
`UnknownParameterData`), `Experiments` (`SessionName`, `ExperimentName`), `MaterialLibrary` (`MaterialVersionId`) and
`ItemModels` (`ParameterName.gain`).

1. **`UnknownOwner`**: `ItemOwner of ItemId | MaterialOwner of MaterialVersionId | SessionOwner of SessionName |
   ExperimentOwner of ExperimentName`. It is what holds one value of an unknown. It is derived and never stored, so it
   has no `value` tag.
2. **`StudyUnknown`**: `{ parameter : StudyParameterRef; scope : ParameterScope; unknown : UnknownParameterData }`.
3. **`type Study with`**:
   - `scopeOf parameter`: the study's `ScopedParameter` for the parameter, else the default.
     - A material component defaults to `MaterialScope`.
     - An item parameter named `ParameterName.gain` defaults to `SessionBlockScope`.
     - Every other item parameter defaults to `ItemScope`.
   - `unknownsWith used`: the unknowns, in this order.
     - For each item of `used` in order, every `UnknownParameter` parameter as an `ItemParameterRef`.
     - Then for each opened material in order, every `UnknownParameter` component as a `MaterialParameterRef`.
     - A link's follower is left out. Each unknown takes `scopeOf`.
4. **`type StudyUnknown with`**, `owners sessions users`:
   - Under `ItemScope` / `MaterialScope`, the parameter's one owner: `ItemOwner` of its item, `MaterialOwner` of its
     material.
   - Under `SessionBlockScope`, one `SessionOwner` per session that holds a user, in session order.
   - Under `ExperimentScope`, one `ExperimentOwner` per user, in order, each once.

No other type or member is added.

## Tests (`StudyTests`, +2)

The fixtures are test-local items made by `ItemDraft.toItem`, then `withParameterState`:

- CPL1 and CPL2 over the built-in circular-polarizer model;
- Phone over the intensity-detector model.

The opened absorbing material comes from `Study.openMaterial` and `withMaterialParameterState`, over
`CrystalMaterialFixtures.absorbingTriclinicParameters`.

- `the study's unknowns are its items' and opened materials' unknown parameters, with their scopes, leaving out link
  followers`:
  - the default list in order;
  - a `ScopedParameter` making CPL1's retardance `ExperimentScope`;
  - a link literal whose follower is CPL2's retardance.
- `an unknown's owners are its item or material, one per session holding a user, or one per user`: the four scopes
  over sessions `Session 1` [E1; E2] and `Session 2` [E3].

## Risks

- **ELEV.** The names are unique: grep found none of them in Berreman or STL. No primitive appears in any signature.
  Owners are built from elevated values, so nothing is re-wrapped.
- **The s36 invariants** grep `Berreman/`. The new code adds none of their words.
