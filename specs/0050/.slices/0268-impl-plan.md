# Step 0268 — impl plan

The kit's naming template gets its grammar as pure Domain (spec 0050 S8.3; 001 §J, §A, §O). Step 267
stored the template as text (`Kit.NamingTemplate`). This step reads that text into its pieces and refuses
a text that is no template. Writing names and parsing them back are steps 269-271.

## Approach

1. New `OpticalConstructor.Domain/NamingTemplates.fs`: `module NamingTemplates` in
   `namespace OpticalConstructor.Domain`. It opens `Kit` and `Setups` and is compiled directly after
   `Setups.fs`, because a placeholder names a `SlotRole`. It declares, in order:
   - `NamePart of string` with `.value`;
   - `NameAlias = { label : ItemLabel; part : NamePart }`;
   - `PlaceholderNeed` (`RequiredPlaceholder` | `OptionalPlaceholder`);
   - `TemplatePlaceholder` (`SlotPlaceholder of SlotRole` | `AnglePlaceholder of SlotPosition` |
     `NotePlaceholder`), with `.text` (the word and its number) and `.need`;
   - `TemplateValue = { placeholder; aliases }` and `TemplatePiece` (`LiteralPiece` | `ValuePiece`);
   - `NamingTemplateError`, seven `of reason : string` cases ending in `Err`, with `.describe`;
   - `type NamingTemplate with`: `member this.pieces` and `static member tryCreate`.
2. `pieces` is a left-to-right scan done by locals inside the member:
   - text outside braces collects into one `LiteralPiece` per run, and an empty run is never made;
   - `{` opens a placeholder and `}` closes it; a stray `}`, a nested `{` or an unclosed `{` is a
     `TemplateSyntaxErr`;
   - the placeholder's head is a word plus an optional number (ASCII digits, no leading `0`; none for
     `light` and `note`; no number means 1);
   - after a `:` comes the comma-separated `label=part` alias list. Each label and part is non-empty and
     free of `{ } : = ,`, and the label goes through `ItemLabel.tryCreate`. Aliases are allowed on slot
     placeholders only.
   - Then, in the slice's order, it refuses a repeated placeholder (`DuplicatePlaceholderErr`), two
     adjacent values (`AdjacentPlaceholdersErr`), and an alias list that repeats a label (ignoring case) or
     a part (`DuplicateAliasErr`).
3. `tryCreate text` answers `NamingTemplate text` when `pieces` is `Ok`, and its error otherwise.
4. `OpticalConstructor.Domain.fsproj`: one `<Compile Include="NamingTemplates.fs" />` line after
   `Setups.fs`, with no comment.
5. Tests: new `OpticalConstructor.Tests/NamingTemplateTests.fs` (`module NamingTemplateTests`), appended
   last in `OpticalConstructor.Tests.fsproj`. It holds the five census facts, named exactly as the slice
   names them.

## Risks

- ELEV: every new type, DU case and module name was checked against Berreman and STL with `git grep`, and
  none is held. The `NamingTemplate` extensions (`pieces`, `tryCreate`) are the only members on that
  receiver. Scanning helpers stay local to the members, which the signature rules do not read.
- Literal braces in interpolated strings must be doubled (`{{` / `}}`) in the error reasons.
- Run the red check first: the tests must fail to compile before `NamingTemplates.fs` exists.
