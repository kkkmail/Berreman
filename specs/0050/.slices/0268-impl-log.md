# Step 0268 — impl log

## Progress

1. Read the task, both system prompts, the project prompt, the slice spec and the `.gates` snapshot. No operator
   note was in flight.
2. Surveyed the inputs the grammar names:
   - `Kit.NamingTemplate` and `NamingTemplate.standard` (step 267, `Kit.fs`);
   - `Kit.ItemLabel.tryCreate`, which refuses a blank label;
   - `Setups.SlotPosition` and `Setups.SlotRole`, declared late in `Setups.fs` (step 257).

   `NamingTemplates.fs` therefore compiles directly after `Setups.fs`.
3. Checked every new type, DU case and module name with `git grep` over Berreman and STL (`C:\GitHub\Softellect`).
   Nothing holds any of them; the only hit is the forward reference in `Kit.fs`'s `NamingTemplate` doc. The
   `NamingTemplate` extensions (`pieces`, `tryCreate`) are the only extension members on that receiver (ELEV019).
4. Read STL's analyzer sources (read-only) to confirm how local bindings are treated:
   - ELEV003 and ELEV011 walk `Tree.collectMembers` and `collectEntities` only;
   - ELEV010 checks module-level functions and members only;
   - `collectLocalBindings` is used by no rule.

   So the scanning helpers live as locals inside `pieces`, as the slice prescribes.
5. Tests first: `OpticalConstructor.Tests/NamingTemplateTests.fs` (`module NamingTemplateTests`) was appended last in
   `OpticalConstructor.Tests.fsproj` and holds the five census facts. **Red:** the build failed in the tests with
   FS0039 (`TemplatePlaceholder`, `TemplatePiece`, `PlaceholderNeed`, `ValuePiece`, `LiteralPiece`,
   `SlotPlaceholder`, `AnglePlaceholder` are not defined) — `0268-red-check-build.log`.
6. `OpticalConstructor.Domain/NamingTemplates.fs`, as the slice describes:
   - types: `NamePart`, `NameAlias`, `PlaceholderNeed`, `TemplatePlaceholder` (`text`, `need`), `TemplateValue`,
     `TemplatePiece` and `NamingTemplateError` (`describe`);
   - `type NamingTemplate with`: `pieces` and `static member tryCreate`;
   - one `<Compile Include="NamingTemplates.fs" />` line after `Setups.fs`, with no comment.
7. Green: the build is clean and the class passes 5/5. Printed every refusal's reason through `dotnet fsi`
   (`0268-reasons.fsx` / `.log`). Reworded the reasons scoped to one placeholder as `in {obj0}, …`, so a template
   that is a single placeholder no longer repeats itself.
8. Development runs of each gate's command over the final tree (below).

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/NamingTemplates.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/OpticalConstructor.Domain.fsproj` (one compile line)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/NamingTemplateTests.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj` (one compile line)

These are exactly the four edits the step authorizes. `specs/0050/.manifest.state.json` was already modified when
the round started (the supervisor's); it was not touched.

## Decisions

- **Word and number split at the first ASCII digit.**
  - `{obj1a}` reads as the word `obj` and the number `1a`, which is refused as no slot number.
  - `{1obj}` has an empty word, which names no placeholder.
  - Words are matched exactly and case-sensitively (`{Source}` is refused): the slice lists them in lower case.
- **A number that overflows `int`** (`{obj99999999999}`) is a `TemplateSyntaxErr`, never a throw. The digits are
  checked with `Char.IsAsciiDigit` before `Int32.TryParse` with `NumberStyles.None`.
- **The alias list.**
  - The placeholder splits at its first `:`. The list splits on `,`, and each entry on `=` into exactly two sides,
    each non-empty and free of `{ } : = ,`.
  - An empty list (`{source:}`) is one empty entry, so it is refused as a malformed alias.
  - The label then goes through `ItemLabel.tryCreate`, so a blank label (`{source: =CPL}`) is refused with the item
    error's text.
  - Label and part are kept as given, with no trimming, as `ItemLabel` keeps its text.
- **Duplicate aliases.** Labels compare ignoring case (`StringComparison.OrdinalIgnoreCase` on the label's text,
  as the slice says). Parts compare exactly, since a name written with `CPL` and one with `cpl` can be told apart.
- **Check order** follows the slice: syntax while scanning, then a repeated placeholder, two adjacent values and a
  repeated alias. The first refusal found is answered, as an ordered `List.tryPick` over the three checks.
- **The shared ordered Result collector is not reachable here.** `ItemEvaluation.sequenced` is typed to
  `ItemEvaluationError` and compiles last, after this module. The alias list uses the Domain's inline
  `fold … Result.bind … (Ok [])` idiom (`CatalogueImport.fs`, `KitFolders.fs`), as a local of `pieces`.
- **`need` compares with `SlotPosition 1`** instead of reaching into the union or reading `.value`. Position 0
  never comes out of the parser.
- **Reasons.** A reason opens with the template's text, then names the placeholder as written (`in {phi1:A=B}, …`)
  or the character position (1-based) of a stray brace. `MissingNameValueErr`, `NameMismatchErr` and
  `UnknownNameItemErr` are declared and rendered now, as the slice requires. They are raised by steps 269 and 271.

## Testing state

No gate verdict is claimed (Invariant 6). Development runs used each gate's own command over the final tree; the
logs are in `specs/0050/.artifacts/0268-dev-*.log`.

| Gate | Outcome | Duration / deadline |
|---|---|---|
| build | exit 0, 0 warnings, 0 errors | 52 s / 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, known findings 0 (all 12 projects, no base) | 199 s / 600 s |
| constructor-unit-tests | `Passed!`, 1435 succeeded, 0 failed, 0 skipped (1430 at step 0267, +5) | 69 s / 180 s |
| fifteen s27 invariants | each `INVARIANT HOLDS … found nothing`, exit 0 (`0268-dev-invariants.log`) | — |

- All five census identities are `Passed` in `TestResults/constructor-unit-tests.trx` under their exact names.
- `git grep` searches tracked files only, so the two new (untracked) files were also grepped for every invariant's
  words: nothing.
- The red-check build showed the vendored MathNet's two existing `SYSLIB0051` warnings
  (`MathNetNumerics/Numerics/Exceptions.cs`, untouched) while MathNet recompiled, as at steps 0265-0267. The final
  builds showed none.
- Every touched file is LF (no CR byte); `git diff --numstat` equals `--ignore-cr-at-eol`.

## Artifacts

- `specs/0050/.artifacts/0268-red-check-build.log`
- `specs/0050/.artifacts/0268-dev-build.log`, `0268-dev-constructor-unit-tests.log`, `0268-dev-elevation.log`,
  `0268-dev-invariants.log`
- `specs/0050/.artifacts/0268-reasons.fsx`, `0268-reasons.log` — every refusal's rendered reason

## Gotchas

- In Git Bash, `grep -c $'\r'` counted a "CR" on every line of LF files. Count CR bytes with `tr -cd '\r' | wc -c`,
  or use `git ls-files --eol`.
- `git grep` (the invariants) does not see a new file until it is tracked. Grep new files directly when checking an
  invariant before the commit.
