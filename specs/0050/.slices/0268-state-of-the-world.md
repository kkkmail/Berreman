# Step 0268 — state of the world

## Where we are

This is step 0268 of spec 0050, the second step of section S8.3 (the naming template and the grid generator). Step
0267 stored the kit's naming template as text. This step gives it a grammar as pure Domain: `NamingTemplate.pieces`
reads a template's text into its literals and placeholder values, and `NamingTemplate.tryCreate` refuses a text
that is no template with a typed error. Writing names from a template, reading them back, the reference pipeline's
labels and the grid generator follow in steps 269-271 and later.

## What's working

- Add `NamingTemplates.fs` (Domain, after `Setups.fs`) with the grammar's types: `NamePart`, `NameAlias`,
  `PlaceholderNeed`, `TemplatePlaceholder`, `TemplateValue`, `TemplatePiece` and `NamingTemplateError`.
- Placeholders speak the polarimeter's words: `light`, `source<k>`, `obj<k>`, `phi<k>`, `analyzer<k>`, `detector<k>`
  and `note`. No number means 1, and each placeholder knows whether a name must fill it (`need`).
- `NamingTemplate.pieces` reads 001 §J's standard template into its seven values and six literals. Aliases
  (`{source:CPL1=CPL,LP1=LP}`) are read in order.
- `NamingTemplate.tryCreate` refuses a malformed text with its reason:
  - `TemplateSyntaxErr` for a brace without its partner, an unknown word or number, or a malformed or misplaced
    alias;
  - `DuplicatePlaceholderErr`, `AdjacentPlaceholdersErr` and `DuplicateAliasErr` for the other three faults.
- New `NamingTemplateTests` with five facts; constructor-unit-tests 1430 → 1435.

## Tests

No gate verdict is claimed (Invariant 6); the supervisor runs the frozen roster. The development runs below used each
gate's own command over the final tree, with logs in `specs/0050/.artifacts/0268-dev-*.log`.

- **build:** exit 0, 0 warnings, 0 errors, 52 s (deadline 600 s). The red-check build before it recompiled the
  vendored MathNet and showed its two existing `SYSLIB0051` warnings (`MathNetNumerics/Numerics/Exceptions.cs`,
  untouched), as at steps 0265-0267.
- **elevation:** `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, known findings 0, 199 s (deadline 600 s). The scope was all 12 projects, since a standalone run has no comparison base.
- **constructor-unit-tests:** `Passed!`, 1435 succeeded, 0 failed, 0 skipped, 69 s (deadline 180 s).
  - The count is 1430 at step 0267, so it rises by 5, as the acceptance requires.
  - Every census identity is `Passed` in `TestResults/constructor-unit-tests.trx` under its exact name:
    - `the standard template is the lab's: the source, two objects at their angles, the analyzer and the note`;
    - `a placeholder without a number names the first slot of its role, and each role has its word`;
    - `an alias writes a kit item under another name in its placeholder`;
    - `a text that is no naming template is refused with its reason`;
    - `every naming template error renders its condition and its payload`.
- **The fifteen s27 invariants** each report `INVARIANT HOLDS … found nothing` (`0268-dev-invariants.log`). The two
  new, still untracked files were grepped for the same words directly: nothing.
- **Red, measured** (`0268-red-check-build.log`): with the tests in place and no `NamingTemplates.fs`, the build
  failed with FS0039 on `TemplatePlaceholder`, `TemplatePiece`, `PlaceholderNeed`, `ValuePiece`, `LiteralPiece`,
  `SlotPlaceholder` and `AnglePlaceholder`.
- Every touched file is LF; `git diff --numstat` equals `--ignore-cr-at-eol`. No run exceeded its deadline.

```yaml
gates:
  constructor_unit_tests: 1435
```

## Architecture

- The grammar is pure Domain beside the setups it names. A slot placeholder carries a `SlotRole` (step 257), and an
  angle placeholder carries the Obj slot's `SlotPosition`. A placeholder is never stored apart from the template's
  text, so it has no wire tag.
- `pieces` is one left-to-right scan done by locals inside the member, so no module-level signature carries a
  primitive. The refusals after the scan are an ordered list of checks, and the first one found is answered.
- `NamingTemplate` stays the stored text from step 267. `tryCreate` only adds a checked way to make one, and every
  use of a template's structure goes through `pieces`, which answers a `Result`.
- `NamingTemplateError` already declares the three name-side cases (`MissingNameValueErr`, `NameMismatchErr`,
  `UnknownNameItemErr`) with their renders. Steps 269 and 271 raise them.

## Deferred

Nothing. Writing names from a template (269), reading them back (269, 271), the reference pipeline's labels (270) and
the grid generator are separate steps of S8.3, not this step's scope.

## Gotchas

- Labels in one alias list compare ignoring case; parts compare exactly. `{source:A=CPL,B=cpl}` is a valid template.
- Placeholder words are case-sensitive (`{Source}` is refused).
- The word/number split is at the first ASCII digit, so `{obj1a}` is refused as a bad number, not an unknown word.
- `git grep`, which the s27 invariants use, does not see an untracked file. Grep new files directly before the
  commit.
- In Git Bash, `grep -c $'\r'` counts every line of an LF file. Count CR bytes with `tr -cd '\r' | wc -c`.

## Changelog

- 2026-11-10 — step 0268: the naming template's grammar (`NamingTemplates.fs`: `NamePart`, `NameAlias`,
  `PlaceholderNeed`, `TemplatePlaceholder`, `TemplateValue`, `TemplatePiece`, `NamingTemplateError`,
  `NamingTemplate.pieces` / `tryCreate`); constructor-unit-tests 1430 → 1435.
