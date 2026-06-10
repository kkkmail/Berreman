# Architecture critique -- 003.slice-md cycle 1

## Summary

Clean slice with one or two architectural suggestions, none gate-blocking.
The localization mechanism lands as a deliberate dependency-free leaf module
that `UserEnvironment` consumes for the `Language` DU, the `strings.json`
content item mirrors the env-schema shipping pattern exactly, and all four
ACs are covered. The single most useful suggestion: the unit-symbol glyphs
in `Localization.Symbols` are divorced from the `UnitOfMeasure` type they
describe in the units spine, which sets up a second source of truth for a
later slice.

## Separation of concerns

`Localization.Symbols` (Localization.fs:213-234) hard-codes the unit display
glyphs (`meter = "m"`, `millimeter = "mm"`, `micrometer = "µm"`, … `wavenumber
= "cm⁻¹"`) as loose string constants, disconnected from the
`UnitOfMeasure` DU that names exactly these units
(Units.fs:20-27). Constraint 0.2 designates `Units.fs` as the SOLE
unit-conversion seam, and the natural home for "the display glyph of a unit"
is right there next to `toMeters`/`fromMeters`. As written there is no
`UnitOfMeasure -> string` mapping anywhere; a later slice that needs to render
the symbol for a given `UnitOfMeasure` value will either reach into
`Symbols.millimeter` by hand or add its own match, creating drift. Smallest
fix: add `unitSymbol : UnitOfMeasure -> string` in `Units.fs` and have
`Symbols`/`neutralSymbol` reference it, so the unit-glyph knowledge has one
home. The Ψ/Δ/°/R1-R3 constants, by contrast, have no domain type to attach
to and are fine where they are.

## Consistency

Strong adherence to existing precedent. The `language` field follows the
`Theme` fieldless-DU pattern exactly (UserEnvironment.fs:148 `theme : Theme`):
a fieldless DU, round-tripped as a bare string through the shared
`ProjectJson.options` (`.WithUnionUnwrapFieldlessTags()`, ProjectJson.fs:76),
constrained by a schema `enum` mirroring the existing `theme` enum
(schema lines 83-88). No second serializer, no per-field converter — the
round-trip is free, and the two new AC-I2 tests exercise it. The
`strings.json` content item (Ui.fsproj:127-131) is a byte-for-byte mirror of
the env-schema content item above it (Ui.fsproj:115-119), so the
runtime-read-beside-the-assembly behaviour and transitive flow to consuming
projects are identical to the already-validated schema path. One minor note:
the loader uses a bespoke `System.Text.Json` `JsonDocument` walk
(Localization.fs:101-122) rather than the `OpticalConstructor.Storage` stack
the rest of the solution serializes through. That is defensible here —
`strings.json` is a simple human-authored map with no schema, and keeping
`Localization` a leaf with zero project dependencies is what lets
`UserEnvironment` depend on it — so I read this as an intentional trade, not a
drift.

## Spec fit

All four ACs are met and the scope boundaries the slice drew are respected:
no Settings-ribbon selector control (deferred to 006), no key-map fields
(deferred to 005), no constructor-as-default-landing edit (deferred to 006).
The `EnvironmentRoundTripTests.fs` edit sits outside the "files in scope"
list, but the testing plan explicitly routes AC-I2 "via the existing
`EnvironmentRoundTripTests` harness", the SoW documents the choice, and the
diff adds two tests without altering any existing one — so this is sanctioned,
not creep. The SoW "Files modified" set matches `git diff HEAD` plus the three
new untracked files; no omissions.

One precision gap worth flagging. The SoW states "the mechanism supports
adding [a language] by editing strings.json", and I.1.1 says a translator can
"add a further language … by editing that file — no rebuild required." The
resource *format* genuinely supports this — `parse`/`lookup` key on arbitrary
language-code strings — but the `Language` DU, `languageCode`, and the schema
`enum` are all closed to `English`/`Russian`. So an editor can add the *data*
for a third language to the file, but cannot *select* it without a DU case +
`languageCode` arm + schema-enum edit (i.e. a rebuild). Given 0.6 ("no third
language ships"), this is an acceptable posture, but the claim slightly
overstates it. Suggestion: state the boundary explicitly (file-edit covers
translations and string content; activating a brand-new selectable language is
a deliberate code change because none ships), or let `lookup` accept a raw code
string with English fallback so the data path is truly code-free.

## Evolvability

`strings.json` pre-commits a fairly broad key catalogue (`menu.*`,
`command.*`, `element.*`, `settings.*`, `rotation.*`) that the later UI slices
(005/006+) do not yet consume. Because the `AC-I3 the shipped resource is
complete` test asserts `startupCheck` returns `None` for both EN and RU, every
key shipped now carries an obligation to stay complete in both languages, and
those later slices inherit both the key *names* and the translation upkeep.
This is mostly positive (it gives downstream slices a ready vocabulary, and
the SoW Gotcha flags it), but it front-loads naming decisions the ribbon/menu
slices nominally own — if 006 prefers different keys, today's entries become
dead weight that still has to pass the completeness test. Low cost; worth a
conscious nod when 006 lands.

## Risks

The startup completeness check is single-language: `startupCheck
settings.language` (Localization.fs:197-202) validates only the *active*
language. English is the fallback target for every lookup, yet a missing or
emptied English value is not checked at startup unless English is itself
active — so with Russian active and a broken EN entry, `lookup` would silently
fall through to the raw key with no copyable warning. The cross-language
guarantee currently lives only in the `AC-I3 shipped resource is complete`
test, not at runtime. Low severity (the shipped resource is complete and the
test guards regressions), but since English is the universal fallback,
consider having `startupCheck` also report English gaps regardless of the
active language. Separately, `parse` silently maps non-object entries to an
empty language map; that is surfaced for the active language by `check`, so it
degrades to a copyable warning rather than a silent drop — acceptable.

## Bottom line

I would ship this. The slice is well-layered (a clean leaf module, no reversed
import edges), strongly consistent with the `Theme`/env-schema precedents, and
covers all four ACs with the deferred scope correctly held back. The findings
are forward-looking suggestions, not defects: the unit-glyph placement is the
one I would most like the worker to reconsider before the UI slices build on
it, followed by tightening the "add a language by file edit" claim and the
single-language startup check. None blocks the gate; the judge should weigh
these against the green gate results as polish, not as a re-spawn trigger.
