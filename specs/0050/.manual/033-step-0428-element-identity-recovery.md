# Step 0428: reproducible reference-kit element identities

2026-11-15. Post-mortem `.slices/0428-01.postmortem-md` and review
`.slices/0428-01.postmortem-review-md`: the fact `the built optics-mueller kit equals
the committed kit file by file` cannot pass, because every added table slot takes its
identity from `freshId` (`TableAndElementRotationView.fs`), which calls
`Guid.NewGuid` directly. AT-1 adds three slots through `AddElement`; their identities
change on every build and differ in 95 reference-kit files. The fixture already
injects item and kit identities and the clock; slot identities are the one ambient
identity left, and 0428's effects exclude the file that mints them.

## Binding ruling

At 0428, authorize editing `Berreman/OpticalConstructor/OpticalConstructor.Ui/TableAndElementRotationView.fs`
to add an element-identity provider, beside the already-authorized
`Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/KitAcceptanceTests.fs`.

The rule: `ElementIdProvider`, a record of one function minting a `Library.ElementId`,
declared in `TableAndElementRotationView.fs` with a `system` member that mints a fresh
GUID, is the only place a table slot's identity is minted; `freshId` is deleted. The
table model holds it as `elementIds`, defaulted to `ElementIdProvider.system`, and
`addElement` reads it, so `AddElement` and `AddElementBoundTo` keep their behavior in
the app. Each AT-1 build supplies three fixed, distinct identities through that field
before sending its unchanged messages, fails on a fourth request and asserts all three
were consumed. The reference kit is regenerated from a build with this repair, its
root and `library/` files copied byte for byte (`data/` untouched, `.oc/` excluded),
and the fact must then pass on a second, independent build. Exact-file comparison,
every assertion, every test identity, R0001 and R0002 are preserved.

Verify before commit: the 26 gates of `.slices/0428.gates` as declared. Resume normally
with executability profile 1 and the operator reply, without `--accept-current-slice`,
so normal review and the frozen roster publish measured checkpoint 427; this overrides
the general committed-green acceptance rule for this recovery only. At 0432, AT-2's
added reflected detector takes its identity from the same provider; preserve that
injection and AT-1's through 0494. The additional production authority is confined
to 0428. No gate, floor, deadline, census, durable format, test effect or
published-spec amendment is authorized.
