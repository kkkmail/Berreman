# Implementation plan

Implement S32-DROP-ORIGIN in the two authorized source files. Add the named regression fact first, then extend `ImportOrigin` with a dropped entry file and an honest `describe` renderer. Collision labels will use that renderer; the existing catalogue-only latest-move match will remain unchanged. Document that dropped origins contain the file's leaf name and keep the held latest version.

Move the existing per-envelope read/decision body from `decideCatalogue` to `CatalogueImportProxy.decideEntry`. Keep catalogue row order, error propagation and unlisted rows intact, with no additional store access or writes. Preserve the existing catalogue assertions and add exactly one fact covering both dropped-file decisions and origin rendering.

Run the resolved Release build, elevation, constructor suite and eleven s13 invariants as local verification required by the session instructions; supervisor acceptance and checkpoints remain external. Capture persistent evidence only under the supplied `.artifacts` folder and write the required state-of-the-world at the supplied path. Do not change versions, gate definitions, baselines, external repositories or the existing supervisor state edit.

The principal risks are a non-exhaustive origin match and changing catalogue read order during extraction. Use `FileName` from the existing shared primitives and preserve the original decision body verbatim apart from the parameter name.
