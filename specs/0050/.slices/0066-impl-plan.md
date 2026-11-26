# Step 0066 implementation plan

Extract the existing material and category store facts into abstract contract
classes in their current files. Keep the concrete class names and every fact
name, retain the assertions except the explicitly authorized sample-survival
assertion, and leave the proxy stubs and pure structure facts on the concrete
in-memory classes. Move shared fixture values into private modules without
changing their values.

MaterialProxyContract will supply freshProxy, composedProxies and the existing
pin helper. ReferencingSamples will expose active sample ids and a removal that
fails the fact on error. CategoryProxyContract will supply the category/material
pair; both concrete adapters will reuse BuiltinCatalogue and the existing live
reference lookups. No project, product, schema or package changes are needed.

The main risks are inherited xUnit discovery changing test identities/counts,
and accidentally changing assertions while moving the facts. Review the move
against the original files and verify the affected test project locally as
required by the development environment. Acceptance gates remain exclusively
with the arc-runner; no worker acceptance result or new baseline is asserted.
