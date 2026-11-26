# Implementation plan

Implement S32-ENTRY-DIFF in the six authorized product/test files. Add the elevated line and diff types to Domain immediately after CatalogueFiles.fs. Strip equal leading and trailing lines before constructing an immutable longest-common-subsequence table for the middle; collect each changed block with removals before additions. Use private elevated counts and offsets for the table and traversal.

Extend LibraryEntryEnvelope in EntryFileJson.fs by rendering both sides through the existing fileText member, splitting on LF and removing only the terminal empty segment. Propagate rendering errors unchanged. No format, version, package, database or UI changes are needed.

Add the three named Domain facts and the named Silicon envelope fact first. Besides the specified examples, cover empty sides, repeated lines, interior matches, grouped changes and long equal ends within those facts. Check recovery of both input lists. The storage fact will compare exact changed lines and the remaining common lines, including the absence of a phantom terminal line and propagation of rendering failures.

Run the resolved Release build, elevation, complete constructor suite and eleven s13 invariants, recording evidence under the supplied artifacts directory. Higher-priority session instructions require local verification despite the worker prompt's act-only wording; supervisor acceptance remains external. Preserve the pre-existing manifest-state edit and all checkpoint files.

Risks: ambiguous LCS ties must be deterministic without interleaving added and removed lines; duplicate lines must survive reconstruction; full-file allocation of a quadratic table would defeat the prefix/suffix optimization; the test count must rise by exactly four.
