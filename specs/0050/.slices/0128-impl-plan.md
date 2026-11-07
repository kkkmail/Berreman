# Implementation plan

Implement S41-DATA-FILES in the three authorized source files. Extend KitProxy with the elevated readDataFile field, resolve the in-memory folder's existing text map, and read filesystem paths relative to the captured kit folder. Reuse KitDataFile, ContentHash.ofBytes, FileByteContent, the existing BOM-removal member and tryKitFolderIo. Hash the bytes before any text normalization and perform no writes. Move the existing filesystem UTF-8 decoding onto a private FileByteContent member so all folder reads use the same conversion.

Author the prescribed shared contract fact first, producing one identity in each concrete proxy class, then the two prescribed folder-only facts. Within those identities cover nested paths and spaces, root and non-data files, UTF-8 text, empty content, missing files, unchanged paths and bytes, BOM-inclusive hashes, and typed failure while an exclusive handle is held. Preserve all existing assertions and test identities.

Risks are hashing decoded text instead of stored bytes, stripping more than a leading BOM, restricting reads to data/, and allowing filesystem exceptions to escape. The filesystem implementation will catch missing-file/directory exceptions as MissingKitFileErr and route other failures through the existing KitIoErr boundary. No durable format changes, copy-in, attachment, migration or version bump belongs to this slice.

The IMPLEMENT worker is act-only: do not execute builds, tests, analyzers or invariant gates. Write the required implementation log and state-of-the-world at the supplied paths, carrying forward only a supervisor-recorded baseline. The supervisor owns all measured verification and commit readiness.

## Attempt 02 repair

The judge's counterexample is confirmed by source inspection: metadata lives only in kitFileOpt, while readDataFile reads texts. Extend the existing shared lab-file fact first to read .kit-json after initialization and replacement, comparing the existing KitFile.fileText contract and an independently calculated stored-byte hash through proxies opened before the writes and freshly opened proxies. Keep every existing assertion and all four authorized identities.

Move the createInMemory composition extension from Domain to Storage, retaining its folder-only signature. Storage will inject the existing KitFile.fileText serializer with the existing typed error conversion into InMemoryKitFolder.createKitProxy. Domain will store successful metadata serialization in its existing text map alongside the typed metadata; both initialization and replacement use one write operation, and paths will derive metadata presence from that map. This avoids a second serializer and a Domain-to-Storage dependency. Record the factory's new extension-module home as an in-process wiring change; no project references, durable formats, gates or oracle entries change.
