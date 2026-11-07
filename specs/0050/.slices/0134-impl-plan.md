# Implementation plan — 0134

Implement S42-ATTACH only in KitFolders.fs and KitProxyTests.fs. Reuse the
existing KitProxy extension after readOnlyView: attachLabFile selects an
in-kit read or an outside copy and projects KitDataFile.attachment;
attachmentStatus reads the current file and delegates comparison to
DataAttachment.statusAgainst, translating only MissingKitFileErr to absence.

First add the three prescribed KitProxyContract facts, inherited by both
KitProxyTests and KitProxyFolderTests. Cover in-place attachment despite
different source bytes, unchanged paths and stored texts, outside copying and
collision suffixes, and live unchanged/changed/missing results through writable
and read-only proxies. Extend these same facts for typed error propagation and
read-only attachment behavior without adding test identities.

Keep the existing fixture, storage implementations, lock guard, formats,
versions, gates, oracle and census unchanged. The main risks are accidentally
using the supplied bytes for an in-kit file, swallowing a read error, or
writing during status checks. IMPLEMENT invariant 6 assigns all build, test,
analyzer and invariant execution to the supervisor. Write the required log
and state-of-world without claiming gate results or a new measured baseline.
