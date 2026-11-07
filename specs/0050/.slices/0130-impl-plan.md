# Implementation plan

Implement S41-CHANGE-CHECK only in KitFolders.fs, KitFolderStore.fs and KitProxyTests.fs. Add recordStamps and changedKitFiles to KitProxy, keeping each opener's record inside its closure. Reuse KitFileKind.all for root-file selection, ContentHash.ofBytes for filesystem bytes, and the existing atomic writer. The in-memory record compares metadata by KitFile equality and other files by their stored text. No durable shape or file format changes.

Add the two prescribed KitProxyContract facts, discovered under both concrete backends (four new identities). Cover the exact A/B writer sequence, repeated recording, fresh proxies, all file kinds, unchanged content, own updates/deletes, ordinal ordering and ignored lab/cache/temporary files. Preserve existing assertions and test identities.

Risks are acknowledging unrelated external edits during an own write, recording partially read filesystem state after an IO failure, and filename case differences on Windows. Update only the successfully written/deleted entry, replace the whole record only after a complete read, and retain case-insensitive filesystem identity while returning actual names in ordinal order. Record progress and decisions in the prescribed log and state-of-world files; the supervisor remains the gate authority.
