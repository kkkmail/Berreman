# Step 0057 implementation plan

The operator ruling in `.manual/005-step-0056-parity-retirement.md` authorized
this slice's exact test replacement during step 0056. Preserve that completed
implementation and the declared test effects. Read the existing integrity fact,
its project entry, the catalogue reader and the immutable S17-INTEGRITY oracle
to determine whether any authorized source edit remains necessary.

The source scope is limited to the retired parity file, the new integrity file
and `OpticalConstructor.Tests.fsproj`. If the existing implementation already
fulfills the slice, make no redundant source changes. Create this plan, append
the normal-cycle record to `0057-impl-log.md`, and refresh
`0057-state-of-the-world.md` with the required handoff sections.

The main risks are duplicating the earlier test effects, changing the exact fact
identity or counts, and treating historical verification as a current-cycle
measurement. Keep the operator's evidence distinct from this worker's work.
The IMPLEMENT worker protocol reserves all gate execution and checkpoint writes
for the supervisor; leave the ten-gate roster and the inherited census intact.
Do not modify existing supervisor-state changes or any published contract.
