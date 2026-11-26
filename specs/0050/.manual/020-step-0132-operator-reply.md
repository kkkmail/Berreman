# Approved 0132 recovery: normal supervisor verification

The operator approved `.manual/009-step-0132-lock-guard-recovery.md` to resolve
the authority conflict in `0132-01.postmortem-md`. Read that ruling and the
project-prompt pointer before reviewing or changing this slice. Preserve the
verified attachment implementation and guard repair. This note is prepared for
use only after the operator commits the repair and instructs a relaunch.

Review S42-ATTACH-SHAPES and S41-KIT-LOCK together within the approved scope.
The shared guard handles native 5/32/33 with at most 101 opening attempts and
100 ten-millisecond waits; permanent failures remain typed and failed release
retains ownership. All existing assertion lines and fact identities are intact.
Evidence is in `.artifacts/0132-operator-repair/final-review.md`.

Normal resume must retain `--executability-profile 1`. Do not use
`--accept-current-slice`, reset state/counters, edit expected-red records or
fabricate a checkpoint. The previous constructor baseline is 1,072; the operator
repair measured all 1,076 cases passing with exact census and all 14 gates green.
Return the preserved implementation for normal supervisor gate verification so
the supervisor itself publishes checkpoint index 131 and the measured 1,076-case
floor. Continue the arc only after that acceptance succeeds.
