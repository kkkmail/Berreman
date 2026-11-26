# Implementation plan

Implement S121-SPLIT-GROUP-OVER within the five authorized source files. Reuse
DataSplit's existing group offers, ExperimentName.nextFreeAmong and the current
MatchByTemplate application path. Add only the two declared domain facts and
one declared model fact, and extend the existing error-rendering fact.

First write the regression coverage for unreadable groups, collision-free
experiment names, refusals and persisted keyed attachments. Then add the pure
DataSplit.withGroupOver member and error descriptions, followed by the caption
cases and OfferSplitGroupOver message/update. Keep offering a transient change;
reserve both held experiment names and other offered names before ticking it.

The main risks are accidentally accepting matched/duplicate/invalid keys,
losing another group's state, allocating a colliding experiment name, or writing
before MatchByTemplate. Tests will exercise these within the authorized facts.
No durable format changes or migrations are needed. Preserve the pre-existing
supervisor change to .manifest.state.json.

IMPLEMENT Invariant 6 assigns builds, tests, analyzers and invariants to the
supervisor after worker exit. This worker will write the required handoff files
without claiming measured gate results or changing baseline/checkpoint metadata.
