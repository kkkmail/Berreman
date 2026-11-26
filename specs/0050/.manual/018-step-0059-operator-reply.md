The operator committed the approved recovery and explicitly authorized this relaunch.

Resume slice 0059 normally with executability profile 1. Follow the recovery ruling in specs/0050/.manual/006-step-0059-case-count-accounting.md and the current specs/0050/.slices/0059-state-of-the-world.md handoff. The product retirement already exists in commit a6fb67f; preserve it. Review it against accepted slice 0058 (a402443293089bb84b224b3028285cd2bdfa70d0).

ASG now accounts for executed cases: the four retired methods represent seven cases (one four-row theory and three facts), so checkpoint 57's 801 becomes 794. Recovered checkpoint-57 evidence is in .test-case-counts-json. All 13 unchanged 0059 gates passed during recovery; evidence is in specs/0050/.artifacts/0059-operator-repair/.

Run the normal review, gate and checkpoint cycle. Record the measured 794 at checkpoint index 58 and persist its full-suite case evidence before 0060. Do not accept the slice without that cycle, rewrite checkpoint 57, restore or repeat the retirement, pad tests, waive gates, or modify the published spec, slices, gate contracts or test-effect ledger. Follow the same ruling for the future arithmetic corrections at 0151, 0152 and 0260.
