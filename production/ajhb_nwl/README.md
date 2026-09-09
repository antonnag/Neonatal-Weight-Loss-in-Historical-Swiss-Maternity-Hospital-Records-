# AJHB neonatal weight loss — production workspace

This folder is the working production copy for the AJHB revision of **Determinants of Early Neonatal Weight Loss in Historical Swiss Maternity Hospital Records**.

## Authoritative imported code snapshot

- Source repository: `antonnag/Neonatal-Weight-Loss-in-Historical-Swiss-Maternity-Hospital-Records-`
- Source commit: `a9953f104c030b50522094cc3f5455beb37c6373`
- Commit message: `Update analysis code`
- Source commit date: 2026-08-16
- Imported into this dissertation workspace: 2026-09-09

## Working convention

- `R/` contains the production analysis code used for the current revision.
- `manuscript/` will contain the current Word manuscript and, when useful, a text/Markdown mirror for code-vs-manuscript checks.
- `review/` will contain reviewer/editor comments, revision checklist, and response-to-reviewers material.
- Reviewer-specific new analyses should be added transparently (e.g. `R/09_reviewer_revision.R`) rather than silently changing the historical main-analysis logic.

The source repository remains untouched; all current AJHB revision work should be coordinated from this workspace.
