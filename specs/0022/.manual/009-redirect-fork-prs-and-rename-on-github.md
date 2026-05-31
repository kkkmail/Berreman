# Make PRs target your own repo + rename it to `Avalonia.FuncUI.Clone` on GitHub

## Your current state (verified from `C:\GitHub\Avalonia.FuncUI.Clone`)

- Local repo: `C:\GitHub\Avalonia.FuncUI.Clone`
- Two remotes, **HTTPS**:
  - `origin   → https://github.com/kkkmail/Avalonia.FuncUI.git`  (your fork)
  - `upstream → https://github.com/fsprojects/Avalonia.FuncUI.git`  (the original)
- Current branch: **`kk-master`** (local branches: `kk-master`, `master`; HEAD `f6a5c64`).
- `origin` is a **full fork**: it carries all the upstream branches (master, nightly,
  many `JaggerJo-patch-*`, experiment branches, plus your `kk-master`) and **27 tags**.
- `kkkmail/Avalonia.FuncUI` on GitHub is a **fork** of `fsprojects/Avalonia.FuncUI`.
- **`gh` CLI is NOT installed** (`gh: command not found`). The instructions below default
  to **Git + the GitHub web UI**; optional `gh` variants are marked "(if you install gh)".

## Why PRs go to the original repo

The key point: **because your GitHub repo is a *fork*, the "New pull request" page
defaults the *base* repository to the parent (`fsprojects/Avalonia.FuncUI`).** That is a
property of the **fork relationship**, not of the repo name.

> **Renaming the repo will NOT change this.** A renamed fork is still a fork, so PRs will
> still default to upstream. To make PRs target *your* repo you must remove the fork
> relationship (detach), or recreate the repo as a normal (non-fork) repository.

Two real paths. **Option A is recommended** — fully self-service, no GitHub Support
ticket, gives the correct name in one step, and matches your intent (an independent
"Clone", not a fork that tracks upstream).

---

## Option A (recommended): recreate as a standalone, non-fork repo

Net effect: a brand-new repo `kkkmail/Avalonia.FuncUI.Clone` that is **not** a fork, so
PRs default to itself; full history/branches/tags preserved; the old fork deleted.

### A1. Create the new EMPTY repo (do NOT use the "Fork" button)

- **Web UI:** https://github.com/new → Owner `kkkmail`, Repository name
  **`Avalonia.FuncUI.Clone`**, Private or Public as you wish, **do not** add README /
  .gitignore / license (it must be empty so the mirror push isn't rejected) → **Create
  repository**. Copy the new URL: `https://github.com/kkkmail/Avalonia.FuncUI.Clone.git`.
- **or (if you install gh):**
  ```powershell
  gh repo create kkkmail/Avalonia.FuncUI.Clone --private --disable-wiki
  ```
  (use `--public` if you want it public).

A freshly created repo is **not** marked a fork — that is the whole point.

### A2. Push your full history to the new repo

You have many branches and 27 tags, so do a **mirror push** to copy everything exactly.
From `C:\GitHub\Avalonia.FuncUI.Clone`:

```powershell
# Add the NEW repo as a separate remote (keep origin/upstream as-is for now)
git remote add clone https://github.com/kkkmail/Avalonia.FuncUI.Clone.git

# Mirror EVERYTHING (all branches + all 27 tags) into the brand-new empty repo
git push clone --mirror
```

> `--mirror` makes the new remote match your local refs exactly. It is safe **only**
> against the brand-new empty repo (it can delete refs that aren't present locally — fine
> here since the target is empty). Your local `origin`/`upstream` are untouched.
>
> Note: `--mirror` pushes the refs you have locally. Branches you only have as
> `remotes/origin/*` (not checked out) are remote-tracking, not local; if you want *those*
> too, first materialize them, e.g.:
> ```powershell
> # optional: create local branches for every origin branch, then re-mirror
> git fetch origin
> foreach ($b in (git branch -r | Select-String '^\s*origin/' | %{ ($_ -replace '.*origin/','').Trim() } | Where-Object { $_ -notmatch 'HEAD' })) {
>     git branch --track $b "origin/$b" 2>$null
> }
> git push clone --mirror
> ```
> If you only care about `kk-master` (+ `master`) and the tags, skip that and the plain
> `git push clone --mirror` above is enough.

### A3. Make the new repo your `origin`

```powershell
# Repoint origin to the new repo and drop the temporary 'clone' remote
git remote set-url origin https://github.com/kkkmail/Avalonia.FuncUI.Clone.git
git remote remove clone

# Set upstream tracking for the branch you work on
git push -u origin kk-master

git remote -v
# origin    https://github.com/kkkmail/Avalonia.FuncUI.Clone.git (fetch/push)
# upstream  https://github.com/fsprojects/Avalonia.FuncUI.git     (fetch/push)
```

Keep `upstream` → `fsprojects/Avalonia.FuncUI` so you can still pull in future upstream
changes with `git fetch upstream` and merge — that does not make your repo a fork again.

### A4. Verify the new repo is NOT a fork

- **Web UI:** open https://github.com/kkkmail/Avalonia.FuncUI.Clone — there should be
  **no** "forked from fsprojects/Avalonia.FuncUI" line under the title.
- **or (if you install gh):**
  ```powershell
  gh repo view kkkmail/Avalonia.FuncUI.Clone --json name,isFork,parent
  # Expect: "isFork": false, "parent": null
  ```

### A5. Confirm PRs now default to your repo

```powershell
git switch -c test/pr-target
git commit --allow-empty -m "test: confirm PR base is my own repo"
git push -u origin test/pr-target
```
- **Web UI:** the "Compare & pull request" banner now shows
  `base: kkkmail/Avalonia.FuncUI.Clone ← compare: test/pr-target` with no upstream option
  forced. Pick base branch `kk-master` (or whatever you make default — see A6).
- **or (if you install gh):**
  ```powershell
  gh pr create --repo kkkmail/Avalonia.FuncUI.Clone --base kk-master --head test/pr-target --fill
  ```
Clean up:
```powershell
git push origin --delete test/pr-target
git switch kk-master
git branch -D test/pr-target
```

### A6. (Optional) set the default branch

If you want `kk-master` to be the default branch (so PRs and the repo home default to it):
- **Web UI:** repo → Settings → General → Default branch → switch to `kk-master`.

### A7. Delete the old fork (optional but recommended)

Once the new repo is confirmed good:
- **Web UI:** `kkkmail/Avalonia.FuncUI` → Settings → Danger Zone → **Delete this
  repository**.
- **or (if you install gh, needs `delete_repo` scope):**
  ```powershell
  gh repo delete kkkmail/Avalonia.FuncUI --yes
  ```

Done — `kkkmail/Avalonia.FuncUI.Clone` is now a standalone repo whose PRs target itself.

---

## Option B: keep the existing fork — rename it, then detach it

Use only if you must preserve the existing GitHub repo object (its stars, issues,
existing PRs). Requires **both** a rename **and** a detach; rename alone does not fix the
PR target.

### B1. Rename on GitHub

- **Web UI:** `kkkmail/Avalonia.FuncUI` → Settings → General → **Repository name** →
  `Avalonia.FuncUI.Clone` → **Rename**.
- **or (if you install gh):**
  ```powershell
  gh repo rename Avalonia.FuncUI.Clone --repo kkkmail/Avalonia.FuncUI
  ```
GitHub keeps a redirect from the old URL, but update your local remote anyway:
```powershell
git remote set-url origin https://github.com/kkkmail/Avalonia.FuncUI.Clone.git
git remote -v
```

### B2. Detach the fork (this is what actually redirects PRs)

GitHub has **no reliable self-service button** to detach a fork:
1. **GitHub Support** — https://support.github.com/contact → ask them to
   *"detach `kkkmail/Avalonia.FuncUI.Clone` from its upstream fork network."* After they
   do, PRs default to your own repo. Typically a day or two.
2. If that is unacceptable, there is no in-place fix — fall back to **Option A**, which
   severs the relationship immediately and is self-service.

### B3. Interim workaround until detached — pick the base repo per PR

While the repo is still a fork you can still target your own repo on each PR manually:
- **Web UI:** on the "Open a pull request" page use the **base repository** dropdown and
  select `kkkmail/Avalonia.FuncUI.Clone` instead of the upstream default. The comparison
  becomes `base: kkkmail/… ← compare: <your branch>`.
- **or (if you install gh):** always pass `--repo` so it can't drift to upstream:
  ```powershell
  gh pr create --repo kkkmail/Avalonia.FuncUI.Clone --base kk-master --head <your-branch> --fill
  ```

---

## Which to choose

| Goal | Option A (recreate) | Option B (rename + detach) |
|---|---|---|
| PRs default to your repo | ✅ immediately | ✅ only after Support detaches |
| Self-service (no ticket) | ✅ | ❌ (needs GitHub Support) |
| Correct name `Avalonia.FuncUI.Clone` | ✅ (created with it) | ✅ (rename step) |
| Preserves stars/issues/existing PRs on the repo object | ❌ (new repo) | ✅ |
| Removes "forked from …" banner | ✅ | ✅ after detach |
| Effort | Low, ~5 min | Low rename + wait on Support |

**Recommendation:** for a private "Clone" you maintain for vendoring/auditing, you don't
care about stars/issues on the GitHub object, so **Option A** is the clean, immediate
choice. Use Option B only if you must keep the existing repo's issues/PR history.

---

## Notes / gotchas

- **`gh` is not installed.** Either follow the Web-UI steps (no CLI needed), or install it
  first: `winget install --id GitHub.cli`, then `gh auth login` (run interactively — in
  this session you can type `! gh auth login`). Every step above has a Web-UI equivalent,
  so installing `gh` is optional.
- **Remotes are HTTPS** (`https://github.com/…`), not SSH — all commands above use HTTPS
  to match. (SSH form would be `git@github.com:kkkmail/Avalonia.FuncUI.Clone.git`.)
- **Current branch is `kk-master`, not `master`** — the push/PR commands target
  `kk-master`. Decide which branch should be the repo default (A6).
- **The new repo must be empty** before the mirror push (no auto-created
  README/.gitignore/license), or the push is rejected.
- **Keep `upstream`** pointing at `fsprojects/Avalonia.FuncUI`; pulling from it later
  (`git fetch upstream`) does **not** turn your repo back into a fork.
- **Renaming never breaks local clones** beyond needing `git remote set-url`; GitHub
  redirects the old path, but updating the remote avoids surprises.
