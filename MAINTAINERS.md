# Maintainers guide

## Repository topics (discoverability)

If you don’t use the Settings app, set topics in the UI:

1. GitHub → Repository → Settings → General → Topics
2. Add: `common-lisp, lisp, windows, roswell, sbcl, quicklisp, fiveam, powershell, github-actions, coverage, ci, starter-template, toolkit`

Or via GitHub CLI:

```powershell
gh repo edit OrztirrStudio/lisp-windows-toolkit --add-topic common-lisp --add-topic lisp --add-topic windows --add-topic roswell --add-topic sbcl --add-topic quicklisp --add-topic fiveam --add-topic powershell --add-topic github-actions --add-topic coverage --add-topic ci --add-topic starter-template --add-topic toolkit
```

## Branch protection for main

UI path:

1. Settings → Branches → Branch protection rules → Add rule
2. Branch name pattern: `main`
3. Enable:
   - Require a pull request before merging (1 approval)
   - Dismiss stale approvals when new commits are pushed (optional)
   - Require conversation resolution before merging
   - Require status checks to pass before merging → select the CI check(s)
   - Include administrators

GitHub CLI (replace CHECK_NAME with the exact check):

```powershell
gh api \
  -X PUT \
  -H "Accept: application/vnd.github+json" \
  "/repos/OrztirrStudio/lisp-windows-toolkit/branches/main/protection" \
  -f required_status_checks.strict=true \
  -f required_status_checks.contexts[]="CHECK_NAME" \
  -f enforce_admins=true \
  -f required_pull_request_reviews.required_approving_review_count=1 \
  -f required_conversation_resolution=true
```

Tip: The check name to require is the job name shown in the Checks tab (e.g., `windows-ci`), not the workflow filename.

Current repo configuration expects the `test-windows` job to pass (see `.github/settings.yml`). If you rename that job in `ci.yml`, update `.github/settings.yml` accordingly.
