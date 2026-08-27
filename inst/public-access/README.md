# The public-access descriptor

`public-access.json` is what the package reads to decide whether the
"Connect as public user" button can be offered, and with which credential.
The copy in this directory is a **template**. The one that matters is the one
published at:

    https://umr-amap.github.io/cafriplotsR/public-access.json

Nothing in the package installs or reads the copy here. It exists so the
shape of the file is documented and reviewable in the repository, without the
current password ever being committed.

## Why the credential is not in the source

The public account is read-only and opens onto data the project intends to
publish, so the password is not a secret. But it must be **revocable**: the
database runs on OVH Webcloud, where no per-role connection limit can be set
(`inst/docs/PLAN_SECURITY_REMEDIATION.md`, P0.4), so withdrawing the
credential is the only control over a published login exhausting
`max_connections`.

A password compiled into the package cannot be withdrawn — it stays valid in
every installed copy until every user reinstalls. Reading it at runtime turns
rotation into a one-line edit that every installation, however old, picks up
on its next launch.

## Fields

| Field | Meaning |
|---|---|
| `enabled` | `false` hides the button everywhere within `.public_credential_ttl` (5 min). The kill switch. |
| `user`, `password` | The credential. Ignored when `enabled` is `false`. |
| `message` | Shown on the login screen in place of the button. Use it to say *why* — an empty message means the button simply disappears. Free text, not translated. |

Anything unexpected — unreachable host, non-200, malformed JSON, missing
field — is treated as unavailable. There is no fallback value anywhere in the
package.

## Publishing it

The pkgdown workflow deploys `docs/` to `gh-pages` with `clean: false`
(`.github/workflows/pkgdown.yaml`), so a file committed straight to that
branch survives later site rebuilds. That is the whole procedure:

```bash
git fetch origin gh-pages
git worktree add /tmp/ghp gh-pages
cp inst/public-access/public-access.json /tmp/ghp/public-access.json
# edit /tmp/ghp/public-access.json - put the real password in
cd /tmp/ghp
git add public-access.json
git commit -m "chore(public-access): rotate public credential"
git push origin gh-pages
cd - && git worktree remove /tmp/ghp
```

Live in about 30 seconds. Verify with:

```r
CafriplotsR:::.public_credential(force = TRUE)
```

## Rotating

1. OVH panel -> Users -> `CafriP_public` -> regenerate the password.
2. Publish the new value as above.

Both hosted and local installations follow immediately. Nothing to release,
nobody to notify. Do **not** put the new value in this repository.

## Withdrawing under abuse

Set `"enabled": false`, give `message` a sentence saying when it will be back,
and push. Public login disappears everywhere within five minutes; users with
their own accounts are unaffected throughout. Rotate afterwards, at leisure.

## The served deployment does not use this file

`CAFRI_PUBLIC_USER` and `CAFRI_PUBLIC_PASS` take precedence over the
descriptor, and the SSP Cloud deployment sets them
(`deployment/taxonomic_match/README.md`). A hosted app must not depend on
GitHub Pages being reachable in order to let anyone in — and the credential
there never leaves the server.
