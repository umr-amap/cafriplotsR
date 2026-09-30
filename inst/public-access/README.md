# The public-access descriptor

`public-access.json` is what the package reads to decide whether the
"Connect as public user" button can be offered, and with which credential.
The copy in this directory is a **template**. The one that matters is
`docs/public-access.json` on master, which the package reads over two routes:

    https://umr-amap.github.io/cafriplotsR/public-access.json
    https://raw.githubusercontent.com/umr-amap/cafriplotsR/master/docs/public-access.json

One file, two hostnames - see "Two routes to one file" below.

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
field — is treated as unavailable. There is no fallback *value* anywhere in
the package: when no route yields a descriptor the button is not offered, and
the login screen says the descriptor could not be checked rather than leaving
a blank space.

## Publishing it

GitHub Pages serves this repository from **`master`, path `/docs`** (check with
`gh api repos/umr-amap/cafriplotsR/pages`) — not from `gh-pages`, which does not
exist here even though `.github/workflows/pkgdown.yaml` deploys to it. So the
served copy is `docs/public-access.json` on master, and the descriptor is in the
public repository. That is a deliberate trade: the value is world-readable
either way, and what matters is that it can be changed and withdrawn in one
commit.

The master copy lives in `pkgdown/assets/`, which `pkgdown::build_site()` copies
verbatim into `docs/`. Editing only `docs/` would work until the next site
rebuild dropped it, so **edit both**:

```bash
# edit pkgdown/assets/public-access.json, then mirror it
cp pkgdown/assets/public-access.json docs/public-access.json
git add pkgdown/assets/public-access.json docs/public-access.json
git commit -m "chore(public-access): rotate public credential"
git push origin master
```

The `raw.githubusercontent.com` route is live on push; Pages follows about a
minute later. Verify with:

```r
CafriplotsR:::.public_credential(force = TRUE)$available   # as an app sees it

# And each route on its own. With a fallback in place, a route that has
# stopped working is otherwise invisible until it is the only one left.
vapply(CafriplotsR:::.public_credential_urls,
       function(u) CafriplotsR:::.public_credential(url = u, force = TRUE)$available,
       logical(1))
```

The template in this directory stays a placeholder. It documents the shape of
the file; it is never the file that is served, and a real value put here is a
value published in a place nothing reads.

## Two routes to one file

`.public_credential_urls` lists the Pages URL first and the
`raw.githubusercontent.com` URL second, and the resolver tries them in order.
They are not two descriptors: both serve `docs/public-access.json` from the
same commit, so a rotation or a withdrawal takes one push and reaches both.
Nothing has to be kept in step, and there is deliberately no procedure here
for writing two files. If a location is ever added that is a **separate**
file, that stops being true and this document needs one.

The second route is there because filtering is hostname-shaped. One site's
network reset the TLS handshake to `*.github.io` while leaving
`raw.githubusercontent.com` alone, so the public button disappeared there
while the descriptor was healthy everywhere else. What that site saw was

    Public access descriptor unavailable (SSL connect error
    [umr-amap.github.io] Recv failure: connection was reset)

in the console, and nothing at all on the login screen. Both hostnames
resolve into GitHub's `185.199.108-111.x` range, so a block by address
defeats both; only the hostname case is covered.

A location that is *read* settles the question, including when it says
`enabled: false`. The resolver falls through only when a location could not
be read at all, so a second route does not weaken the kill switch: a
withdrawal stops everyone who can reach the first location, and everyone who
cannot reads the same withdrawn file from the second.

A site that can reach neither points the package at its own copy, which
replaces the list outright:

```r
options(CafriplotsR.public_access_url = "file:///srv/mirror/public-access.json")
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
