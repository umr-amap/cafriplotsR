# Resolve the public login credential

Reports whether the "Connect as public user" button can be offered, and
with which credential. Anything unexpected — no network, a blocked host,
a malformed descriptor, or public access switched off upstream — comes
back as unavailable. There is deliberately no built-in fallback value; a
credential with a default in the source is a credential that gets
published the first time the file moves.

## Usage

``` r
.public_credential(
  url = getOption("CafriplotsR.public_access_url", .public_credential_url),
  timeout = 5,
  force = FALSE
)
```

## Arguments

- url:

  Descriptor to read. Defaults to the published one, or to
  \`getOption("CafriplotsR.public_access_url")\` when set — which is how
  a test, or a site mirroring the descriptor behind its own firewall,
  points this elsewhere.

- timeout:

  Seconds to wait for it. Kept short: this runs while the user is
  looking at the login screen.

- force:

  Skip the cache and ask again.

## Value

A list with: - \`available\`: logical, whether public login can be
offered - \`user\`, \`password\`: the credential, empty strings when
unavailable - \`message\`: text to show in place of the button, possibly
empty

## Details

Environment variables win over the published descriptor. A served
deployment injects them (see \`deployment/taxonomic_match/\`) and must
not depend on a third-party host being reachable to let anyone in.
