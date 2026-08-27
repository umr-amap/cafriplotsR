# Resolution of the read-only public login used by the Shiny apps.
#
# The credential is deliberately not a secret: it opens a read-only account
# onto data the project intends to publish. What it must be is *revocable*.
# A value frozen into this file at build time cannot be changed without a
# release, so in practice it never changes, and it stays valid for as long as
# any installed copy of the package survives. That matters here because the
# database runs on OVH Webcloud, where no per-role connection limit can be
# set (see inst/docs/PLAN_SECURITY_REMEDIATION.md, P0.4): withdrawing the
# credential is the only control there is over a published login exhausting
# `max_connections`.
#
# So the package ships a URL rather than a value. Rotating means editing one
# file on the gh-pages branch; withdrawing means setting `enabled` to false
# there. Every installation, however old, follows on its next launch.

# Where the published descriptor lives. The pkgdown site is deployed from
# docs/ to gh-pages with `clean: false` (.github/workflows/pkgdown.yaml), so a
# file committed directly to that branch survives later site rebuilds.
.public_credential_url <-
  "https://umr-amap.github.io/cafriplotsR/public-access.json"

# Resolution is cached for this long. Both the login UI and the login server
# ask, and a long-lived process should still notice a withdrawal without being
# restarted.
.public_credential_ttl <- 300

.public_credential_cache <- new.env(parent = emptyenv())

#' Resolve the public login credential
#'
#' Reports whether the "Connect as public user" button can be offered, and
#' with which credential. Anything unexpected — no network, a blocked host, a
#' malformed descriptor, or public access switched off upstream — comes back
#' as unavailable. There is deliberately no built-in fallback value; a
#' credential with a default in the source is a credential that gets published
#' the first time the file moves.
#'
#' Environment variables win over the published descriptor. A served
#' deployment injects them (see `deployment/taxonomic_match/`) and must not
#' depend on a third-party host being reachable to let anyone in.
#'
#' @param url Descriptor to read. Defaults to the published one, or to
#'   `getOption("CafriplotsR.public_access_url")` when set — which is how a
#'   test, or a site mirroring the descriptor behind its own firewall, points
#'   this elsewhere.
#' @param timeout Seconds to wait for it. Kept short: this runs while the user
#'   is looking at the login screen.
#' @param force Skip the cache and ask again.
#'
#' @return A list with:
#'   - `available`: logical, whether public login can be offered
#'   - `user`, `password`: the credential, empty strings when unavailable
#'   - `message`: text to show in place of the button, possibly empty
#'
#' @keywords internal
.public_credential <- function(url = getOption("CafriplotsR.public_access_url",
                                               .public_credential_url),
                               timeout = 5,
                               force = FALSE) {

  env_user <- Sys.getenv("CAFRI_PUBLIC_USER", "")
  env_pass <- Sys.getenv("CAFRI_PUBLIC_PASS", "")
  if (nzchar(env_user) && nzchar(env_pass)) {
    return(.public_credential_result(TRUE, env_user, env_pass))
  }

  cached <- .public_credential_cache$value
  if (!isTRUE(force) && !is.null(cached) &&
      difftime(Sys.time(), cached$at, units = "secs") < .public_credential_ttl) {
    return(cached$result)
  }

  result <- .public_credential_from(.fetch_public_descriptor(url, timeout))
  .public_credential_cache$value <- list(result = result, at = Sys.time())
  result
}

#' Forget a cached resolution
#'
#' @keywords internal
#' @noRd
.public_credential_forget <- function() {
  .public_credential_cache$value <- NULL
  invisible(NULL)
}

#' Shape a resolution result
#' @keywords internal
#' @noRd
.public_credential_result <- function(available, user = "", password = "",
                                      message = "") {
  list(available = isTRUE(available), user = user, password = password,
       message = message)
}

#' Turn a parsed descriptor into a resolution result
#'
#' A disabled or unusable descriptor still carries its `message`, which is
#' what the login screen shows in place of the button.
#'
#' @keywords internal
#' @noRd
.public_credential_from <- function(descriptor) {
  if (is.null(descriptor)) return(.public_credential_result(FALSE))

  chr <- function(field) {
    value <- as.character(descriptor[[field]] %||% "")[1]
    if (is.na(value)) "" else value
  }

  msg <- chr("message")
  if (!isTRUE(descriptor$enabled)) return(.public_credential_result(FALSE, message = msg))

  user <- chr("user")
  password <- chr("password")
  if (!nzchar(user) || !nzchar(password)) {
    return(.public_credential_result(FALSE, message = msg))
  }

  .public_credential_result(TRUE, user, password, msg)
}

#' Read the published public-access descriptor
#'
#' @return The parsed descriptor, or `NULL` if it could not be read.
#' @keywords internal
#' @noRd
.fetch_public_descriptor <- function(url, timeout = 5) {
  tryCatch({
    # GitHub Pages sits behind a CDN that caches for minutes. A withdrawal
    # that takes ten minutes to reach anyone is not a kill switch, so the
    # request is made unique.
    bust <- paste0(url, if (grepl("?", url, fixed = TRUE)) "&" else "?",
                   "t=", as.integer(Sys.time()))
    handle <- curl::new_handle(timeout = timeout, connecttimeout = timeout)
    response <- curl::curl_fetch_memory(bust, handle = handle)
    # 0 is what a scheme with no status line reports — `file://`, which is how
    # a site behind a firewall points `CafriplotsR.public_access_url` at a
    # local mirror of the descriptor.
    status <- as.integer(response$status_code)
    if (!status %in% c(200L, 0L)) return(NULL)
    jsonlite::fromJSON(rawToChar(response$content), simplifyVector = TRUE)
  }, error = function(e) {
    message("Public access descriptor unavailable (", conditionMessage(e), ").")
    NULL
  })
}
