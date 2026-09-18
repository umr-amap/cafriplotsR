# Tropicos API key
#
# A Tropicos API key is a personal credential: each user requests their own at
# <https://services.tropicos.org/help?requestkey>. The package therefore ships
# no key at all, and resolves one exactly the way it resolves database
# credentials - explicit argument, then the session cache, then `.Renviron`,
# then an interactive prompt. The session cache is the same `credentials`
# environment used for the database user and password, so `cleanup_connections()`
# clears the key along with everything else.

#' Store a Tropicos API key in `.Renviron`
#'
#' @description
#' Saves a Tropicos API key as `TROPICOS_API_KEY` in `~/.Renviron` so that it is
#' available in every future R session, and caches it for the current one. The
#' key is the personal credential obtained from
#' <https://services.tropicos.org/help?requestkey>; the package does not ship
#' one.
#'
#' WARNING: the key is stored in plain text. Only use this on a personal,
#' secure computer.
#'
#' @param key Character. The Tropicos API key. If `NULL` (the default), it is
#'   asked for interactively.
#'
#' @return `TRUE` invisibly if the key was written, `FALSE` otherwise.
#'
#' @seealso [get_tropicos_key()], [remove_tropicos_key()]
#'
#' @examples
#' \dontrun{
#' setup_tropicos_key("your-tropicos-api-key")
#' }
#' @export
setup_tropicos_key <- function(key = NULL) {

  cli::cli_alert_warning("WARNING: the key will be stored in plain text in ~/.Renviron")
  cli::cli_alert_warning("Only proceed if this is your personal, secure computer")

  if (is.null(key)) {
    key <- get_password_secure("Enter your Tropicos API key: ")
  }

  key <- trimws(as.character(key)[1])

  if (is.na(key) || !nzchar(key)) {
    cli::cli_alert_info("No key given, nothing saved")
    return(invisible(FALSE))
  }

  renviron_path <- file.path(path.expand("~"), ".Renviron")

  if (file.exists(renviron_path)) {
    existing_lines <- readLines(renviron_path)
    existing_lines <- existing_lines[!grepl("^TROPICOS_API_KEY=", existing_lines)]
  } else {
    existing_lines <- character(0)
  }

  writeLines(c(existing_lines, paste0("TROPICOS_API_KEY=", key)), renviron_path)

  # Reload immediately so the key is available without restarting R
  readRenviron(renviron_path)
  credentials$tropicos_key <- key

  cli::cli_alert_success("Tropicos API key saved to ~/.Renviron")
  cli::cli_alert_info("The key is active in this session (no restart needed)")
  cli::cli_alert_info("To remove it later, use: remove_tropicos_key()")

  invisible(TRUE)
}


#' Remove the stored Tropicos API key
#'
#' @description
#' Deletes `TROPICOS_API_KEY` from `~/.Renviron` and drops it from the session
#' cache.
#'
#' @return `TRUE` invisibly if a key was removed, `FALSE` otherwise.
#'
#' @seealso [setup_tropicos_key()]
#'
#' @examples
#' \dontrun{
#' remove_tropicos_key()
#' }
#' @export
remove_tropicos_key <- function() {

  # Forget the key for this session whatever happens to the file
  if (exists("tropicos_key", envir = credentials, inherits = FALSE)) {
    rm("tropicos_key", envir = credentials, inherits = FALSE)
  }
  Sys.unsetenv("TROPICOS_API_KEY")

  renviron_path <- file.path(path.expand("~"), ".Renviron")

  if (!file.exists(renviron_path)) {
    cli::cli_alert_info("No .Renviron file found")
    return(invisible(FALSE))
  }

  existing_lines <- readLines(renviron_path)

  if (!any(grepl("^TROPICOS_API_KEY=", existing_lines))) {
    cli::cli_alert_info("No stored Tropicos API key found")
    return(invisible(FALSE))
  }

  writeLines(existing_lines[!grepl("^TROPICOS_API_KEY=", existing_lines)],
             renviron_path)

  cli::cli_alert_success("Tropicos API key removed from ~/.Renviron")

  invisible(TRUE)
}


#' Get the Tropicos API key
#'
#' @description
#' Resolves the personal Tropicos API key used to query Tropicos through
#' \pkg{taxize}, in this order: the `key` argument, the key cached earlier in
#' this session, the `TROPICOS_API_KEY` environment variable (typically set by
#' [setup_tropicos_key()] or by `.Renviron`), and finally an interactive
#' prompt. A key found anywhere is cached for the rest of the session.
#'
#' The package ships no key: request one at
#' <https://services.tropicos.org/help?requestkey>.
#'
#' @param key Character. A key to use and cache. `NULL` (the default) to
#'   resolve one from the cache, the environment, or the user.
#' @param prompt Logical. Ask for the key if none was found. Defaults to
#'   [interactive()]; pass `FALSE` where a prompt would block, such as inside a
#'   Shiny app.
#'
#' @return The key as a single string, or `NULL` if none is available.
#'
#' @seealso [setup_tropicos_key()] to store the key permanently.
#'
#' @examples
#' \dontrun{
#' key <- get_tropicos_key()
#' taxize::tp_search(sci = "Dacryodes edulis", key = key)
#' }
#' @export
get_tropicos_key <- function(key = NULL, prompt = interactive()) {

  clean <- function(x) {
    if (is.null(x) || length(x) != 1L) return(NULL)
    x <- trimws(as.character(x))
    if (is.na(x) || !nzchar(x)) NULL else x
  }

  key <- clean(key)
  if (!is.null(key)) {
    credentials$tropicos_key <- key
    return(key)
  }

  cached <- clean(credentials$tropicos_key)
  if (!is.null(cached)) return(cached)

  from_env <- clean(Sys.getenv("TROPICOS_API_KEY", ""))
  if (!is.null(from_env)) {
    credentials$tropicos_key <- from_env
    return(from_env)
  }

  if (!isTRUE(prompt)) return(NULL)

  cli::cli_alert_info(
    "No Tropicos API key found. Request one at {.url https://services.tropicos.org/help?requestkey}"
  )
  cli::cli_alert_info("Use {.fn setup_tropicos_key} to store it for future sessions")

  entered <- clean(get_password_secure("Enter your Tropicos API key: "))
  if (is.null(entered)) return(NULL)

  credentials$tropicos_key <- entered
  entered
}
