#' Safely delete individual-specimen links
#'
#' @description
#' **DANGER: This permanently deletes data from the database!**
#'
#' This function safely deletes one or more rows from `data_link_specimens`
#' (the table that records which individual trees are linked to herbarium
#' specimens). Links can be selected by individual ID, specimen ID, or direct
#' link ID.
#'
#' **Individuals and specimens are preserved** — only the link records are
#' removed.
#'
#' **Safety features:**
#' - Dry-run mode to preview what will be deleted
#' - Shows a summary of every link that would be removed
#' - Requires explicit confirmation (unless `force = TRUE`)
#' - Uses database transactions (rollback on error)
#' - Detailed logging of each step
#'
#' @param individual_ids Integer vector. Delete all links involving these
#'   individual ID(s) (`id_n` from `data_individuals`). Default NULL.
#' @param specimen_ids Integer vector. Delete all links involving these
#'   specimen ID(s) (`id_specimen` from `specimens`). Default NULL.
#' @param link_ids Integer vector. Delete specific link record(s) by their
#'   primary key (`id_link_specimens` from `data_link_specimens`). Takes
#'   precedence over the other selectors when provided. Default NULL.
#' @param con Database connection. If NULL, will connect automatically.
#' @param dry_run Logical. If TRUE, shows what would be deleted without
#'   deleting. Default TRUE for safety.
#' @param force Logical. If TRUE, skips confirmation prompts. Default FALSE.
#'   **USE WITH EXTREME CAUTION!**
#' @param verbose Logical. Show detailed progress? Default TRUE.
#'
#' @return List with deletion summary (invisible).
#'
#' @examples
#' \dontrun{
#' con <- call.mydb()
#'
#' # STEP 1: Always do a dry-run first!
#'
#' # Remove all links for a given individual
#' safe_delete_specimen_links(individual_ids = 12345, dry_run = TRUE)
#'
#' # Remove all links pointing to a given specimen
#' safe_delete_specimen_links(specimen_ids = 678, dry_run = TRUE)
#'
#' # Remove specific link records by their PK
#' safe_delete_specimen_links(link_ids = c(1001, 1002), dry_run = TRUE)
#'
#' # STEP 2: Review the output, then delete if sure
#' safe_delete_specimen_links(individual_ids = 12345, dry_run = FALSE)
#' }
#'
#' @export
safe_delete_specimen_links <- function(individual_ids = NULL,
                                       specimen_ids   = NULL,
                                       link_ids       = NULL,
                                       con            = NULL,
                                       dry_run        = TRUE,
                                       force          = FALSE,
                                       verbose        = TRUE) {

  # ===== Input validation =====
  if (is.null(individual_ids) && is.null(specimen_ids) && is.null(link_ids)) {
    stop(
      "Must provide at least one of: individual_ids, specimen_ids, or link_ids",
      call. = FALSE
    )
  }

  if (!is.null(link_ids) && (!is.null(individual_ids) || !is.null(specimen_ids))) {
    cli::cli_alert_warning(
      "link_ids provided alongside individual_ids / specimen_ids — using link_ids only"
    )
    individual_ids <- NULL
    specimen_ids   <- NULL
  }

  if (!is.null(individual_ids) && !is.null(specimen_ids)) {
    cli::cli_alert_warning(
      "Both individual_ids and specimen_ids provided — links matching EITHER will be removed"
    )
  }

  if (is.null(con)) {
    con <- call.mydb()
  }

  if (!test_connection(con)) {
    stop("Invalid database connection", call. = FALSE)
  }

  summary <- list(
    individual_ids = individual_ids,
    specimen_ids   = specimen_ids,
    link_ids       = link_ids,
    dry_run        = dry_run,
    counts         = list(),
    deleted        = list(),
    errors         = list(),
    success        = FALSE
  )

  batch_size <- 5000

  # ===== STEP 1: Find links to delete =====
  if (verbose) cli::cli_h1("Finding Specimen Links to Delete")

  # Build WHERE clause
  where_parts <- c()
  identifier_parts <- c()

  if (!is.null(link_ids)) {
    where_parts      <- sprintf("ls.id_link_specimens IN (%s)", paste(unique(link_ids), collapse = ","))
    identifier_parts <- sprintf("%d direct link ID(s)", length(unique(link_ids)))
  } else {
    if (!is.null(individual_ids)) {
      where_parts      <- c(where_parts, sprintf("ls.id_n IN (%s)", paste(unique(individual_ids), collapse = ",")))
      identifier_parts <- c(identifier_parts, sprintf("%d individual ID(s)", length(unique(individual_ids))))
    }
    if (!is.null(specimen_ids)) {
      where_parts      <- c(where_parts, sprintf("ls.id_specimen IN (%s)", paste(unique(specimen_ids), collapse = ",")))
      identifier_parts <- c(identifier_parts, sprintf("%d specimen ID(s)", length(unique(specimen_ids))))
    }
    where_parts <- paste(where_parts, collapse = " OR ")
  }

  identifier_desc <- paste(identifier_parts, collapse = " and ")

  links_info <- tryCatch({
    DBI::dbGetQuery(con, sprintf("
      SELECT ls.id_link_specimens,
             ls.id_n,
             ls.id_specimen,
             i.tag,
             p.plot_name,
             s.colnbr,
             c.colnam,
             COALESCE(lt.linktype, ls.type) AS linktype
      FROM data_link_specimens ls
      LEFT JOIN data_individuals i   ON ls.id_n        = i.id_n
      LEFT JOIN data_liste_plots p   ON i.id_table_liste_plots_n = p.id_liste_plots
      LEFT JOIN specimens s          ON ls.id_specimen  = s.id_specimen
      LEFT JOIN table_colnam c       ON s.id_colnam    = c.id_table_colnam
      LEFT JOIN linktypelist lt      ON ls.id_linktype = lt.id_linktype
      WHERE %s
      ORDER BY p.plot_name, i.tag, ls.id_specimen
    ", where_parts))
  }, error = function(e) {
    stop("Failed to query specimen links: ", e$message, call. = FALSE)
  })

  if (nrow(links_info) == 0) {
    cli::cli_alert_danger("No specimen links found matching criteria: {identifier_desc}")
    return(invisible(summary))
  }

  summary$counts$links <- nrow(links_info)
  link_ids_to_delete   <- unique(links_info$id_link_specimens)

  if (verbose) {
    cli::cli_h2("Specimen Links to Delete")
    cli::cli_alert_info("Found {nrow(links_info)} link(s) matching: {identifier_desc}")

    n_show <- min(nrow(links_info), 10)
    cli::cli_alert_info("Showing first {n_show} link(s):")
    for (i in seq_len(n_show)) {
      row <- links_info[i, ]
      link_type_label <- if (!is.na(row$linktype) && nchar(row$linktype) > 0) row$linktype else "unknown"
      cli::cli_li(
        "link ID: {row$id_link_specimens} | type: {link_type_label} | individual: {row$id_n} (tag {row$tag}, plot {row$plot_name}) | specimen: {row$id_specimen} ({row$colnam} {row$colnbr})"
      )
    }
    if (nrow(links_info) > 10) {
      cli::cli_alert_info("... and {nrow(links_info) - 10} more")
    }
  }

  # ===== STEP 2: Dry-run exit =====
  if (dry_run) {
    cli::cli_alert_info("This was a DRY-RUN — nothing was deleted")
    cli::cli_alert_info("To actually delete, run with dry_run = FALSE")
    cli::cli_alert_success("Individuals and specimens are preserved (only link records would be removed)")
    return(invisible(summary))
  }

  # ===== STEP 3: Confirmation =====
  if (!force) {
    cli::cli_h2("WARNING: CONFIRMATION REQUIRED")
    cli::cli_alert_danger("You are about to PERMANENTLY delete:")
    cli::cli_ul(sprintf("%d specimen link record(s) from data_link_specimens", nrow(links_info)))
    cli::cli_alert_success("Individuals and specimens will be PRESERVED (only link records are deleted)")

    confirm <- choose_prompt(message = "Are you ABSOLUTELY SURE you want to delete these specimen links?")

    if (!confirm) {
      cli::cli_alert_info("Deletion cancelled by user")
      return(invisible(summary))
    }
  }

  # ===== STEP 4: Delete in batches =====
  cli::cli_h2("Deleting Specimen Links")

  # Handle Pool connections: checkout a raw connection for transaction work
  is_pool <- inherits(con, "Pool")
  if (is_pool) {
    raw_con <- pool::poolCheckout(con)
    on.exit(pool::poolReturn(raw_con), add = TRUE)
  } else {
    raw_con <- con
  }

  if (verbose) cli::cli_alert_info("Deleting {length(link_ids_to_delete)} link(s) in batches...")

  unique_link_ids <- unique(link_ids_to_delete)
  n_batches       <- ceiling(length(unique_link_ids) / batch_size)
  total_deleted   <- 0L

  for (batch_idx in seq_len(n_batches)) {
    start_idx  <- (batch_idx - 1L) * batch_size + 1L
    end_idx    <- min(batch_idx * batch_size, length(unique_link_ids))
    batch_ids  <- unique_link_ids[start_idx:end_idx]

    tryCatch({
      DBI::dbBegin(raw_con)
      rs    <- DBI::dbSendQuery(raw_con, sprintf(
        "DELETE FROM data_link_specimens WHERE id_link_specimens IN (%s)",
        paste(batch_ids, collapse = ",")
      ))
      n_del <- DBI::dbGetRowsAffected(rs)
      DBI::dbClearResult(rs)
      DBI::dbCommit(raw_con)

      total_deleted <- total_deleted + n_del

      if (verbose && n_batches > 1) {
        cli::cli_alert_info("    Batch {batch_idx}/{n_batches}: deleted {n_del} link(s)")
      }
    }, error = function(e) {
      tryCatch(DBI::dbRollback(raw_con), error = function(e2) {})
      cli::cli_alert_danger("Error deleting specimen links (batch {batch_idx}): {e$message}")
      summary$success <- FALSE
      summary$errors  <- c(summary$errors, list(e$message))
    })
  }

  summary$deleted$links <- total_deleted

  # ===== STEP 5: Summary =====
  if (total_deleted == length(unique_link_ids)) {
    summary$success <- TRUE
    if (verbose) {
      cli::cli_h2("Deletion Summary")
      cli::cli_alert_success("Successfully deleted {total_deleted} specimen link(s)")
      cli::cli_alert_success("Individuals and specimens preserved (data_individuals and specimens tables not modified)")
    }
  } else {
    cli::cli_alert_warning(
      "Partial deletion: {total_deleted}/{length(unique_link_ids)} link(s) deleted"
    )
  }

  invisible(summary)
}
