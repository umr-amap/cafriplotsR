# PENDING MIGRATION - written, not yet applied
#
# This file is not part of the package namespace. It is installed under
# inst/migrations/ so that what is done to the database stays readable.
#
# One column, and the values a citation is built from:
#
#   backbone_list.homepage          the site a backbone is cited from (new)
#   backbone_list.publisher         the publisher, spelled as it asks to be
#   backbone_import.source_version  the publisher's own version of the current
#                                   import (APD's "4.0.0"), which the export
#                                   does not carry
#
# APD asks to be cited as:
#
#   African Plant Database (version 4.0.0). Conservatoire et Jardin botaniques
#   de la Ville de Geneve and South African National Biodiversity Institute,
#   Pretoria, accessed <month year>, from <http://africanplantdatabase.ch>.
#
# apd_backbone.R registered APD with the shorter publisher name, and
# import_apd_names() stored the file name in source_version because the export
# carries no version tag. Both are corrected here; later imports pass
# source_version = "4.0.0" (or whatever the site then states) themselves.
#
# Why a column rather than reusing url_template: url_template is the address of
# one name (it holds an {id} placeholder) and is meant for linking a taxon.
# What a citation needs is the site itself. Taking the host out of a per-name
# URL would tie the citation to a link format nobody has settled yet.
#
# The month and year are not stored: backbone_citation() takes them from the
# version of the current import, so the citation states when the names in the
# database were obtained rather than when they are cited.
#
# To run (taxa database):
#   source(system.file("migrations", "backbone_citation_metadata.R", package = "CafriplotsR"))
#   con_taxa <- CafriplotsR::call.mydb.taxa()
#   migrate_backbone_citation_metadata(con_taxa)                   # rehearsal
#   migrate_backbone_citation_metadata(con_taxa, dry_run = FALSE)  # apply


# Accented characters are built with intToUtf8() to keep this file ASCII
.citation_publisher_apd <- function() {
  paste0("Conservatoire et Jardin botaniques de la Ville de Gen",
         intToUtf8(232),
         "ve and South African National Biodiversity Institute, Pretoria")
}


#' Add backbone_list.homepage and write the citation metadata
#'
#' @param con_taxa Connection or pool to the taxa database, with write access.
#' @param apd_source_version APD's release version to record for the current
#'   import. Default "4.0.0".
#' @param dry_run If TRUE (default), print what would change and write nothing.
migrate_backbone_citation_metadata <- function(con_taxa,
                                               apd_source_version = "4.0.0",
                                               dry_run = TRUE) {

  cli::cli_h1("Citation metadata of the taxonomic backbones")
  con <- if (inherits(con_taxa, "Pool")) pool::poolCheckout(con_taxa) else con_taxa
  on.exit(if (inherits(con_taxa, "Pool")) pool::poolReturn(con), add = TRUE)
  if (!DBI::dbIsValid(con)) cli::cli_abort("Invalid database connection")

  if (is.na(DBI::dbGetQuery(con, "SELECT to_regclass('public.backbone_list')::text AS r")$r)) {
    cli::cli_abort(c("{.field backbone_list} does not exist.",
                     "i" = "Apply {.file inst/migrations/multi_backbone.R} first."))
  }

  has_homepage <- nrow(DBI::dbGetQuery(con, "
    SELECT 1 FROM information_schema.columns
     WHERE table_schema = 'public' AND table_name = 'backbone_list'
       AND column_name = 'homepage'")) > 0

  wanted <- data.frame(
    code      = c("apd", "wcvp"),
    publisher = c(.citation_publisher_apd(), "Royal Botanic Gardens, Kew"),
    homepage  = c("http://africanplantdatabase.ch",
                  "https://powo.science.kew.org"),
    stringsAsFactors = FALSE
  )

  before <- DBI::dbGetQuery(con, sprintf("
    SELECT b.code, b.publisher, %s AS homepage, i.version, i.source_version
      FROM backbone_list b
      LEFT JOIN backbone_import i ON i.id_backbone = b.id_backbone AND i.is_current
     ORDER BY b.code", if (has_homepage) "b.homepage" else "NULL::text"))
  cli::cli_h2("Now")
  cli::cli_alert_info("{.field backbone_list.homepage}: {if (has_homepage) 'present' else 'to be added'}")
  print(before)

  present <- wanted[wanted$code %in% before$code, , drop = FALSE]
  absent  <- setdiff(wanted$code, before$code)
  if (length(absent) > 0) {
    cli::cli_alert_info("Not registered, skipped: {.val {absent}}")
  }

  after <- before
  for (i in seq_len(nrow(present))) {
    j <- match(present$code[i], after$code)
    after$publisher[j] <- present$publisher[i]
    after$homepage[j]  <- present$homepage[i]
  }
  j <- match("apd", after$code)
  if (!is.na(j)) after$source_version[j] <- apd_source_version
  cli::cli_h2("After")
  print(after)

  if (dry_run) {
    cli::cli_alert_info("Dry run - nothing was changed. Re-run with {.code dry_run = FALSE}.")
    return(invisible(after))
  }

  DBI::dbBegin(con)
  n_import <- tryCatch({
    if (!has_homepage) {
      DBI::dbExecute(con, "ALTER TABLE backbone_list ADD COLUMN homepage text")
    }
    for (i in seq_len(nrow(present))) {
      DBI::dbExecute(
        con,
        "UPDATE backbone_list SET publisher = $1, homepage = $2 WHERE code = $3",
        params = list(present$publisher[i], present$homepage[i], present$code[i])
      )
    }
    n <- DBI::dbExecute(
      con,
      "UPDATE backbone_import i SET source_version = $1
         FROM backbone_list b
        WHERE b.id_backbone = i.id_backbone AND b.code = 'apd' AND i.is_current",
      params = list(apd_source_version)
    )
    DBI::dbCommit(con)
    n
  }, error = function(e) {
    DBI::dbRollback(con)
    cli::cli_abort("Nothing was changed: {conditionMessage(e)}")
  })

  cli::cli_alert_success(
    "Citation metadata written ({nrow(present)} backbone{?s}, {n_import} import row{?s})."
  )
  cli::cli_alert_info("Check with {.code backbone_citation(\"apd\", con_taxa)}.")
  invisible(after)
}
